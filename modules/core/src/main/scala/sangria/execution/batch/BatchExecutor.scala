package sangria.execution.batch

import sangria.ast
import sangria.ast.AstVisitor
import sangria.execution._
import sangria.execution.deferred.DeferredResolver
import sangria.marshalling.InputUnmarshaller.emptyMapVars
import sangria.marshalling.{
  InputUnmarshaller,
  ResultMarshaller,
  SimpleResultMarshallerForType,
  SymmetricMarshaller
}
import sangria.renderer.SchemaRenderer
import sangria.schema.{
  Argument,
  Directive,
  DirectiveLocation,
  ListInputType,
  OptionInputType,
  Schema,
  StringType,
  Type
}
import sangria.validation.SchemaBasedDocumentAnalyzer.VariableUsage
import sangria.validation.{QueryValidator, TypeInfo, Violation}
import sangria.visitor.VisitorCommand

import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.util.control.Breaks.{break, breakable}
import BatchExecutionPlan._

/** __EXPERIMENTAL__
  *
  * Batch query executor which provides following features:
  *
  * - Allows specifying multiple `operationNames` when executing a GraphQL query document.
  *   All operations would be executed in order inferred from the dependencies between queries.
  * - Support for `@export(as: "foo")` directive. This directive allows you to save the
  *   results of the query execution and then use it as a variable in a different query within the same document.
  *   This provides a way to define data dependencies between queries.
  * - When used with `@export` directive, the variables would be automatically inferred by
  *   the execution engine, so you don't need to declare them explicitly (though you can)
  */
object BatchExecutor {
  val AsArg = Argument("as", StringType, "The variable name.")

  val ExportDirective = Directive(
    "export",
    description = Some("Make the field value available for other operations via variable."),
    arguments = AsArg :: Nil,
    locations = Set(DirectiveLocation.Field),
    shouldInclude = _ => true
  )

  val OperationNameExtension: Middleware[Any] = Middleware.simpleExtension[Any](ctx =>
    ast.ObjectValue(
      "batch" -> ast.ObjectValue("operationName" ->
        ctx.operationName.fold(ast.NullValue(): ast.Value)(ast.StringValue(_)))))

  /** __EXPERIMENTAL__ */
  def executeBatch[Ctx, Root, Input, T](
      schema: Schema[Ctx, Root],
      queryAst: ast.Document,
      operationNames: Seq[String],
      userContext: Ctx = (),
      root: Root = (),
      variables: Input = emptyMapVars,
      queryValidator: QueryValidator = QueryValidator.default,
      deferredResolver: DeferredResolver[Ctx] = DeferredResolver.empty,
      exceptionHandler: ExceptionHandler = ExceptionHandler.empty,
      deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
      middleware: List[Middleware[Ctx]] = Nil,
      maxQueryDepth: Option[Int] = None,
      queryReducers: List[QueryReducer[Ctx, _]] = Nil,
      inferVariableDefinitions: Boolean = true
  )(implicit
      executionContext: ExecutionContext,
      marshaller: SymmetricMarshaller[T],
      um: InputUnmarshaller[Input],
      scheme: ExecutionScheme): scheme.Result[Ctx, T] = {
    val executor = Executor(
      schema,
      QueryValidator.empty,
      deferredResolver,
      exceptionHandler,
      deprecationTracker,
      middleware,
      maxQueryDepth,
      queryReducers)
    val validations =
      validateOperationNames(queryAst, operationNames, exceptionHandler)
        .flatMap(_ =>
          calcExecutionPlan(
            schema,
            queryAst,
            operationNames,
            inferVariableDefinitions,
            exceptionHandler))
        .flatMap { case res @ (updatedDocument, _) =>
          val violations = queryValidator.validateQuery(schema, updatedDocument)

          if (violations.nonEmpty) Failure(ValidationError(violations, exceptionHandler))
          else Success(res)
        }

    implicit val m = marshaller.marshaller

    val convertedVariables = convertVariables(variables, marshaller)

    validations match {
      case Failure(e) => scheme.failed(e)
      case Success((updatedDocument, executionPlan)) =>
        scheme match {
          case ss: ExecutionScheme.StreamBasedExecutionScheme[_] =>
            val childScheme =
              if (scheme.extended) ExecutionScheme.Extended else ExecutionScheme.Default

            val futures =
              doExecuteBatchPlan(
                executionPlan,
                marshaller,
                variables,
                convertedVariables,
                childScheme.extended,
                single = false) { (opName, vars, iu) =>
                implicit val iiu = iu

                executeIndividual(
                  executor,
                  updatedDocument,
                  opName,
                  userContext,
                  root,
                  vars,
                  childScheme).asInstanceOf[Future[AnyRef]]
              }

            ss.subscriptionStream
              .merge(futures.map(ss.subscriptionStream.singleFuture))
              .asInstanceOf[scheme.Result[Ctx, T]]
          case es =>
            val futures =
              doExecuteBatchPlan(
                executionPlan,
                marshaller,
                variables,
                convertedVariables,
                es.extended,
                single = true) { (opName, vars, iu) =>
                executeIndividual(
                  executor,
                  updatedDocument,
                  opName,
                  userContext,
                  root,
                  variables,
                  es).asInstanceOf[Future[AnyRef]]
              }

            futures.head.asInstanceOf[scheme.Result[Ctx, T]]
        }
    }
  }

  private def convertVariables[In, M](variables: In, marshaller: SymmetricMarshaller[M])(implicit
      iu: InputUnmarshaller[In]): Map[String, M] = {
    import sangria.marshalling.MarshallingUtil._

    implicit val m = SimpleResultMarshallerForType[M](marshaller.marshaller)

    if (iu.isMapNode(variables)) {
      val keys = iu.getMapKeys(variables)

      keys.flatMap(k => iu.getRootMapValue(variables, k).map(v => k -> v.convertMarshaled[M])).toMap
    } else Map.empty
  }

  private def doExecuteBatchPlan[In, T, M](
      plan: BatchExecutionPlan,
      marshaller: SymmetricMarshaller[M],
      origVariables: In,
      convertedVariables: Map[String, M],
      extendedScheme: Boolean,
      single: Boolean
  )(
      executeFn: (String, Any, InputUnmarshaller[Any]) => Future[T]
  )(implicit
      executionContext: ExecutionContext,
      inputUnmarshaller: InputUnmarshaller[In]): Vector[Future[T]] = {
    val inProgress = new mutable.HashMap[String, Future[T]]

    def loop(opName: String, deps: Vector[(String, Set[String])]): Future[T] =
      inProgress.getOrElseUpdate(
        opName,
        if (deps.nonEmpty) {
          val depFutures = deps.map { case d @ (dep, _) =>
            loop(dep, plan.dependencies(dep)).map(d -> _)
          }

          Future.sequence(depFutures).flatMap { resolved =>
            collectVariables(
              opName,
              plan,
              resolved,
              marshaller,
              convertedVariables,
              extendedScheme) match {
              case Some(newVars) =>
                executeFn(
                  opName,
                  newVars,
                  marshaller.inputUnmarshaller.asInstanceOf[InputUnmarshaller[Any]])
              case None =>
                executeFn(
                  opName,
                  origVariables,
                  inputUnmarshaller.asInstanceOf[InputUnmarshaller[Any]])
            }

          }
        } else {
          if (single && inProgress.nonEmpty)
            break()
          else
            executeFn(opName, origVariables, inputUnmarshaller.asInstanceOf[InputUnmarshaller[Any]])
        }
      )

    breakable {
      plan.dependencies.foreach { case (opName, deps) =>
        loop(opName, deps)
      }
    }

    inProgress.values.toVector
  }

  private def collectVariables[M](
      opName: String,
      plan: BatchExecutionPlan,
      depValues: Vector[((String, Set[String]), Any)],
      marshaller: SymmetricMarshaller[M],
      origVariables: Map[String, M],
      extendedScheme: Boolean): Option[M] = {
    val collectedValues = new mutable.HashMap[String, mutable.ListBuffer[M]]
    val iu = marshaller.inputUnmarshaller
    val m = marshaller.marshaller

    depValues.foreach { depVal =>
      val ((operationName, neededExports), executionResult) = depVal
      val exports = plan
        .exportOperations(operationName)
        .exports
        .filter(e => neededExports contains e.exportedName)
      val result =
        if (extendedScheme) executionResult.asInstanceOf[ExecutionResult[Any, M]].result
        else executionResult.asInstanceOf[M]

      def visitPath(name: String, result: M, path: Vector[String]): Unit =
        if (path.isEmpty) {
          val c = collectedValues.getOrElseUpdate(name, new mutable.ListBuffer[M])

          if (iu.isListNode(result))
            c ++= iu.getListValue(result)
          else
            c += result
        } else if (iu.isMapNode(result)) {
          val key = path.head
          val childPath = path.tail

          iu.getMapValue(result, key).foreach { value =>
            if (iu.isListNode(value)) iu.getListValue(value).foreach(visitPath(name, _, childPath))
            else visitPath(name, value, childPath)
          }
        }

      exports.foreach { export =>
        visitPath(export.exportedName, result, "data" +: export.path)
      }
    }

    // merge in original variables with overlapping keys
    origVariables.foreach { case (key, value) =>
      collectedValues.get(key) match {
        case Some(list) if iu.isListNode(value) =>
          list ++= iu.getListValue(value)
        case Some(list) =>
          list += value
        case _ => // do nothing
      }
    }

    if (collectedValues.isEmpty) None
    else {
      val collectedKeys = collectedValues.keySet
      val builder =
        m.emptyMapNode(origVariables.keys.filterNot(collectedKeys.contains).toSeq ++ collectedKeys)

      collectedValues.foreach { case (key, values) =>
        if (isListVariable(opName, plan, key))
          m.addMapNodeElem(
            builder,
            key,
            m.arrayNode(values.toVector.asInstanceOf[Vector[m.Node]]),
            false)
        else if (values.nonEmpty)
          m.addMapNodeElem(builder, key, values.head.asInstanceOf[m.Node], false)
      }

      // add original vars
      origVariables.foreach { case (key, value) =>
        if (!collectedValues.contains(key))
          m.addMapNodeElem(builder, key, value.asInstanceOf[m.Node], false)
      }

      Some(m.mapNode(builder).asInstanceOf[M])
    }
  }

  private def isListVariable(
      opName: String,
      plan: BatchExecutionPlan,
      variableName: String): Boolean = {
    val op = plan.exportOperations(opName)

    op.variableDefs.find(_.name == variableName) match {
      case Some(definition) => isInputList(definition.tpe)
      case None =>
        op.variableUsages.find(_.node.name == variableName) match {
          case Some(usage) if usage.tpe.isDefined => isInputList(usage.tpe.get)
          case _ => true
        }
    }
  }

  private def isInputList(tpe: Type): Boolean = tpe match {
    case _: ListInputType[_] => true
    case OptionInputType(ofType) => isInputList(ofType)
    case _ => false
  }

  private def isInputList(tpe: ast.Type): Boolean = tpe match {
    case _: ast.ListType => true
    case ast.NotNullType(ofType, _) => isInputList(ofType)
    case _ => false
  }

  private def executeIndividual[Ctx, Root, Input](
      executor: Executor[Ctx, Root],
      queryAst: ast.Document,
      operationName: String,
      userContext: Ctx,
      root: Root,
      variables: Input,
      scheme: ExecutionScheme
  )(implicit
      marshaller: ResultMarshaller,
      um: InputUnmarshaller[Input],
      ec: ExecutionContext): scheme.Result[Ctx, marshaller.Node] = {
    implicit val s = scheme

    executor
      .execute(queryAst, userContext, root, Some(operationName), variables)
      .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
  }

  private def validateOperationNames(
      document: ast.Document,
      operationNames: Seq[String],
      exceptionHandler: ExceptionHandler): Try[Unit] =
    if (operationNames.isEmpty)
      Failure(
        OperationSelectionError(
          s"List of operations to execute in batch is empty.",
          exceptionHandler))
    else
      operationNames.find(op => !document.operations.contains(Some(op))) match {
        case Some(opName) =>
          Failure(OperationSelectionError(s"Unknown operation name '$opName'.", exceptionHandler))
        case None => Success(())
      }

  private def calcExecutionPlan(
      schema: Schema[_, _],
      queryAst: ast.Document,
      operationNames: Seq[String],
      allowedToInferVariableDefinitions: Boolean,
      exceptionHandler: ExceptionHandler): Try[(ast.Document, BatchExecutionPlan)] = {
    val (exportedAll, exportFragments) = findUsages(schema, queryAst)

    val collectResult =
      exportedAll.foldLeft(Success(exportedAll): Try[mutable.HashMap[String, ExportOperation]]) {
        case (s @ Success(ops), (opName, op)) =>
          collectFragmentInfo(op, exportFragments, exceptionHandler) match {
            case Success(o) =>
              ops(opName) = o
              s

            case Failure(e) => Failure(e)
          }
        case (f @ Failure(_), _) => f
      }

    val exportedRelevant: Map[String, ExportOperation] =
      exportedAll.filter { case (k, _) => operationNames.contains(k) }.toMap

    collectResult
      .flatMap { _ =>
        if (allowedToInferVariableDefinitions)
          inferVariableDefinitions(exportedAll, queryAst, exceptionHandler)
        else {
          val violations =
            exportedAll.values.flatMap { op =>
              findUndefinedVariableUsages(op).map(
                UndefinedVariableDefinitionViolation(
                  op.operationName,
                  _,
                  queryAst.sourceMapper,
                  op.variableUsages.flatMap(_.node.location).toList))
            }

          if (violations.nonEmpty)
            Failure(BatchExecutionViolationError(violations.toVector, exceptionHandler))
          else
            Success(queryAst)
        }
      }
      .flatMap { updatedQueryAst =>
        val exportedVars = findExportedVariableNames(exportedRelevant)
        val dependencies = findOperationDependencies(exportedRelevant, exportedVars)

        validateCircularOperationDependencies(updatedQueryAst, dependencies, exceptionHandler)
      }
      .map { case (updatedQueryAst, dependencies) =>
        updatedQueryAst -> BatchExecutionPlan(exportedRelevant, dependencies)
      }
  }

  private def validateCircularOperationDependencies(
      queryAst: ast.Document,
      dependencies: Map[String, Vector[(String, Set[String])]],
      exceptionHandler: ExceptionHandler)
      : Try[(ast.Document, Map[String, Vector[(String, Set[String])]])] = {
    val violations = new mutable.ListBuffer[Violation]

    def loop(
        src: String,
        deps: Vector[(String, Set[String])],
        path: Vector[(String, String)]): Unit =
      if (path.exists(_._1 == src))
        violations += CircularOperationDependencyViolation(
          src,
          path.map(_._2),
          queryAst.sourceMapper,
          queryAst.operations(Some(src)).location.toList)
      else {
        deps.foreach { d =>
          loop(
            d._1,
            dependencies(d._1),
            path :+ ((src, s"$src(${d._2.map("$" + _).mkString(", ")})")))
        }
      }

    dependencies.foreach { case (op, deps) =>
      loop(op, deps, Vector.empty)
    }

    if (violations.nonEmpty)
      Failure(BatchExecutionViolationError(violations.toVector, exceptionHandler))
    else
      Success(queryAst -> dependencies)
  }

  private def findOperationDependencies(
      exportOperations: Map[String, ExportOperation],
      exportedVars: Set[String]): Map[String, Vector[(String, Set[String])]] =
    exportOperations.values.map { src =>
      val requires =
        (src.variableDefs.map(_.name).filter(exportedVars.contains) ++ src.variableUsages
          .map(_.node.name)
          .filter(exportedVars.contains)).toSet

      val providers = exportOperations.values
        .map(dst => dst.operationName -> dst.exports.map(_.exportedName).toSet.intersect(requires))
        .filter(_._2.nonEmpty)

      src.operationName -> providers.toVector
    }.toMap

  private def findExportedVariableNames(
      exportOperations: Map[String, ExportOperation]): Set[String] =
    exportOperations.values.flatMap(_.exports.map(_.exportedName)).toSet

  private def inferVariableDefinitions(
      exportOperations: mutable.HashMap[String, ExportOperation],
      queryAst: ast.Document,
      exceptionHandler: ExceptionHandler): Try[ast.Document] = {
    val inferenceViolations = new mutable.ListBuffer[Violation]

    val updatedDocument =
      AstVisitor.visit(
        queryAst,
        AstVisitor {
          case od: ast.OperationDefinition
              if od.name.isDefined && exportOperations.contains(od.name.get) =>
            val exportOperation = exportOperations(od.name.get)
            val undefined = findUndefinedVariableUsages(exportOperation)

            val usagesByName = exportOperation.variableUsages.groupBy(_.node.name)

            val newVariableDefs = new mutable.ListBuffer[ast.VariableDefinition]

            undefined.foreach { ud =>
              val allUsages = usagesByName(ud)
              val first = allUsages.head
              val firstType = first.tpe.getOrElse(
                throw new IllegalStateException(
                  "Variable usage type is not detected, but expected at this point!"))
              val firstAstType = SchemaRenderer.renderTypeNameAst(firstType)
              val tail = allUsages.tail

              val violations = new mutable.ListBuffer[Violation]

              tail.foreach { curr =>
                val currType = curr.tpe.getOrElse(throw new IllegalStateException(
                  "Variable usage type is not detected, but expected at this point!"))
                val currAstType = SchemaRenderer.renderTypeNameAst(currType)

                if (firstAstType != currAstType)
                  violations += VariableDefinitionInferenceViolation(
                    exportOperation.operationName,
                    ud,
                    firstAstType.renderPretty,
                    currAstType.renderPretty,
                    queryAst.sourceMapper,
                    first.node.location.toList ++ curr.node.location.toList
                  )
              }

              if (violations.nonEmpty)
                inferenceViolations ++= violations
              else
                newVariableDefs += ast.VariableDefinition(
                  ud,
                  firstAstType,
                  None,
                  Vector.empty,
                  Vector(ast.Comment("Inferred variable")))
            }

            if (newVariableDefs.nonEmpty && inferenceViolations.isEmpty)
              VisitorCommand.Transform(od.copy(variables = od.variables ++ newVariableDefs))
            else
              VisitorCommand.Continue
        }
      )

    if (inferenceViolations.nonEmpty)
      Failure(BatchExecutionViolationError(inferenceViolations.toVector, exceptionHandler))
    else
      Success(updatedDocument)
  }

  private def findUndefinedVariableUsages(exportOperation: ExportOperation) = {
    val defs = exportOperation.variableDefs.map(_.name).toSet
    val usages = exportOperation.variableUsages.map(_.node.name).toSet

    usages.diff(defs)
  }

  private def collectFragmentInfo(
      exportOperation: ExportOperation,
      exportFragments: mutable.HashMap[String, ExportFragment],
      exceptionHandler: ExceptionHandler): Try[ExportOperation] = {
    val currentExports = new mutable.ListBuffer[Export]
    val currentVariables = new mutable.ListBuffer[VariableUsage]

    val recursive = new mutable.HashSet[String]
    val unknown = new mutable.HashSet[String]

    def loop(spreads: Set[SpreadInfo], seenFragmentNames: Set[String], path: Vector[String]): Unit =
      spreads.foreach { s =>
        if (seenFragmentNames.contains(s.fragmentName))
          recursive += s.fragmentName
        else
          exportFragments.get(s.fragmentName) match {
            case Some(f) =>
              val spreadPath = path ++ s.path

              currentExports ++= f.exports.map(e => e.copy(path = spreadPath ++ e.path))
              currentVariables ++= f.variableUsages

              loop(f.fragmentSpreads, seenFragmentNames + s.fragmentName, spreadPath)
            case None =>
              unknown += s.fragmentName
          }
      }

    loop(exportOperation.fragmentSpreads, Set.empty, Vector.empty)

    if (recursive.nonEmpty)
      Failure(
        BatchExecutionError(
          s"Query contains recursive fragments: ${recursive.mkString(", ")}.",
          exceptionHandler))
    else if (unknown.nonEmpty)
      Failure(
        BatchExecutionError(
          s"Query contains undefined fragments: ${unknown.mkString(", ")}.",
          exceptionHandler))
    else if (currentExports.nonEmpty || currentVariables.nonEmpty)
      Success(
        exportOperation.copy(
          exports = exportOperation.exports ++ currentExports.toVector,
          variableUsages = exportOperation.variableUsages ++ currentVariables.toVector))
    else
      Success(exportOperation)
  }

  private def findUsages(schema: Schema[_, _], queryAst: ast.Document)
      : (mutable.HashMap[String, ExportOperation], mutable.HashMap[String, ExportFragment]) = {
    var currentOperation: Option[String] = None
    var currentFragment: Option[String] = None

    val currentSpreads = new mutable.HashSet[SpreadInfo]
    val currentDirectives = new mutable.ListBuffer[Export]
    val currentVariableDefs = new mutable.ListBuffer[ast.VariableDefinition]
    val currentVariables = new mutable.ListBuffer[VariableUsage]

    val exportOperations = new mutable.HashMap[String, ExportOperation]
    val exportFragments = new mutable.HashMap[String, ExportFragment]

    AstVisitor.visitAstWithTypeInfo(schema, queryAst)(typeInfo =>
      AstVisitor(
        onEnter = {
          case op: ast.OperationDefinition if op.name.isDefined =>
            currentOperation = op.name
            VisitorCommand.Continue

          case fd: ast.FragmentDefinition =>
            currentFragment = Some(fd.name)
            VisitorCommand.Continue

          case fs: ast.FragmentSpread if currentOperation.isDefined || currentFragment.isDefined =>
            currentSpreads += SpreadInfo(fs.name, calcPath(typeInfo))
            VisitorCommand.Continue

          case vd: ast.VariableDefinition
              if currentOperation.isDefined || currentFragment.isDefined =>
            currentVariableDefs += vd
            VisitorCommand.Continue

          case vv: ast.VariableValue if currentOperation.isDefined || currentFragment.isDefined =>
            currentVariables += VariableUsage(vv, typeInfo.inputType, typeInfo.defaultValue)
            VisitorCommand.Continue

          case field: ast.Field if currentOperation.isDefined || currentFragment.isDefined =>
            field.directives.find(_.name == ExportDirective.name) match {
              case Some(d) =>
                d.arguments.find(_.name == AsArg.name) match {
                  case Some(ast.Argument(_, ast.StringValue(as, _, _, _, _), _, _))
                      if typeInfo.fieldDef.isDefined =>
                    currentDirectives += Export(
                      as,
                      calcPath(typeInfo),
                      d,
                      typeInfo.fieldDef.get.fieldType)
                    VisitorCommand.Continue
                  case _ => VisitorCommand.Continue
                }
              case None => VisitorCommand.Continue
            }
        },
        onLeave = {
          case od: ast.OperationDefinition if od.name.isDefined =>
            val name = od.name.get

            currentOperation = None

            exportOperations(name) = ExportOperation(
              name,
              currentVariableDefs.toVector,
              currentVariables.toVector,
              currentDirectives.toVector,
              currentSpreads.toSet)

            currentSpreads.clear()
            currentDirectives.clear()
            currentVariableDefs.clear()
            currentVariables.clear()

            VisitorCommand.Continue

          case fd: ast.FragmentDefinition =>
            currentFragment = None

            exportFragments(fd.name) = ExportFragment(
              fd.name,
              currentVariables.toVector,
              currentDirectives.toVector,
              currentSpreads.toSet)

            currentSpreads.clear()
            currentDirectives.clear()
            currentVariableDefs.clear()
            currentVariables.clear()

            VisitorCommand.Continue
        }
      ))

    exportOperations -> exportFragments
  }

  private def calcPath(typeInfo: TypeInfo) =
    typeInfo.ancestors.collect { case f: ast.Field => f.outputName }.toVector
}
