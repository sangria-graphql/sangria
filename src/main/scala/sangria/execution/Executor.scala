package sangria.execution

import sangria.ast
import sangria.integration.{ResultMarshaller, InputUnmarshaller}
import sangria.parser.SourceMapper
import sangria.schema._
import sangria.validation.QueryValidator
import InputUnmarshaller.emptyMapVars

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure, Try}

case class Executor[Ctx, Root](
    schema: Schema[Ctx, Root],
    root: Root = (),
    userContext: Ctx = (),
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver[Ctx] = DeferredResolver.empty,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = PartialFunction.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
    middleware: List[Middleware[Ctx]] = Nil,
    maxQueryDepth: Option[Int] = None,
    queryReducers: List[QueryReducer[Ctx, _]] = Nil)(implicit executionContext: ExecutionContext) {

  def execute[Input](
      queryAst: ast.Document,
      operationName: Option[String] = None,
      variables: Input = emptyMapVars)(implicit marshaller: ResultMarshaller, um: InputUnmarshaller[Input]): Future[marshaller.Node] = {
    val violations = queryValidator.validateQuery(schema, queryAst)

    if (violations.nonEmpty)
      Future.successful(new ResultResolver(marshaller, exceptionHandler).resolveError(ValidationError(violations)).asInstanceOf[marshaller.Node])
    else {                              
      val valueCollector = new ValueCollector[Ctx, Input](schema, variables, queryAst.sourceMapper, deprecationTracker, userContext)(um)

      val executionResult = for {
        operation ← getOperation(queryAst, operationName)
        unmarshalledVariables ← valueCollector.getVariableValues(operation.variables)
        fieldCollector = new FieldCollector[Ctx, Root](schema, queryAst, unmarshalledVariables, queryAst.sourceMapper, valueCollector)
        res ← executeOperation(
          queryAst,
          operationName,
          variables,
          um,
          operation,
          queryAst.sourceMapper,
          valueCollector,
          fieldCollector,
          marshaller,
          unmarshalledVariables)
      } yield res

      executionResult match {
        case Success(future) ⇒ future
        case Failure(error) ⇒ Future.successful(new ResultResolver(marshaller, exceptionHandler).resolveError(error).asInstanceOf[marshaller.Node])
      }
    }
  }

  def getOperation(document: ast.Document, operationName: Option[String]): Try[ast.OperationDefinition] =
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(new ExecutionError("Must provide operation name if query contains multiple operations"))
    else {
      val operation = operationName flatMap (opName ⇒ document.operations get Some(opName)) orElse document.operations.values.headOption

      operation map (Success(_)) getOrElse Failure(new ExecutionError(s"Unknown operation name: ${operationName.get}"))
    }

  def executeOperation[Input](
      queryAst: ast.Document,
      operationName: Option[String] = None,
      inputVariables: Input,
      inputUnmarshaller: InputUnmarshaller[Input],
      operation: ast.OperationDefinition,
      sourceMapper: Option[SourceMapper],
      valueCollector: ValueCollector[Ctx, _],
      fieldCollector: FieldCollector[Ctx, Root],
      marshaller: ResultMarshaller,
      variables: Map[String, Any]): Try[Future[marshaller.Node]] =
    for {
      tpe ← getOperationRootType(operation, sourceMapper)
      fields ← fieldCollector.collectFields(Nil, tpe, operation :: Nil)
    } yield {
      def doExecute(ctx: Ctx) = {
        val middlewareCtx = MiddlewareQueryContext(ctx, this, queryAst, operationName, inputVariables, inputUnmarshaller)

        val middlewareVal = middleware map (m ⇒ m.beforeQuery(middlewareCtx) → m)

        val resolver = new Resolver[Ctx](
          marshaller,
          middlewareCtx,
          schema,
          valueCollector,
          variables,
          fieldCollector,
          ctx,
          exceptionHandler,
          deferredResolver,
          sourceMapper,
          deprecationTracker,
          middlewareVal,
          maxQueryDepth)

        val result =
          operation.operationType match {
            case ast.OperationType.Query ⇒ resolver.resolveFieldsPar(tpe, root, fields).asInstanceOf[Future[marshaller.Node]]
            case ast.OperationType.Mutation ⇒ resolver.resolveFieldsSeq(tpe, root, fields).asInstanceOf[Future[marshaller.Node]]
          }

        if (middlewareVal.nonEmpty) {
          def onAfter() =
            middlewareVal foreach { case (v, m) ⇒ m.afterQuery(v.asInstanceOf[m.QueryVal], middlewareCtx)}

          result
              .map { x ⇒ onAfter(); x}
              .recover { case e ⇒ onAfter(); throw e}
        } else result
      }

      if (queryReducers.nonEmpty)
        reduceQuery(fieldCollector, valueCollector, variables, tpe, fields, queryReducers.toVector) match {
          case future: Future[Ctx] ⇒ future.flatMap(newCtx ⇒ doExecute(newCtx))
          case newCtx: Ctx @unchecked ⇒ doExecute(newCtx)
        }
      else doExecute(userContext)
    }

  def getOperationRootType(operation: ast.OperationDefinition, sourceMapper: Option[SourceMapper]) = operation.operationType match {
    case ast.OperationType.Query ⇒ Success(schema.query)
    case ast.OperationType.Mutation ⇒ schema.mutation map (Success(_)) getOrElse
        Failure(new ExecutionError("Schema is not configured for mutations", sourceMapper, operation.position.toList))
  }

  private def reduceQuery[Val](
      fieldCollector: FieldCollector[Ctx, Val],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, Any],
      rootTpe: ObjectType[_, _],
      fields: Map[String, (ast.Field, Try[List[ast.Field]])],
      reducers: Vector[QueryReducer[Ctx, _]]): Any = {
    // Using mutability here locally in order to reduce footprint
    import scala.collection.mutable.ListBuffer

    val argumentValuesFn = (path: List[String], argumentDefs: List[Argument[_]], argumentAsts: List[ast.Argument]) ⇒
      valueCollector.getFieldArgumentValues(path, argumentDefs, argumentAsts, variables)

    val initialValues: Vector[Any] = reducers map (_.initial)

    def loop(path: List[String], tpe: OutputType[_], astFields: List[ast.Field]): Seq[Any] =
      tpe match {
        case OptionType(ofType) ⇒ loop(path, ofType, astFields)
        case ListType(ofType) ⇒ loop(path, ofType, astFields)
        case objTpe: ObjectType[Ctx, _] ⇒
          fieldCollector.collectFields(path, objTpe, astFields) match {
            case Success(ff) ⇒
              ff.values.toVector.foldLeft(ListBuffer(initialValues: _*)) {
                case (acc, (_, Success(fields))) if objTpe.getField(schema, fields.head.name).nonEmpty ⇒
                  val astField = fields.head
                  val field = objTpe.getField(schema, astField.name).head
                  val newPath = path :+ astField.outputName
                  val childReduced = loop(newPath, field.fieldType, fields)

                  for (i ← 0 until reducers.size) {
                    val reducer = reducers(i)

                    acc(i) = reducer.reduceField[Any](
                      acc(i).asInstanceOf[reducer.Acc],
                      childReduced(i).asInstanceOf[reducer.Acc],
                      newPath, userContext, fields,
                      objTpe.asInstanceOf[ObjectType[Any, Any]],
                      field.asInstanceOf[Field[Ctx, Any]], argumentValuesFn)
                  }

                  acc
                case (acc, _) ⇒ acc
              }
            case Failure(_) ⇒ initialValues
          }
        case abst: AbstractType ⇒
          schema.possibleTypes
            .get (abst.name)
            .map (types ⇒
              types.map(loop(path, _, astFields)).transpose.zipWithIndex.map{
                case (values, idx) ⇒
                  val reducer = reducers(idx)
                  reducer.reduceAlternatives(values.asInstanceOf[Seq[reducer.Acc]])
              })
            .getOrElse (initialValues)
        case s: ScalarType[_] ⇒ reducers map (_.reduceScalar(path, userContext, s))
        case e: EnumType[_] ⇒ reducers map (_.reduceEnum(path, userContext, e))
        case _ ⇒ initialValues
      }

    val reduced = fields.values.toVector.foldLeft(ListBuffer(initialValues: _*)) {
      case (acc, (_, Success(astFields))) if rootTpe.getField(schema, astFields.head.name).nonEmpty =>
        val astField = astFields.head
        val field = rootTpe.getField(schema, astField.name).head
        val path = astField.outputName :: Nil
        val childReduced = loop(path, field.fieldType, astFields)

        for (i ← 0 until reducers.size) {
          val reducer = reducers(i)

          acc(i) = reducer.reduceField(
            acc(i).asInstanceOf[reducer.Acc],
            childReduced(i).asInstanceOf[reducer.Acc],
            path, userContext, astFields,
            rootTpe.asInstanceOf[ObjectType[Any, Any]],
            field.asInstanceOf[Field[Ctx, Any]], argumentValuesFn)
        }

        acc
      case (acc, _) ⇒ acc
    }

    // Unsafe part to avoid addition boxing in order to reduce the footprint
    reducers.zipWithIndex.foldLeft(userContext: Any) {
      case (acc: Future[Ctx], (reducer, idx)) ⇒
        acc.flatMap(a ⇒ reducer.reduceCtx(reduced(idx).asInstanceOf[reducer.Acc], a) match {
          case FutureValue(future) ⇒ future
          case Value(value) ⇒ Future.successful(value)
          case TryValue(value) ⇒ Future.fromTry(value)
        })

      case (acc: Ctx @unchecked, (reducer, idx)) ⇒
        reducer.reduceCtx(reduced(idx).asInstanceOf[reducer.Acc], acc)  match {
          case FutureValue(future) ⇒ future
          case Value(value) ⇒ value
          case TryValue(value) ⇒ value.get
        }
    }
  }
}

object Executor {
  def execute[Ctx, Root, Input](
    schema: Schema[Ctx, Root],
    queryAst: ast.Document,
    operationName: Option[String] = None,
    variables: Input = emptyMapVars,
    root: Root = (),
    userContext: Ctx = (),
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver[Ctx] = DeferredResolver.empty,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = PartialFunction.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
    middleware: List[Middleware[Ctx]] = Nil,
    maxQueryDepth: Option[Int] = None,
    queryReducers: List[QueryReducer[Ctx, _]] = Nil
  )(implicit executionContext: ExecutionContext, marshaller: ResultMarshaller, um: InputUnmarshaller[Input]): Future[marshaller.Node] =
    Executor(schema, root, userContext, queryValidator, deferredResolver, exceptionHandler, deprecationTracker, middleware, maxQueryDepth, queryReducers).execute(queryAst, operationName, variables)
}

case class HandledException(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty)
