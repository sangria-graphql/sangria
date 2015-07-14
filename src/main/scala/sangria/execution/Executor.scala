package sangria.execution

import sangria.ast
import sangria.parser.SourceMapper
import sangria.schema._

import scala.concurrent.Future
import scala.util.{Success, Failure, Try}

case class Executor[Ctx, Root](
    schema: Schema[Ctx, Root],
    root: Root = (),
    userContext: Ctx = (),
    deferredResolver: DeferredResolver = NilDeferredResolver,
    exceptionHandler: PartialFunction[Throwable, String] = PartialFunction.empty)(implicit scheduler: SangriaScheduler) {

  def execute[Input](
      queryAst: ast.Document,
      operationName: Option[String] = None,
      arguments: Option[Input] = None)(implicit marshaller: ResultMarshaller, um: InputUnmarshaller[Input]): Future[ExecutionResult[marshaller.Node]] = {
    val valueExecutor = new ValueExecutor[Input](schema, arguments getOrElse um.emptyNode, queryAst.sourceMapper)(um)

    val foo = for {
      operation <- getOperation(queryAst, operationName)
      variables <- valueExecutor.getVariableValues(operation.variables)
      res <- executeOperation(operation, variables, queryAst.sourceMapper, valueExecutor)
    } yield ExecutionResult(marshaller.booleanNode(true), Nil, marshaller.booleanNode(true), res)

    Future.fromTry(foo)
  }

  def handleException(exception: Throwable) = exception match {
    case e: UserFacingError => e.getMessage
    case e if exceptionHandler isDefinedAt e => exceptionHandler(e)
    case e =>
      e.printStackTrace() // todo proper logging?
      "Internal server error"
  }

  def getOperation(document: ast.Document, operationName: Option[String]): Try[ast.OperationDefinition] =
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(new ExecutionError("Must provide operation name if query contains multiple operations"))
    else {
      val operation = operationName flatMap (opName => document.operations get Some(opName)) orElse document.operations.values.headOption

      operation map (Success(_)) getOrElse Failure(new ExecutionError(s"Unknown operation name: ${operationName.get}"))
    }

  def executeOperation(operation: ast.OperationDefinition, variables: Map[String, Any], sourceMapper: Option[SourceMapper], valueExecutor: ValueExecutor[_]) = {
    for {
      tpe <- getOperationRootType(operation, sourceMapper)
      fields = collectFields(tpe, operation.selections, variables, sourceMapper, valueExecutor)
    } yield fields
  }

  def getOperationRootType(operation: ast.OperationDefinition, sourceMapper: Option[SourceMapper]) = operation.operationType match {
    case ast.OperationType.Query => Success(schema.query)
    case ast.OperationType.Mutation => schema.mutation map (Success(_)) getOrElse
        Failure(new ExecutionError("Schema is not configured for mutations", sourceMapper, operation.position))
  }

  def collectFields(tpe: ObjectType[Ctx, Root], selections: List[ast.Selection], variables: Map[String, Any], sourceMapper: Option[SourceMapper], valueExecutor: ValueExecutor[_]): Try[Map[String, Try[List[ast.Field]]]] =
    selections.foldLeft(Success(Map.empty) : Try[Map[String, Try[List[ast.Field]]]]) {
      case (f @ Failure(_), selection) => f
      case (s @ Success(acc), selection) =>
        selection match {
          case field @ ast.Field(_, _, _, dirs, _, _) =>
            val name = resultName(field)

            shouldIncludeNode(dirs, selection, variables, sourceMapper, valueExecutor) match {
              case Success(true) => acc.get(name) match {
                case Some(Success(list)) => Success(acc.updated(name, Success(list :+ field)))
                case Some(Failure(_)) => s
                case None => Success(acc.updated(name, Success(field :: Nil)))
              }
              case Success(false) => s
              case Failure(error) => Success(acc.updated(name, Failure(error)))
            }
          case fragment @ ast.InlineFragment(typeCondition, dirs, fragmentSelections, _) =>
            for {
              shouldInclude <- shouldIncludeNode(dirs, selection, variables, sourceMapper, valueExecutor)
              fragmentConditionMatch <- doesFragmentConditionMatch(tpe, fragment, sourceMapper)
              fragmentFields <-
                if (shouldInclude && fragmentConditionMatch)
                  collectFields(tpe, fragmentSelections, variables, sourceMapper, valueExecutor)
                else s
            } yield fragmentFields
        }
    }

  def resultName(field: ast.Field) = field.alias getOrElse field.name

  def shouldIncludeNode(directives: List[ast.Directive], selection: ast.Selection, variables: Map[String, Any], sourceMapper: Option[SourceMapper], valueExecutor: ValueExecutor[_]): Try[Boolean] = {
    val possibleDirs = directives
        .map(d => schema.directivesByName
          .get(d.name)
          .map(dd => selection match {
            case _: ast.Field if !dd.onField => Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on fields", sourceMapper, d.position))
            case _: ast.InlineFragment | _: ast.FragmentSpread if !dd.onFragment =>
              Failure(new ExecutionError(s"Directive '${dd.name}' is not allowed to be used on fields", sourceMapper, d.position))
            case _ => Success(d -> dd)
          })
          .getOrElse(Failure(new ExecutionError(s"Directive '${d.name}' not found.", sourceMapper, d.position))))
        .map(_.flatMap{case (astDir, dir) => valueExecutor.getAttributeValues(dir.arguments, astDir.arguments, variables) map (dir -> _)})

    possibleDirs.collect{case Failure(error) => error}.headOption map (Failure(_)) getOrElse {
      val validDirs = possibleDirs collect {case Success(v) => v}
      val should = validDirs.forall { case (dir, args) => dir.shouldInclude(DirectiveContext(selection, dir, args)) }

      Success(should)
    }
  }

  def doesFragmentConditionMatch(tpe: ObjectType[_, _], conditional: ast.ConditionalFragment, sourceMapper: Option[SourceMapper]): Try[Boolean] =
    schema.outputTypes.get(conditional.typeCondition)
      .map(condTpe => Success(condTpe.name == tpe.name || (condTpe.isInstanceOf[AbstractType] && schema.isPossibleType(condTpe.name, tpe))))
      .getOrElse(Failure(new ExecutionError(s"Unknown type '${conditional.typeCondition}'.", sourceMapper, conditional.position)))
}

case class ExecutionResult[T](data: T, errors: List[T], result: T, foo: Any)
