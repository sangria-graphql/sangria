package sangria.execution

import sangria.ast
import sangria.parser.SourceMapper
import sangria.schema._
import sangria.validation.QueryValidator

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure, Try}

case class Executor[Ctx, Root](
    schema: Schema[Ctx, Root],
    root: Root = (),
    userContext: Ctx = (),
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver = DeferredResolver.empty,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = PartialFunction.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty)(implicit executionContext: ExecutionContext) {

  def execute[Input](
      queryAst: ast.Document,
      operationName: Option[String] = None,
      arguments: Option[Input] = None)(implicit marshaller: ResultMarshaller, um: InputUnmarshaller[Input]): Future[marshaller.Node] = {
    val violations = queryValidator.validateQuery(schema, queryAst)

    if (violations.nonEmpty)
      Future.successful(new ResultResolver(marshaller, exceptionHandler).resolveError(ValidationError(violations)).asInstanceOf[marshaller.Node])
    else {
      val valueCollector = new ValueCollector[Ctx, Input](schema, arguments getOrElse um.emptyNode, queryAst.sourceMapper, deprecationTracker, userContext)(um)

      val executionResult = for {
        operation <- getOperation(queryAst, operationName)
        variables <- valueCollector.getVariableValues(operation.variables)
        fieldCollector = new FieldCollector[Ctx, Root](schema, queryAst, variables, queryAst.sourceMapper, valueCollector)
        res <- executeOperation(operation, queryAst.sourceMapper, valueCollector, fieldCollector, marshaller, variables)
      } yield res

      executionResult match {
        case Success(future) => future.asInstanceOf[Future[marshaller.Node]]
        case Failure(error) => Future.successful(new ResultResolver(marshaller, exceptionHandler).resolveError(error).asInstanceOf[marshaller.Node])
      }
    }
  }

  def getOperation(document: ast.Document, operationName: Option[String]): Try[ast.OperationDefinition] =
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(new ExecutionError("Must provide operation name if query contains multiple operations"))
    else {
      val operation = operationName flatMap (opName => document.operations get Some(opName)) orElse document.operations.values.headOption

      operation map (Success(_)) getOrElse Failure(new ExecutionError(s"Unknown operation name: ${operationName.get}"))
    }

  def executeOperation(
      operation: ast.OperationDefinition,
      sourceMapper: Option[SourceMapper],
      valueCollector: ValueCollector[Ctx, _],
      fieldCollector: FieldCollector[Ctx, Root],
      marshaller: ResultMarshaller,
      variables: Map[String, Any]) = {
    for {
      tpe <- getOperationRootType(operation, sourceMapper)
      fields <- fieldCollector.collectFields(Nil, tpe, operation :: Nil)
      resolver = new Resolver[Ctx](marshaller, schema, valueCollector, variables, fieldCollector, userContext, exceptionHandler, deferredResolver, sourceMapper, deprecationTracker)
    } yield operation.operationType match {
      case ast.OperationType.Query => resolver.resolveFieldsPar(tpe, root, fields)
      case ast.OperationType.Mutation => resolver.resolveFieldsSeq(tpe, root, fields)
    }
  }

  def getOperationRootType(operation: ast.OperationDefinition, sourceMapper: Option[SourceMapper]) = operation.operationType match {
    case ast.OperationType.Query => Success(schema.query)
    case ast.OperationType.Mutation => schema.mutation map (Success(_)) getOrElse
        Failure(new ExecutionError("Schema is not configured for mutations", sourceMapper, operation.position.toList))
  }
}

object Executor {
  def execute[Ctx, Root, Input](
    schema: Schema[Ctx, Root],
    queryAst: ast.Document,
    operationName: Option[String] = None,
    arguments: Option[Input] = None,
    root: Root = (),
    userContext: Ctx = (),
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver = DeferredResolver.empty,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = PartialFunction.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty
  )(implicit executionContext: ExecutionContext, marshaller: ResultMarshaller, um: InputUnmarshaller[Input]): Future[marshaller.Node] =
    Executor(schema, root, userContext, queryValidator, deferredResolver, exceptionHandler, deprecationTracker).execute(queryAst, operationName, arguments)
}

case class HandledException(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty)
