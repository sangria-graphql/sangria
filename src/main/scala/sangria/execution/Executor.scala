package sangria.execution

import sangria.ast
import sangria.parser.SourceMapper
import sangria.schema._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Success, Failure, Try}

case class Executor[Ctx, Root](
    schema: Schema[Ctx, Root],
    root: Root = (),
    userContext: Ctx = (),
    deferredResolver: DeferredResolver = NilDeferredResolver,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node] = PartialFunction.empty)(implicit executionContext: ExecutionContext) {

  def execute[Input](
      queryAst: ast.Document,
      operationName: Option[String] = None,
      arguments: Option[Input] = None)(implicit marshaller: ResultMarshaller, um: InputUnmarshaller[Input]): Future[marshaller.Node] = {
    val valueExecutor = new ValueExecutor[Input](schema, arguments getOrElse um.emptyNode, queryAst.sourceMapper)(um)

    val executionResult = for {
      operation <- getOperation(queryAst, operationName)
      variables <- valueExecutor.getVariableValues(operation.variables)
      fieldExecutor = new FieldExecutor[Ctx, Root](schema, queryAst, variables, queryAst.sourceMapper, valueExecutor)
      res <- executeOperation(operation, queryAst.sourceMapper, valueExecutor, fieldExecutor, marshaller, variables)
    } yield res

    executionResult match {
      case Success(future) => future.asInstanceOf[Future[marshaller.Node]]
      case Failure(error) => Future.successful(new ResultResolver(marshaller, exceptionHandler).resolveError(error).asInstanceOf[marshaller.Node])
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
      valueExecutor: ValueExecutor[_],
      fieldExecutor: FieldExecutor[Ctx, Root],
      marshaller: ResultMarshaller,
      variables: Map[String, Any]) = {
    for {
      tpe <- getOperationRootType(operation, sourceMapper)
      fields <- fieldExecutor.collectFields(Nil, tpe, operation.selections)
      resolver = new Resolver[Ctx](marshaller, schema, valueExecutor, variables, fieldExecutor, userContext, exceptionHandler, deferredResolver, sourceMapper)
    } yield resolver.resolveFields(tpe, root, fields)
  }

  def getOperationRootType(operation: ast.OperationDefinition, sourceMapper: Option[SourceMapper]) = operation.operationType match {
    case ast.OperationType.Query => Success(schema.query)
    case ast.OperationType.Mutation => schema.mutation map (Success(_)) getOrElse
        Failure(new ExecutionError("Schema is not configured for mutations", sourceMapper, operation.position))
  }
}
