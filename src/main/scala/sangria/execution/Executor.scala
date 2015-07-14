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
      fieldExecutor = new FieldExecutor[Ctx, Root](schema, queryAst, variables, queryAst.sourceMapper, valueExecutor)
      res <- executeOperation(operation, queryAst.sourceMapper, fieldExecutor)
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

  def executeOperation(operation: ast.OperationDefinition, sourceMapper: Option[SourceMapper], fieldExecutor: FieldExecutor[Ctx, Root]) = {
    for {
      tpe <- getOperationRootType(operation, sourceMapper)
      fields = fieldExecutor.collectFields(tpe, operation.selections)
    } yield fields
  }

  def getOperationRootType(operation: ast.OperationDefinition, sourceMapper: Option[SourceMapper]) = operation.operationType match {
    case ast.OperationType.Query => Success(schema.query)
    case ast.OperationType.Mutation => schema.mutation map (Success(_)) getOrElse
        Failure(new ExecutionError("Schema is not configured for mutations", sourceMapper, operation.position))
  }
}

case class ExecutionResult[T](data: T, errors: List[T], result: T, foo: Any)
