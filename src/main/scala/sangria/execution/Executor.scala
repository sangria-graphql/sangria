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
    middleware: List[Middleware] = Nil,
    maxQueryDepth: Option[Int] = None)(implicit executionContext: ExecutionContext) {

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
        operation <- getOperation(queryAst, operationName)
        unmarshalledVariables <- valueCollector.getVariableValues(operation.variables)
        fieldCollector = new FieldCollector[Ctx, Root](schema, queryAst, unmarshalledVariables, queryAst.sourceMapper, valueCollector)
        res <- executeOperation(MiddlewareQueryContext(this, queryAst, operationName, variables, um), operation, queryAst.sourceMapper, valueCollector, fieldCollector, marshaller, unmarshalledVariables)
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
      middlewareCtx: MiddlewareQueryContext[Ctx, _],
      operation: ast.OperationDefinition,
      sourceMapper: Option[SourceMapper],
      valueCollector: ValueCollector[Ctx, _],
      fieldCollector: FieldCollector[Ctx, Root],
      marshaller: ResultMarshaller,
      variables: Map[String, Any]) =
    for {
      tpe <- getOperationRootType(operation, sourceMapper)
      fields <- fieldCollector.collectFields(Nil, tpe, operation :: Nil)
      middlewareVal = middleware map (m => m.beforeQuery(middlewareCtx) -> m)
      resolver = new Resolver[Ctx](marshaller, middlewareCtx, schema, valueCollector, variables, fieldCollector, userContext, exceptionHandler, deferredResolver, sourceMapper, deprecationTracker, middlewareVal, maxQueryDepth)
    } yield {
      val result =
        operation.operationType match {
          case ast.OperationType.Query => resolver.resolveFieldsPar(tpe, root, fields)
          case ast.OperationType.Mutation => resolver.resolveFieldsSeq(tpe, root, fields)
        }

      if (middlewareVal.nonEmpty) {
        def onAfter() =
          middlewareVal foreach {case (v, m) => m.afterQuery(v.asInstanceOf[m.QueryVal], middlewareCtx)}

        result
          .map {x => onAfter(); x}
          .recover {case e => onAfter(); throw e}
      } else result

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
    variables: Input = emptyMapVars,
    root: Root = (),
    userContext: Ctx = (),
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver[Ctx] = DeferredResolver.empty,
    exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = PartialFunction.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
    middleware: List[Middleware] = Nil,
    maxQueryDepth: Option[Int] = None
  )(implicit executionContext: ExecutionContext, marshaller: ResultMarshaller, um: InputUnmarshaller[Input]): Future[marshaller.Node] =
    Executor(schema, root, userContext, queryValidator, deferredResolver, exceptionHandler, deprecationTracker, middleware, maxQueryDepth).execute(queryAst, operationName, variables)
}

case class HandledException(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty)
