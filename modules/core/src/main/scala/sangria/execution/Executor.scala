package sangria.execution

import sangria.ast
import sangria.marshalling.{InputUnmarshaller, ResultMarshaller}
import sangria.parser.SourceMapper
import sangria.schema._
import sangria.validation.QueryValidator
import InputUnmarshaller.emptyMapVars
import sangria.effect.Effect
import sangria.execution.deferred.DeferredResolver

import scala.concurrent.{ExecutionContext, Future}
import scala.util.control.NonFatal
import scala.util.{Failure, Success, Try}

case class Executor[Ctx, Root, F[_]: Effect](
    schema: Schema[Ctx, Root, F],
    queryValidator: QueryValidator = QueryValidator.default,
    deferredResolver: DeferredResolver[Ctx, F] = DeferredResolver.empty,
    exceptionHandler: ExceptionHandler = ExceptionHandler.empty,
    deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
    middleware: List[Middleware[Ctx, F]] = Nil,
    maxQueryDepth: Option[Int] = None,
    queryReducers: List[QueryReducer[Ctx, _, F]] = Nil
) {
  def prepare[Input](
      queryAst: ast.Document,
      userContext: Ctx,
      root: Root,
      operationName: Option[String] = None,
      variables: Input = emptyMapVars
  )(implicit
      um: InputUnmarshaller[Input],
      ec: ExecutionContext): Future[PreparedQuery[Ctx, Root, Input, F]] = {
    val (violations, validationTiming) =
      TimeMeasurement.measure(queryValidator.validateQuery(schema, queryAst))

    if (violations.nonEmpty)
      Future.failed(ValidationError(violations, exceptionHandler))
    else {
      val scalarMiddleware = Middleware.composeFromScalarMiddleware(middleware, userContext)
      val valueCollector = new ValueCollector[Ctx, Input, F](
        schema,
        variables,
        queryAst.sourceMapper,
        deprecationTracker,
        userContext,
        exceptionHandler,
        scalarMiddleware,
        false)(um)

      val executionResult = for {
        operation <- Executor.getOperation(exceptionHandler, queryAst, operationName)
        unmarshalledVariables <- valueCollector.getVariableValues(
          operation.variables,
          scalarMiddleware)
        fieldCollector = new FieldCollector[Ctx, Root, F](
          schema,
          queryAst,
          unmarshalledVariables,
          queryAst.sourceMapper,
          valueCollector,
          exceptionHandler)
        tpe <- Executor.getOperationRootType(
          schema,
          exceptionHandler,
          operation,
          queryAst.sourceMapper)
        fields <- fieldCollector.collectFields(ExecutionPath.empty, tpe, Vector(operation))
      } yield {
        val preparedFields = fields.fields.flatMap {
          case CollectedField(_, astField, Success(_)) =>
            val allFields =
              tpe.getField(schema, astField.name).asInstanceOf[Vector[Field[Ctx, Root, F]]]
            val field = allFields.head
            val args = valueCollector.getFieldArgumentValues(
              ExecutionPath.empty.add(astField, tpe),
              Some(astField),
              field.arguments,
              astField.arguments,
              unmarshalledVariables)

            args.toOption.map(PreparedField(field, _))
          case _ => None
        }

        QueryReducerExecutor
          .reduceQuery(
            schema,
            queryReducers,
            exceptionHandler,
            fieldCollector,
            valueCollector,
            unmarshalledVariables,
            tpe,
            fields,
            userContext)
          .map { case (newCtx, timing) =>
            new PreparedQuery[Ctx, Root, Input, F](
              queryAst,
              operation,
              tpe,
              newCtx,
              root,
              preparedFields,
              (c: Ctx, r: Root, m: ResultMarshaller, scheme: ExecutionScheme) =>
                executeOperation(
                  queryAst,
                  operationName,
                  variables,
                  um,
                  operation,
                  queryAst.sourceMapper,
                  valueCollector,
                  fieldCollector,
                  m,
                  unmarshalledVariables,
                  tpe,
                  fields,
                  c,
                  r,
                  scheme,
                  validationTiming,
                  timing
                ))
          }
      }

      executionResult match {
        case Success(future) => future
        case Failure(error) => Future.failed(error)
      }
    }
  }

  def execute[Input](
      queryAst: ast.Document,
      userContext: Ctx,
      root: Root,
      operationName: Option[String] = None,
      variables: Input = emptyMapVars
  )(implicit
      marshaller: ResultMarshaller,
      um: InputUnmarshaller[Input],
      scheme: ExecutionScheme,
      ec: ExecutionContext): scheme.Result[Ctx, marshaller.Node] = {
    val (violations, validationTiming) =
      TimeMeasurement.measure(queryValidator.validateQuery(schema, queryAst))

    if (violations.nonEmpty)
      scheme.failed(ValidationError(violations, exceptionHandler))
    else {
      val scalarMiddleware = Middleware.composeFromScalarMiddleware(middleware, userContext)
      val valueCollector = new ValueCollector[Ctx, Input, F](
        schema,
        variables,
        queryAst.sourceMapper,
        deprecationTracker,
        userContext,
        exceptionHandler,
        scalarMiddleware,
        false)(um)

      val executionResult = for {
        operation <- Executor.getOperation(exceptionHandler, queryAst, operationName)
        unmarshalledVariables <- valueCollector.getVariableValues(
          operation.variables,
          scalarMiddleware)
        fieldCollector = new FieldCollector[Ctx, Root, F](
          schema,
          queryAst,
          unmarshalledVariables,
          queryAst.sourceMapper,
          valueCollector,
          exceptionHandler)
        tpe <- Executor.getOperationRootType(
          schema,
          exceptionHandler,
          operation,
          queryAst.sourceMapper)
        fields <- fieldCollector.collectFields(ExecutionPath.empty, tpe, Vector(operation))
      } yield {
        val reduced = QueryReducerExecutor.reduceQuery(
          schema,
          queryReducers,
          exceptionHandler,
          fieldCollector,
          valueCollector,
          unmarshalledVariables,
          tpe,
          fields,
          userContext)
        scheme.flatMapFuture(reduced) { case (newCtx, timing) =>
          executeOperation(
            queryAst,
            operationName,
            variables,
            um,
            operation,
            queryAst.sourceMapper,
            valueCollector,
            fieldCollector,
            marshaller,
            unmarshalledVariables,
            tpe,
            fields,
            newCtx,
            root,
            scheme,
            validationTiming,
            timing
          )
        }
      }

      executionResult match {
        case Success(result) => result
        case Failure(error) => scheme.failed(error)
      }
    }
  }

  private def executeOperation[Input](
      queryAst: ast.Document,
      operationName: Option[String],
      inputVariables: Input,
      inputUnmarshaller: InputUnmarshaller[Input],
      operation: ast.OperationDefinition,
      sourceMapper: Option[SourceMapper],
      valueCollector: ValueCollector[Ctx, _, F],
      fieldCollector: FieldCollector[Ctx, Root, F],
      marshaller: ResultMarshaller,
      variables: Map[String, VariableValue],
      tpe: ObjectType[Ctx, Root, F],
      fields: CollectedFields,
      ctx: Ctx,
      root: Root,
      scheme: ExecutionScheme,
      validationTiming: TimeMeasurement,
      queryReducerTiming: TimeMeasurement
  )(implicit executionContext: ExecutionContext): scheme.Result[Ctx, marshaller.Node] = {
    val middlewareCtx = MiddlewareQueryContext(
      ctx,
      this,
      queryAst,
      operationName,
      inputVariables,
      inputUnmarshaller,
      validationTiming,
      queryReducerTiming)

    try {
      val middlewareVal = middleware.map(m => m.beforeQuery(middlewareCtx) -> m)
      val deferredResolverState = deferredResolver.initialQueryState

      val resolver = new Resolver[Ctx, F](
        marshaller,
        middlewareCtx.asInstanceOf[MiddlewareQueryContext[Ctx, _, _, F]],
        schema.asInstanceOf[Schema[Ctx, _, F]],
        valueCollector,
        variables,
        fieldCollector,
        ctx,
        exceptionHandler,
        deferredResolver,
        sourceMapper,
        deprecationTracker,
        middlewareVal.asInstanceOf[List[(Any, sangria.execution.Middleware[Ctx, F])]],
        maxQueryDepth,
        deferredResolverState,
        scheme.extended,
        validationTiming,
        queryReducerTiming,
        queryAst)

      val result =
        operation.operationType match {
          case ast.OperationType.Query =>
            resolver
              .resolveFieldsPar(tpe, root, fields)(scheme)
              .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
          case ast.OperationType.Mutation =>
            resolver
              .resolveFieldsSeq(tpe, root, fields)(scheme)
              .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
          case ast.OperationType.Subscription =>
            tpe.uniqueFields.head.tags.collectFirst { case SubscriptionField(s) => s } match {
              case Some(stream) =>
                // Streaming is supported - resolve as a real subscription
                resolver
                  .resolveFieldsSubs(tpe, root, fields)(scheme)
                  .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
              case None =>
                // No streaming is supported - resolve as a normal "query" operation
                resolver
                  .resolveFieldsPar(tpe, root, fields)(scheme)
                  .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
            }

        }

      if (middlewareVal.nonEmpty)
        scheme.onComplete(result)(middlewareVal.foreach { case (v, m) =>
          m.afterQuery(v.asInstanceOf[m.QueryVal], middlewareCtx)
        })
      else result
    } catch {
      case NonFatal(error) =>
        scheme.failed(error)
    }
  }
}

object Executor {
  type ExceptionHandler = sangria.execution.ExceptionHandler

  def execute[Ctx, Root, Input, F[_]: Effect](
      schema: Schema[Ctx, Root, F],
      queryAst: ast.Document,
      userContext: Ctx = (),
      root: Root = (),
      operationName: Option[String] = None,
      variables: Input = emptyMapVars,
      queryValidator: QueryValidator = QueryValidator.default,
      deferredResolver: DeferredResolver[Ctx, F] = DeferredResolver.empty,
      exceptionHandler: ExceptionHandler = ExceptionHandler.empty,
      deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
      middleware: List[Middleware[Ctx, F]] = Nil,
      maxQueryDepth: Option[Int] = None,
      queryReducers: List[QueryReducer[Ctx, _, F]] = Nil
  )(implicit
      executionContext: ExecutionContext,
      marshaller: ResultMarshaller,
      um: InputUnmarshaller[Input],
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    Executor(
      schema,
      queryValidator,
      deferredResolver,
      exceptionHandler,
      deprecationTracker,
      middleware,
      maxQueryDepth,
      queryReducers)
      .execute(queryAst, userContext, root, operationName, variables)

  def prepare[Ctx, Root, Input, F[_]: Effect](
      schema: Schema[Ctx, Root, F],
      queryAst: ast.Document,
      userContext: Ctx = (),
      root: Root = (),
      operationName: Option[String] = None,
      variables: Input = emptyMapVars,
      queryValidator: QueryValidator = QueryValidator.default,
      deferredResolver: DeferredResolver[Ctx, F] = DeferredResolver.empty,
      exceptionHandler: ExceptionHandler = ExceptionHandler.empty,
      deprecationTracker: DeprecationTracker = DeprecationTracker.empty,
      middleware: List[Middleware[Ctx, F]] = Nil,
      maxQueryDepth: Option[Int] = None,
      queryReducers: List[QueryReducer[Ctx, _, F]] = Nil
  )(implicit
      executionContext: ExecutionContext,
      um: InputUnmarshaller[Input]): Future[PreparedQuery[Ctx, Root, Input, F]] =
    Executor(
      schema,
      queryValidator,
      deferredResolver,
      exceptionHandler,
      deprecationTracker,
      middleware,
      maxQueryDepth,
      queryReducers)
      .prepare(queryAst, userContext, root, operationName, variables)

  def getOperationRootType[Ctx, Root, F[_]](
      schema: Schema[Ctx, Root, F],
      exceptionHandler: ExceptionHandler,
      operation: ast.OperationDefinition,
      sourceMapper: Option[SourceMapper]) = operation.operationType match {
    case ast.OperationType.Query =>
      Success(schema.query)
    case ast.OperationType.Mutation =>
      schema.mutation
        .map(Success(_))
        .getOrElse(
          Failure(
            OperationSelectionError(
              "Schema is not configured for mutations",
              exceptionHandler,
              sourceMapper,
              operation.location.toList)))
    case ast.OperationType.Subscription =>
      schema.subscription
        .map(Success(_))
        .getOrElse(
          Failure(
            OperationSelectionError(
              "Schema is not configured for subscriptions",
              exceptionHandler,
              sourceMapper,
              operation.location.toList)))
  }

  def getOperation(
      exceptionHandler: ExceptionHandler,
      document: ast.Document,
      operationName: Option[String]): Try[ast.OperationDefinition] =
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(
        OperationSelectionError(
          "Must provide operation name if query contains multiple operations",
          exceptionHandler))
    else {
      val unexpectedDefinition = document.definitions.find(d =>
        !(d.isInstanceOf[ast.OperationDefinition] || d.isInstanceOf[ast.FragmentDefinition]))

      unexpectedDefinition match {
        case Some(unexpected) =>
          Failure(new ExecutionError(
            s"GraphQL cannot execute a request containing a ${unexpected.getClass.getSimpleName}.",
            exceptionHandler))
        case None =>
          operationName match {
            case Some(opName) =>
              document.operations
                .get(Some(opName))
                .map(Success(_))
                .getOrElse(Failure(
                  OperationSelectionError(s"Unknown operation name '$opName'", exceptionHandler)))
            case None =>
              Success(document.operations.values.head)
          }
      }
    }
}

class PreparedQuery[Ctx, Root, Input, F[_]] private[execution] (
    val queryAst: ast.Document,
    val operation: ast.OperationDefinition,
    val tpe: ObjectType[Ctx, Root, F],
    val userContext: Ctx,
    val root: Root,
    val fields: Seq[PreparedField[Ctx, Root, F]],
    execFn: (Ctx, Root, ResultMarshaller, ExecutionScheme) => Any) {
  def execute(userContext: Ctx = userContext, root: Root = root)(implicit
      marshaller: ResultMarshaller,
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    execFn(userContext, root, marshaller, scheme).asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
}

case class PreparedField[Ctx, Root, F[_]](field: Field[Ctx, Root, F], args: Args)
