package sangria.execution

import cats.effect.Async
import sangria.ast
import sangria.ast.{Document, SourceMapper}
import sangria.catseffect.schema.AsyncValue
import sangria.execution.deferred.DeferredResolver
import sangria.marshalling.ResultMarshaller
import sangria.schema._

import scala.concurrent.{ExecutionContext, Future}

/** The [[AsyncResolver]] is using the [[FutureResolver]] under the hood. So we need a way to
  * transform our [[Async]] into a [[Future]] for now.
  */
trait AsyncToFuture[F[_]] {
  def toFuture[A](f: F[A]): Future[A]
}

private[execution] class AsyncResolverBuilder[F[_]: Async](asyncToFuture: AsyncToFuture[F])
    extends ResolverBuilder {
  override def build[Ctx](
      marshaller: ResultMarshaller,
      middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
      schema: Schema[Ctx, _],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, VariableValue],
      fieldCollector: FieldCollector[Ctx, _],
      userContext: Ctx,
      exceptionHandler: ExceptionHandler,
      deferredResolver: DeferredResolver[Ctx],
      sourceMapper: Option[SourceMapper],
      deprecationTracker: Option[DeprecationTracker],
      middleware: List[ApplyedMiddleware[Ctx, Middleware[Ctx]]],
      beforeFieldMiddlewares: List[ApplyedMiddleware[Ctx, MiddlewareBeforeField[Ctx]]],
      maxQueryDepth: Option[Int],
      deferredResolverState: Any,
      preserveOriginalErrors: Boolean,
      validationTiming: TimeMeasurement,
      queryReducerTiming: TimeMeasurement,
      queryAst: Document)(implicit executionContext: ExecutionContext): Resolver[Ctx] =
    new AsyncResolver[Ctx, F](
      marshaller,
      middlewareCtx,
      schema,
      valueCollector,
      variables,
      fieldCollector,
      userContext,
      exceptionHandler,
      deferredResolver,
      sourceMapper,
      deprecationTracker,
      middleware,
      beforeFieldMiddlewares,
      maxQueryDepth,
      deferredResolverState,
      preserveOriginalErrors,
      validationTiming,
      queryReducerTiming,
      queryAst,
      asyncToFuture
    )
}

/** The [[Resolver]] that is used to resolve [[AsyncValue]].
  *
  * For now, it's using the [[FutureResolver]] under the hood. Later, we can update its
  * implementation to avoid using any [[Future]].
  */
private[execution] class AsyncResolver[Ctx, F[_]: Async](
    val marshaller: ResultMarshaller,
    middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
    schema: Schema[Ctx, _],
    valueCollector: ValueCollector[Ctx, _],
    variables: Map[String, VariableValue],
    fieldCollector: FieldCollector[Ctx, _],
    userContext: Ctx,
    exceptionHandler: ExceptionHandler,
    deferredResolver: DeferredResolver[Ctx],
    sourceMapper: Option[SourceMapper],
    deprecationTracker: Option[DeprecationTracker],
    middleware: List[ApplyedMiddleware[Ctx, Middleware[Ctx]]],
    beforeFieldMiddlewares: List[ApplyedMiddleware[Ctx, MiddlewareBeforeField[Ctx]]],
    maxQueryDepth: Option[Int],
    deferredResolverState: Any,
    preserveOriginalErrors: Boolean,
    validationTiming: TimeMeasurement,
    queryReducerTiming: TimeMeasurement,
    queryAst: ast.Document,
    asyncToFuture: AsyncToFuture[F]
)(implicit executionContext: ExecutionContext)
    extends Resolver[Ctx] {

  private val asyncF: Async[F] = Async[F]

  private val delegate = new FutureResolver[Ctx](
    marshaller,
    middlewareCtx,
    schema,
    valueCollector,
    variables,
    fieldCollector,
    userContext,
    exceptionHandler,
    deferredResolver,
    sourceMapper,
    deprecationTracker,
    middleware,
    beforeFieldMiddlewares,
    maxQueryDepth,
    deferredResolverState,
    preserveOriginalErrors,
    validationTiming,
    queryReducerTiming,
    queryAst
  ) { del =>
    override protected def resolveLeafAction(
        path: ExecutionPath,
        tpe: ObjectType[Ctx, _],
        userCtx: Ctx,
        astFields: Vector[ast.Field],
        field: Field[Ctx, _],
        updateCtx: Option[MappedCtxUpdate[Ctx, Any, Any]])(
        action: LeafAction[Ctx, Any]): (ast.Field, Resolve) =
      action match {
        case a: AsyncValue[Ctx, Any, F] =>
          val f = asyncToFuture.toFuture[Any](a.value)
          super.resolveFutureValue(path, tpe, userCtx, astFields, field, updateCtx)(FutureValue(f))

        case action: StandardLeafAction[Ctx, Any] =>
          super.resolveStandardLeafAction(path, tpe, userCtx, astFields, field, updateCtx)(action)

        case other => unresolvableLeafAction(path, tpe, astFields, updateCtx)(other)
      }

    override protected def handleScheme(
        result: Future[((Vector[RegisteredError], del.marshaller.Node), Ctx)],
        scheme: ExecutionScheme): scheme.Result[Ctx, del.marshaller.Node] =
      scheme match {
        case s: AsyncExecutionScheme[F] =>
          val r: F[((Vector[RegisteredError], del.marshaller.Node), Ctx)] =
            asyncF.fromFuture(asyncF.delay(result))
          handleSchemeF(r, s).asInstanceOf[scheme.Result[Ctx, del.marshaller.Node]]

        case _ =>
          super.handleScheme(result, scheme)
      }

    override protected def resolveStandardFieldResolutionSeqInner(
        path: ExecutionPath,
        uc: Ctx,
        tpe: ObjectType[Ctx, _],
        origField: ast.Field,
        fields: Vector[ast.Field],
        result: LeafAction[Ctx, Any],
        sfield: Field[Ctx, _],
        fieldPath: ExecutionPath,
        resolveUc: Any => Ctx,
        resolveError: Throwable => Throwable,
        resolveVal: Any => Any): Future[(Resolve, Ctx)] =
      result match {
        case a: AsyncValue[Ctx, Any, F] =>
          val f = asyncToFuture.toFuture[Any](a.value)
          super.resolveStandardFieldResolutionSeqInner(
            path,
            uc,
            tpe,
            origField,
            fields,
            FutureValue(f),
            sfield,
            fieldPath,
            resolveUc,
            resolveError,
            resolveVal)
        case other =>
          super.resolveStandardFieldResolutionSeqInner(
            path,
            uc,
            tpe,
            origField,
            fields,
            other,
            sfield,
            fieldPath,
            resolveUc,
            resolveError,
            resolveVal)
      }

    private def handleSchemeF(
        result: F[((Vector[RegisteredError], del.marshaller.Node), Ctx)],
        scheme: AsyncExecutionScheme[F]): scheme.Result[Ctx, del.marshaller.Node] =
      asyncF.map(result) { case ((_, res), _) => res }
  }

  override def resolveFieldsPar(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    delegate
      .resolveFieldsPar(tpe, value, fields)(scheme)
      .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

  override def resolveFieldsSeq(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    delegate
      .resolveFieldsSeq(tpe, value, fields)(scheme)
      .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]

  override def resolveFieldsSubs(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node] =
    delegate
      .resolveFieldsSubs(tpe, value, fields)(scheme)
      .asInstanceOf[scheme.Result[Ctx, marshaller.Node]]
}
