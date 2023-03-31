package sangria.execution

import cats.effect.Async

import scala.concurrent.{ExecutionContext, Future}

/** An [[ExecutionScheme]] that is capable of using [[sangria.catseffect.schema.AsyncValue]].
  *
  * Its result is an [[Async]].
  */
class AsyncExecutionScheme[F[_]: Async](
    val asyncToFuture: AsyncToFuture[F]
) extends ExecutionScheme {
  private val asyncF: Async[F] = Async[F]

  override type Result[Ctx, Res] = F[Res]

  override def failed[Ctx, Res](error: Throwable): Result[Ctx, Res] = asyncF.raiseError(error)

  override def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: => Unit)(implicit
      ec: ExecutionContext): Result[Ctx, Res] =
    asyncF.flatMap(result)(r => asyncF.map(asyncF.delay(op))(_ => r))

  override def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => Result[Ctx, Res])(
      implicit ec: ExecutionContext): Result[Ctx, Res] =
    asyncF.flatMap(asyncF.fromFuture(asyncF.delay(future)))(resultFn)

  override val resolverBuilder: ResolverBuilder = new AsyncResolverBuilder[F](asyncToFuture)

  override def extended: Boolean = false
}
