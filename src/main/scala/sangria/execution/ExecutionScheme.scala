package sangria.execution

import scala.concurrent.{ExecutionContext, Future}

sealed trait ExecutionScheme {
  type Result[Ctx, Res]

  def failed[Ctx, Res](error: Throwable): Result[Ctx, Res]
  def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: ⇒ Unit)(implicit ec: ExecutionContext): Result[Ctx, Res]
  def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T ⇒ Result[Ctx, Res])(implicit ec: ExecutionContext): Result[Ctx, Res]
}

object ExecutionScheme extends AlternativeExecutionScheme {
  implicit object Default extends ExecutionScheme {
    type Result[Ctx, Res] = Future[Res]

    def failed[Ctx, Res](error: Throwable): Result[Ctx, Res] =
      Future.failed(error)

    def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: ⇒ Unit)(implicit ec: ExecutionContext): Result[Ctx, Res] =
      result
        .map {x ⇒ op; x}
        .recover {case e ⇒ op; throw e}

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T ⇒ Result[Ctx, Res])(implicit ec: ExecutionContext): Result[Ctx, Res] =
      future flatMap resultFn
  }
}

trait AlternativeExecutionScheme {
  implicit object FullResult extends ExecutionScheme {
    type Result[Ctx, T] = Future[ExecutionResult[Ctx, T]]

    def failed[Ctx, Res](error: Throwable): Result[Ctx, Res] =
      Future.failed(error)

    def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: ⇒ Unit)(implicit ec: ExecutionContext): Result[Ctx, Res] =
      result
        .map { x ⇒ op; x}
        .recover { case e ⇒ op; throw e}

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T ⇒ Result[Ctx, Res])(implicit ec: ExecutionContext): Result[Ctx, Res] =
      future flatMap resultFn
  }
}

case class ExecutionResult[Ctx, Res](
  ctx: Ctx,
  result: Res,
  errors: Vector[RegisteredError],
  middlewareVals: List[(Any, Middleware[_])])