package sangria.execution

import sangria.streaming.SubscriptionStream

import scala.concurrent.{ExecutionContext, Future}

trait ExecutionScheme {
  type Result[Ctx, Res]

  def failed[Ctx, Res](error: Throwable): Result[Ctx, Res]
  def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: => Unit)(implicit
      ec: ExecutionContext): Result[Ctx, Res]
  def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => Result[Ctx, Res])(implicit
      ec: ExecutionContext): Result[Ctx, Res]
  def extended: Boolean
  val resolverBuilder: ResolverBuilder
}

object ExecutionScheme extends AlternativeExecutionScheme {
  implicit object Default extends ExecutionScheme {
    type Result[Ctx, Res] = Future[Res]

    def failed[Ctx, Res](error: Throwable): Result[Ctx, Res] =
      Future.failed(error)

    def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: => Unit)(implicit
        ec: ExecutionContext): Result[Ctx, Res] =
      result
        .map { x => op; x }
        .recover { case e => op; throw e }

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => Result[Ctx, Res])(implicit
        ec: ExecutionContext): Result[Ctx, Res] =
      future.flatMap(resultFn)

    def extended = false

    override val resolverBuilder: ResolverBuilder = FutureResolverBuilder
  }
}

trait AlternativeExecutionScheme {
  trait StreamBasedExecutionScheme[S[_]] {
    def subscriptionStream: SubscriptionStream[S]
  }

  implicit object Extended extends ExecutionScheme {
    type Result[Ctx, T] = Future[ExecutionResult[Ctx, T]]

    def failed[Ctx, Res](error: Throwable): Result[Ctx, Res] =
      Future.failed(error)

    def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: => Unit)(implicit
        ec: ExecutionContext): Result[Ctx, Res] =
      result
        .map { x => op; x }
        .recover { case e => op; throw e }

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => Result[Ctx, Res])(implicit
        ec: ExecutionContext): Result[Ctx, Res] =
      future.flatMap(resultFn)

    def extended = true

    override val resolverBuilder: ResolverBuilder = FutureResolverBuilder
  }

  implicit def Stream[S[_]](implicit
      stream: SubscriptionStream[S]): ExecutionScheme with StreamBasedExecutionScheme[S] {
    type Result[Ctx, T] = S[T]
  } =
    new ExecutionScheme with StreamBasedExecutionScheme[S] {
      type Result[Ctx, T] = S[T]

      def subscriptionStream = stream
      def extended = false

      def failed[Ctx, Res](error: Throwable): Result[Ctx, Res] =
        stream.failed(error)

      def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: => Unit)(implicit
          ec: ExecutionContext): Result[Ctx, Res] =
        stream.onComplete(result)(op)

      def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => Result[Ctx, Res])(implicit
          ec: ExecutionContext): Result[Ctx, Res] =
        stream.flatMapFuture(future)(resultFn)

      override val resolverBuilder: ResolverBuilder = FutureResolverBuilder
    }

  implicit def StreamExtended[S[_]](implicit
      stream: SubscriptionStream[S]): ExecutionScheme with StreamBasedExecutionScheme[S] {
    type Result[Ctx, T] = S[ExecutionResult[Ctx, T]]
  } =
    new ExecutionScheme with StreamBasedExecutionScheme[S] {
      type Result[Ctx, T] = S[ExecutionResult[Ctx, T]]

      def subscriptionStream = stream
      def extended = true

      def failed[Ctx, Res](error: Throwable): Result[Ctx, Res] =
        stream.failed(error)

      def onComplete[Ctx, Res](result: Result[Ctx, Res])(op: => Unit)(implicit
          ec: ExecutionContext): Result[Ctx, Res] =
        stream.onComplete(result)(op)

      def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => Result[Ctx, Res])(implicit
          ec: ExecutionContext): Result[Ctx, Res] =
        stream.flatMapFuture(future)(resultFn)

      override val resolverBuilder: ResolverBuilder = FutureResolverBuilder
    }
}

case class ExecutionResult[Ctx, Res](
    ctx: Ctx,
    result: Res,
    errors: Vector[RegisteredError],
    middlewareVals: List[(Any, Middleware[_])],
    validationTiming: TimeMeasurement,
    queryReducerTiming: TimeMeasurement)
