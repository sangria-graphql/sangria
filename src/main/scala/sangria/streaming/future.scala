package sangria.streaming

import language.higherKinds

import scala.concurrent.{ExecutionContext, Future}

object future {
  class FutureSubscriptionStream(implicit ec: ExecutionContext) extends SubscriptionStream[Future] {
    def supported[T[_]](other: SubscriptionStream[T]) = other.isInstanceOf[FutureSubscriptionStream]

    def map[A, B](source: Future[A])(fn: A ⇒ B) = source.map(fn)

    def singleFuture[T](value: Future[T]) = value

    def single[T](value: T) = Future.successful(value)

    def mapFuture[A, B](source: Future[A])(fn: A ⇒ Future[B]) =
      source.flatMap(fn)

    def first[T](s: Future[T]) = s

    def failed[T](e: Throwable) = Future.failed(e)

    def onComplete[Ctx, Res](result: Future[Res])(op: ⇒ Unit) =
      result
        .map {x ⇒ op; x}
        .recover {case e ⇒ op; throw e}

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T ⇒ Future[Res]) =
      future flatMap resultFn

    def merge[T](streams: Vector[Future[T]]) = Future.firstCompletedOf(streams)
  }

  implicit def futureSubscriptionStream(implicit ec: ExecutionContext) = new FutureSubscriptionStream
}
