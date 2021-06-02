package sangria.streaming

import scala.language.higherKinds

import com.twitter.util.Future

object future {
  class FutureSubscriptionStream extends SubscriptionStream[Future] {
    def supported[T[_]](other: SubscriptionStream[T]): Boolean =
      other.isInstanceOf[FutureSubscriptionStream]

    def map[A, B](source: Future[A])(fn: A => B): Future[B] = source.map(fn)

    def singleFuture[T](value: Future[T]): Future[T] = value

    def single[T](value: T): Future[T] = Future.value(value)

    def mapFuture[A, B](source: Future[A])(fn: A => Future[B]): Future[B] =
      source.flatMap(fn)

    def first[T](s: Future[T]): Future[T] = s

    def failed[T](e: Throwable): Future[T] = Future.exception(e)

    def onComplete[Ctx, Res](result: Future[Res])(op: => Unit): Future[Res] =
      result
        .map { x => op; x }
        .handle { case e => op; throw e }

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => Future[Res]): Future[Res] =
      future.flatMap(resultFn)

    def merge[T](streams: Vector[Future[T]]): Future[T] = {
      Future.select(streams).flatMap { case (res, _) =>
        Future.const(res)
      }
    }

    def recover[T](stream: Future[T])(fn: Throwable => T): Future[T] =
      stream.handle { case e => fn(e) }
  }

  implicit def futureSubscriptionStream: FutureSubscriptionStream =
    new FutureSubscriptionStream
}
