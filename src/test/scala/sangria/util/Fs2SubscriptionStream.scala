package sangria.util

import cats.effect.{ContextShift, IO}
import fs2.Stream
import sangria.streaming.SubscriptionStream
import scala.concurrent.Future
import scala.language.higherKinds

object Fs2Support {
  type IOS[A] = Stream[IO, A]

  class Fs2SubscriptionStream(implicit CS: ContextShift[IO]) extends SubscriptionStream[IOS] {
    def supported[T[_]](other: SubscriptionStream[T]) = other.isInstanceOf[Fs2SubscriptionStream]

    def map[A, B](source: IOS[A])(fn: A => B) = source.map(fn)

    def singleFuture[T](value: Future[T]) =
      Stream.eval(IO.fromFuture(IO(value)))

    def single[T](value: T) = Stream.emit(value)

    def mapFuture[A, B](source: IOS[A])(fn: A => Future[B]) =
      source.evalMap(a => IO.fromFuture(IO(fn(a))))

    def first[T](s: IOS[T]) =
      s.compile.toVector.map(_.head).unsafeToFuture

    def failed[T](e: Throwable) = Stream.raiseError[IO](e)

    def onComplete[Ctx, Res](result: IOS[Res])(op: => Unit) =
      result.onFinalize(IO(op))

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => IOS[Res]) =
      Stream.eval(IO.fromFuture(IO(future))).flatMap(resultFn)

    def merge[T](streams: Vector[IOS[T]]) =
      if (streams.nonEmpty)
        streams.tail.foldLeft(streams.head)(_.merge(_))
      else
        throw new IllegalStateException("No streams produced!")

    def recover[T](stream: IOS[T])(fn: Throwable => T) =
      stream.handleErrorWith { case e => Stream.emit(fn(e)) }
  }

  implicit def observableSubscriptionStream(implicit CS: ContextShift[IO]): SubscriptionStream[IOS] =
    new Fs2SubscriptionStream
}
