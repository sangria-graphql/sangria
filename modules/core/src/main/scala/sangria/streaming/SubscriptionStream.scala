package sangria.streaming

import scala.language.higherKinds

import scala.annotation.implicitNotFound
import com.twitter.util.Future

@implicitNotFound(
  msg =
    "Can't find suitable `SubscriptionStream` type-class instance for type `${StreamSource}`. " +
      "If you have defined it already, please consider defining an implicit instance `SubscriptionStream[${StreamSource}]`. " +
      "It's also possible that you need to import some executor (like `ExecutionContext`) to make `SubscriptionStream` " +
      "available for `${StreamSource}`.")
trait SubscriptionStream[StreamSource[_]] {
  def supported[T[X]](other: SubscriptionStream[T]): Boolean

  def single[T](value: T): StreamSource[T]
  def singleFuture[T](value: Future[T]): StreamSource[T]
  def first[T](s: StreamSource[T]): Future[T]
  def failed[T](e: Throwable): StreamSource[T]
  def onComplete[Ctx, Res](result: StreamSource[Res])(op: => Unit): StreamSource[Res]
  def flatMapFuture[Ctx, Res, T](future: Future[T])(
      resultFn: T => StreamSource[Res]): StreamSource[Res]
  def mapFuture[A, B](source: StreamSource[A])(fn: A => Future[B]): StreamSource[B]
  def map[A, B](source: StreamSource[A])(fn: A => B): StreamSource[B]
  def merge[T](streams: Vector[StreamSource[T]]): StreamSource[T]
  def recover[T](stream: StreamSource[T])(fn: Throwable => T): StreamSource[T]
}
