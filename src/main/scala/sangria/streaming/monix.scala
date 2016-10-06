package sangria.streaming

import language.higherKinds

import _root_.monix.reactive._
import _root_.monix.execution.Scheduler

import scala.concurrent.Future

object monix {
  class ObservableSubscriptionStream(implicit scheduler: Scheduler) extends SubscriptionStream[Observable] {
    def supported[T[_]](other: SubscriptionStream[T]) = other.isInstanceOf[ObservableSubscriptionStream]

    def map[A, B](source: Observable[A])(fn: A ⇒ B) = source.map(fn)

    def singleFuture[T](value: Future[T]) =
      Observable.fromFuture(value)

    def single[T](value: T) = Observable(value)

    def mapFuture[A, B](source: Observable[A])(fn: A ⇒ Future[B]) =
      source.mergeMap(a ⇒ Observable.fromFuture(fn(a)))

    def first[T](s: Observable[T]) =
      s.firstOrElseL(throw new IllegalStateException("Promise was not completed - observable haven't produced any elements.")).runAsync

    def failed[T](e: Throwable) = Observable.raiseError(e)

    def onComplete[Ctx, Res](result: Observable[Res])(op: ⇒ Unit) =
      result.doOnTerminate(op)

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T ⇒ Observable[Res]) =
      Observable.fromFuture(future).mergeMap(resultFn)

    def merge[T](streams: Vector[Observable[T]]) =
      if (streams.nonEmpty)
        Observable.merge(streams: _*)
      else
        throw new IllegalStateException("No streams produced!")

    def recover[T](stream: Observable[T])(fn: Throwable ⇒ T) =
      stream onErrorRecover {case e ⇒ fn(e)}
  }

  implicit def observableSubscriptionStream(implicit scheduler: Scheduler): SubscriptionStream[Observable] =
    new ObservableSubscriptionStream
}
