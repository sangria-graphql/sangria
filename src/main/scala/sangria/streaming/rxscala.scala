package sangria.streaming

import language.higherKinds
import rx.lang.scala.Observable

import scala.concurrent.{ExecutionContext, Future, Promise}

object rxscala {
  class ObservableSubscriptionStream(implicit ec: ExecutionContext) extends SubscriptionStream[Observable] {
    def supported[T[_]](other: SubscriptionStream[T]) = other.isInstanceOf[ObservableSubscriptionStream]

    def map[A, B](source: Observable[A])(fn: A ⇒ B) = source.map(fn)

    def singleFuture[T](value: Future[T]) =
      Observable.from(value)

    def single[T](value: T) = Observable.just(value)

    def mapFuture[A, B](source: Observable[A])(fn: A ⇒ Future[B]) =
      source.flatMap(a ⇒ Observable.from(fn(a)))

    def first[T](s: Observable[T]) = {
      val promise = Promise[T]()

      s.take(1).subscribe(
        t ⇒ promise.success(t),
        e ⇒ promise.failure(e),
        () ⇒ {
          if (!promise.isCompleted)
            promise.failure(new IllegalStateException("Promise was not completed - observable haven't produced any elements."))
        }
      )

      promise.future
    }

    def failed[T](e: Throwable) = Observable.error(e)

    def onComplete[Ctx, Res](result: Observable[Res])(op: ⇒ Unit) =
      result.doAfterTerminate(op)

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T ⇒ Observable[Res]) =
      Observable.from(future).flatMap(resultFn)

    def merge[T](streams: Vector[Observable[T]]) =
      if (streams.size > 1)
        streams.tail.foldLeft(streams.head){case (acc, e) ⇒ acc merge e}
      else if (streams.nonEmpty)
        streams.head
      else
        throw new IllegalStateException("No streams produced!")
  }

  implicit def observableSubscriptionStream(implicit ec: ExecutionContext): SubscriptionStream[Observable] =
    new ObservableSubscriptionStream
}
