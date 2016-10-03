package sangria.streaming

import language.higherKinds
import akka.NotUsed
import akka.stream.Materializer
import akka.stream.scaladsl.{Merge, Sink, Source}
import sangria.schema.{Action, ValidOutType}

import scala.concurrent.Future

object akkaStreams {
  class AkkaStreamsSubscriptionStream(implicit materializer: Materializer) extends SubscriptionStream[AkkaSource] {
    def supported[T[_]](other: SubscriptionStream[T]) = other.isInstanceOf[AkkaStreamsSubscriptionStream]

    def map[A, B](source: AkkaSource[A])(fn: A ⇒ B) = source.map(fn)

    def singleFuture[T](value: Future[T]) = Source.fromFuture(value)

    def single[T](value: T) = Source.single(value)

    def mapFuture[A, B](source: AkkaSource[A])(fn: A ⇒ Future[B]) =
      source.mapAsync(1)(fn)

    def first[T](s: AkkaSource[T]) = s.runWith(Sink.head)

    def failed[T](e: Throwable) = Source.failed(e).asInstanceOf[AkkaSource[T]]

    def onComplete[Ctx, Res](result: AkkaSource[Res])(op: ⇒ Unit) =
      result
        .map {x ⇒ op; x}
        .recover {case e ⇒ op; throw e}
        .asInstanceOf[AkkaSource[Res]]

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T ⇒ AkkaSource[Res]) =
      Source.fromFuture(future).flatMapMerge(1, resultFn)

    def merge[T](streams: Vector[AkkaSource[T]]) = {
      if (streams.size > 1)
        Source.combine(streams(0), streams(1), streams.drop(2): _*)(Merge(_))
      else if (streams.nonEmpty)
        streams.head
      else
        throw new IllegalStateException("No streams produced!")
    }
  }

  type AkkaSource[+T] = Source[T, NotUsed]

  implicit def akkaSubscriptionStream(implicit materializer: Materializer): SubscriptionStream[AkkaSource] = new AkkaStreamsSubscriptionStream

  implicit def akkaStreamIsValidSubscriptionStream[A[_, _], Ctx, Res, Out](implicit materializer: Materializer, ev1: ValidOutType[Res, Out]): SubscriptionStreamLike[Source[A[Ctx, Res], NotUsed], A, Ctx, Res, Out] =
    new SubscriptionStreamLike[Source[A[Ctx, Res], NotUsed], A, Ctx, Res, Out] {
      type StreamSource[X] = AkkaSource[X]
      val subscriptionStream = new AkkaStreamsSubscriptionStream
    }
}
