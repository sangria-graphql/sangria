package sangria.util

import cats.effect.{ContextShift, IO}
import fs2.Stream
import sangria.streaming.SubscriptionStream
import scala.util.{Success, Failure}
import com.twitter.util.{Future, Promise}

object Fs2Support {
  type IOS[A] = Stream[IO, A]

  import FutureTransform._
  implicit val ec: scala.concurrent.ExecutionContext = scala.concurrent.ExecutionContext.global

  class Fs2SubscriptionStream(implicit CS: ContextShift[IO]) extends SubscriptionStream[IOS] {
    def supported[T[_]](other: SubscriptionStream[T]) = other.isInstanceOf[Fs2SubscriptionStream]

    def map[A, B](source: IOS[A])(fn: A => B) = source.map(fn)

    def singleFuture[T](value: Future[T]) =
      Stream.eval(IO.fromFuture(IO(value.asScala)))

    def single[T](value: T) = Stream.emit(value)

    def mapFuture[A, B](source: IOS[A])(fn: A => Future[B]) =
      source.evalMap(a => IO.fromFuture(IO(fn(a).asScala)))

    def first[T](s: IOS[T]) =
      s.compile.toVector.map(_.head).unsafeToFuture().asTwitter

    def failed[T](e: Throwable) = Stream.raiseError[IO](e)

    def onComplete[Ctx, Res](result: IOS[Res])(op: => Unit) =
      result.onFinalize(IO(op))

    def flatMapFuture[Ctx, Res, T](future: Future[T])(resultFn: T => IOS[Res]) =
      Stream.eval(IO.fromFuture(IO(future.asScala))).flatMap(resultFn)

    def merge[T](streams: Vector[IOS[T]]) =
      if (streams.nonEmpty)
        streams.tail.foldLeft(streams.head)(_.merge(_))
      else
        throw new IllegalStateException("No streams produced!")

    def recover[T](stream: IOS[T])(fn: Throwable => T) =
      stream.handleErrorWith { case e => Stream.emit(fn(e)) }
  }

  implicit def observableSubscriptionStream(implicit
      CS: ContextShift[IO]): SubscriptionStream[IOS] =
    new Fs2SubscriptionStream
}

object FutureTransform {
  import com.twitter.util.{Future => TwitterFuture, Promise => TwitterPromise, Return, Throw}
  import scala.concurrent.{Future => ScalaFuture, Promise => ScalaPromise, ExecutionContext}
  import scala.util.{Success, Failure}

  /** Convert from a Twitter Future to a Scala Future */
  implicit class RichTwitterFuture[A](val tf: TwitterFuture[A]) extends AnyVal {
    def asScala: ScalaFuture[A] = {
      val promise: ScalaPromise[A] = ScalaPromise()
      tf.respond {
        case Return(value) => promise.success(value)
        case Throw(exception) => promise.failure(exception)
      }
      promise.future
    }
  }

  /** Convert from a Scala Future to a Twitter Future */
  implicit class RichScalaFuture[A](val sf: ScalaFuture[A]) extends AnyVal {
    def asTwitter(implicit e: ExecutionContext): TwitterFuture[A] = {
      val promise: TwitterPromise[A] = new TwitterPromise[A]()
      sf.onComplete {
        case Success(value) => promise.setValue(value)
        case Failure(exception) => promise.setException(exception)
      }
      promise
    }
  }
}
