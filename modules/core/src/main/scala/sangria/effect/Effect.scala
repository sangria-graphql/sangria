package sangria.effect

import scala.collection.BuildFrom
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

trait DeferredEffect[F[_], A] {
  def get: F[A]
  def complete(a: Try[A]): Unit
  def completeWith(other: F[A]): Unit
  def success(a: A): Unit = complete(Success(a))
  def failure(cause: Throwable): Unit = complete(Failure(cause))
}

trait Effect[F[_]] {
  def pure[A](a: A): F[A]
  def failed[T](exception: Throwable): F[T]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  def sequence[A, CC[X] <: IterableOnce[X], To](vector: CC[F[A]])(implicit
      bf: BuildFrom[CC[F[A]], A, To]): F[To]
  def recover[A, U >: A](fa: F[A])(pf: PartialFunction[Throwable, U]): F[U]
  def onComplete[A, B](fa: F[A])(f: Try[A] => B): Unit
  def foreach[A, B](fa: F[A])(f: A => B): Unit = onComplete(fa)(tryA => tryA.foreach(f))
  def toFuture[A](fa: F[A]): Future[A]

  def newDeferred[A](): DeferredEffect[F, A]
}

object Effect {
  def apply[F[_]: Effect](): Effect[F] = implicitly[Effect[F]]

  implicit class Ops[F[_], A](value: F[A])(implicit effect: Effect[F]) {
    def map[B](f: A => B): F[B] = effect.map(value)(f)
    def flatMap[B](f: A => F[B]): F[B] = effect.flatMap(value)(f)
    def foreach[B](f: A => B): Unit = effect.foreach(value)(f)
    def recover[U >: A](pf: PartialFunction[Throwable, U]): F[U] = effect.recover[A, U](value)(pf)
  }

  implicit def FutureEffect(implicit
      ec: ExecutionContext = ExecutionContext.Implicits.global): Effect[Future] =
    new Effect[Future] {
      override def pure[A](a: A): Future[A] = Future.successful(a)
      override def failed[T](exception: Throwable): Future[T] = Future.failed(exception)
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
      override def flatMap[A, B](fa: Future[A])(f: A => Future[B]): Future[B] = fa.flatMap(f)
      override def sequence[A, CC[X] <: IterableOnce[X], To](vector: CC[Future[A]])(implicit
          bf: BuildFrom[CC[Future[A]], A, To]): Future[To] =
        Future.sequence(vector)
      override def recover[A, U >: A](fa: Future[A])(pf: PartialFunction[Throwable, U]): Future[U] =
        fa.recover(pf)
      override def onComplete[A, B](fa: Future[A])(f: Try[A] => B): Unit = fa.onComplete(f)
      override def toFuture[A](fa: Future[A]): Future[A] = fa

      override def newDeferred[A](): DeferredEffect[Future, A] = new DeferredEffect[Future, A] {
        private val p = Promise[A]()
        override def get: Future[A] = p.future
        override def complete(a: Try[A]): Unit = p.complete(a)
        override def completeWith(other: Future[A]): Unit = p.completeWith(other)
      }
    }

}
