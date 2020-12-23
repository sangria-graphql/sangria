package sangria.effect

import scala.concurrent.{ExecutionContext, Future}

trait Effect[F[_]] {
  def pure[A](a: A): F[A]
  def failed[T](exception: Throwable): F[T]
  def map[A, B](fa: F[A])(f: A => B): F[B]
  def sequence[A](vector: Vector[F[A]]): F[Vector[A]]
  def recover[A, U >: A](fa: F[A])(pf: PartialFunction[Throwable, U]): F[U]
}

object Effect {
  def apply[F[_]: Effect](): Effect[F] = implicitly[Effect[F]]

  implicit class Ops[F[_], A](value: F[A])(implicit effect: Effect[F]) {
    def map[B](f: A => B): F[B] = effect.map(value)(f)
    def recover[U >: A](pf: PartialFunction[Throwable, U]): F[U] = effect.recover[A, U](value)(pf)
  }

  implicit def FutureEffect(implicit
      ec: ExecutionContext = ExecutionContext.Implicits.global): Effect[Future] =
    new Effect[Future] {
      override def pure[A](a: A): Future[A] = Future.successful(a)
      override def failed[T](exception: Throwable): Future[T] = Future.failed(exception)
      override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
      override def sequence[A](vector: Vector[Future[A]]): Future[Vector[A]] =
        Future.sequence(vector)
      override def recover[A, U >: A](fa: Future[A])(pf: PartialFunction[Throwable, U]): Future[U] =
        fa.recover(pf)
    }

}
