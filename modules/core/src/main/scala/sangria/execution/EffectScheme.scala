package sangria.execution

import scala.collection.BuildFrom
import scala.concurrent.{ExecutionContext, Future}

trait EffectScheme {
  type F[_]
  def map[A, B](eff: F[A])(f: A => B): F[B]
  def flatMap[A, B](eff: F[A])(f: A => F[B]): F[B]
  def success[A](a: A): F[A]
  def sequence[A, CC[X] <: IterableOnce[X]](in: CC[F[A]])(implicit
      bf: BuildFrom[CC[F[A]], A, CC[A]]): F[CC[A]]
  def fromFuture[A](f: Future[A]): F[A]
  def toFuture[A](f: F[A]): Future[A]
}

class FutureEffectScheme(implicit ec: ExecutionContext) extends EffectScheme {
  override type F[A] = Future[A]

  override def map[A, B](eff: Future[A])(f: A => B): Future[B] = eff.map(f)
  override def flatMap[A, B](eff: Future[A])(f: A => Future[B]): Future[B] = eff.flatMap(f)
  override def success[A](a: A): Future[A] = Future.successful(a)
  def sequence[A, CC[X] <: IterableOnce[X]](in: CC[Future[A]])(implicit
      bf: BuildFrom[CC[Future[A]], A, CC[A]]): Future[CC[A]] =
    Future.sequence(in)
  override def fromFuture[A](f: Future[A]): Future[A] = f
  override def toFuture[A](f: Future[A]): Future[A] = f
}
