package sangria.execution

import scala.concurrent.Future

trait FutureToF[F[_]] {
  def liftFuture[A](future: => Future[A]): F[A]
}

object FutureToF {
  import cats.effect.Async
  def default[F[_]: Async]: FutureToF[F] = new FutureToF[F] {
    override def liftFuture[A](future: => Future[A]): F[A] =
      Async[F].fromFuture(Async[F].delay(future))
  }
}
