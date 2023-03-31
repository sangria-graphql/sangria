package sangria.catseffect.execution

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import sangria.execution.{AsyncExecutionScheme, AsyncToFuture}

import scala.concurrent.{ExecutionContext, Future}

/** Prepare an [[sangria.execution.ExecutionScheme]] for [[IO]]. If you want to use another effect,
  * use the same bricks to build your own.
  */
object IOExecutionScheme {
  // sangria is using an implicit ExecutionContext at different places.
  // For the moment, we need to expose one.
  implicit val ec: ExecutionContext = global.compute

  // ideally we would need only this.
  implicit val asyncExecutionScheme: AsyncExecutionScheme[IO] =
    new AsyncExecutionScheme[IO](new AsyncToFuture[IO] {
      override def toFuture[A](f: IO[A]): Future[A] = f.unsafeToFuture()
    })
}
