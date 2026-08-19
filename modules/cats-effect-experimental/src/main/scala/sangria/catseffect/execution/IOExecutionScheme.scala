package sangria.catseffect.execution

import cats.effect.IO
import sangria.execution.AsyncExecutionScheme

/** Prepare an [[sangria.execution.ExecutionScheme]] for [[IO]]. If you want to use another effect,
  * use the same bricks to build your own.
  */
object IOExecutionScheme {
  implicit val asyncExecutionScheme: AsyncExecutionScheme[IO] = new AsyncExecutionScheme[IO]
}
