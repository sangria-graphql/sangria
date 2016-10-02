package sangria.execution

import scala.concurrent.Future

sealed trait ExecutionScheme {
  type Result[Ctx, Res]
}

object ExecutionScheme extends AlternativeExecutionScheme {
  implicit object Default extends ExecutionScheme {
    type Result[Ctx, Res] = Future[Res]
  }
}

trait AlternativeExecutionScheme {
  implicit object Full extends ExecutionScheme {
    type Result[Ctx, T] = Future[ExecutionResult[Ctx, T]]
  }
}

case class ExecutionResult[Ctx, Res](
  ctx: Ctx,
  result: Res,
  errors: Vector[RegisteredError],
  middlewareVals: List[(Any, Middleware[_])])