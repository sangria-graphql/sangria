package sangria.catseffect.schema

import cats.effect.Async
import sangria.schema.{Action, Context, LeafAction}

import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

case class AsyncValue[Ctx, Val, F[_]: Async](value: F[Val]) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): AsyncValue[Ctx, NewVal, F] =
    new AsyncValue(Async[F].map(value)(fn))
}

object AsyncResolver {
  implicit def asyncToAction[Ctx, Val, Res, F[_]: Async](
      resolver: Context[Ctx, Val] => F[Res]): Context[Ctx, Val] => Action[Ctx, Res] = { context =>
    AsyncValue(resolver(context))
  }
}

object AsyncValue {
  implicit def asyncAction[Ctx, Val, F[_]: Async](value: F[Val]): LeafAction[Ctx, Val] =
    AsyncValue(value)
}
