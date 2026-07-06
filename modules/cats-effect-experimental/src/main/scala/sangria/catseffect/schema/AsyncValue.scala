package sangria.catseffect.schema

import cats.effect.Async
import sangria.macros.derive.GraphQLOutputTypeLookup
import sangria.schema.{LeafAction, OutputType}

import scala.concurrent.ExecutionContext
import scala.language.implicitConversions

case class AsyncValue[Ctx, Val, F[_]: Async](value: F[Val]) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): AsyncValue[Ctx, NewVal, F] =
    new AsyncValue(Async[F].map(value)(fn))
}

trait AsyncValueLowPriority {
  implicit def asyncOutputTypeLookup[F[_]: Async, A](implicit
      inner: GraphQLOutputTypeLookup[A]): GraphQLOutputTypeLookup[F[A]] =
    new GraphQLOutputTypeLookup[F[A]] {
      override val graphqlType: OutputType[F[A]] =
        inner.graphqlType.asInstanceOf[OutputType[F[A]]]
    }
}

object AsyncValue extends AsyncValueLowPriority {
  implicit def asyncAction[Ctx, Val, F[_]: Async](value: F[Val]): LeafAction[Ctx, Val] =
    AsyncValue(value)
}
