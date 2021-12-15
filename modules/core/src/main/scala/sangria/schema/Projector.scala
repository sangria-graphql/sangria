package sangria.schema

import language.{higherKinds, implicitConversions}
import sangria.execution._
import sangria.marshalling._

case class ProjectionName(name: String) extends FieldTag
case object ProjectionExclude extends FieldTag

trait Projector[Ctx, Val, Res] extends (Context[Ctx, Val] => Action[Ctx, Res]) {
  val maxLevel: Int = Integer.MAX_VALUE
  def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]): Action[Ctx, Res]
}

object Projector {
  def apply[Ctx, Val, Res](fn: (Context[Ctx, Val], Vector[ProjectedName]) => Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]): Action[Ctx, Res] =
        fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = throw new IllegalStateException(
        "Default apply should not be called on projector!")
    }

  def apply[Ctx, Val, Res](
      levels: Int,
      fn: (Context[Ctx, Val], Vector[ProjectedName]) => Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      override val maxLevel: Int = levels
      def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]): Action[Ctx, Res] =
        fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = throw new IllegalStateException(
        "Default apply should not be called on projector!")
    }
}

case class ProjectedName(
    name: String,
    children: Vector[ProjectedName] = Vector.empty,
    args: Args = Args.empty) {
  lazy val asVector: Vector[Vector[String]] = {
    def loop(name: ProjectedName): Vector[Vector[String]] =
      Vector(name.name) +: (name.children.flatMap(loop).map(name.name +: _))

    loop(this)
  }
}

case class DefaultValueParser[T](
    schema: Schema[_, _],
    parser: InputParser[T],
    toInput: ToInput[T, _])

object DefaultValueParser {
  def forType[T](schema: Schema[_, _])(implicit
      parser: InputParser[T],
      toInput: ToInput[T, _]): DefaultValueParser[T] =
    DefaultValueParser[T](schema, parser, toInput)
}
