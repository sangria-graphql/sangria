package sangria.execution

import sangria.schema.{Context, ObjectType, EnumType, Field}

trait DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]): Unit
  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx): Unit
}

object DeprecationTracker {
  val empty = NilDeprecationTracker
  val print = PrintingDeprecationTracker
}

object NilDeprecationTracker extends DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]) = ()
  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx) = ()
}

object PrintingDeprecationTracker extends DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]) =
    println(s"Deprecated field '${ctx.parentType.name}.${ctx.field.name}' used at path '${ctx.path}'.")

  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx) =
    println(s"Deprecated enum value '$value' used of enum '${enum.name}'.")
}
