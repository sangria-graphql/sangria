package sangria.execution

import sangria.schema.{Context, EnumType}

trait DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]): Unit
  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx): Unit
}

object DeprecationTracker {
  val empty = NilDeprecationTracker
  val print = new LoggingDeprecationTracker(println)
}

object NilDeprecationTracker extends DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]) = ()
  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx) = ()
}

class LoggingDeprecationTracker(logFn: String ⇒ Unit) extends DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]) =
    logFn(s"Deprecated field '${ctx.parentType.name}.${ctx.field.name}' used at path '${ctx.path}'.")

  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx) =
    logFn(s"Deprecated enum value '$value' used of enum '${enum.name}'.")
}
