package sangria.execution

import sangria.schema._

trait DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]): Unit
  def deprecatedEnumValueUsed[T, Ctx](`enum`: EnumType[T], value: T, userContext: Ctx): Unit
  def deprecatedDirectiveArgUsed[Ctx](
      directive: Directive,
      argument: Argument[_],
      ctx: Context[Ctx, _]): Unit
  def deprecatedInputObjectFieldUsed[T, Ctx](
      inputObject: InputObjectType[T],
      field: InputField[_],
      ctx: Context[Ctx, _]): Unit
  def deprecatedFieldArgUsed[Ctx](
      argument: Argument[_],
      ctx: Context[Ctx, _]
  ): Unit
}

object DeprecationTracker {
  val empty = NilDeprecationTracker
  val print = new LoggingDeprecationTracker(println)
}

object NilDeprecationTracker extends DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]) = ()
  def deprecatedEnumValueUsed[T, Ctx](`enum`: EnumType[T], value: T, userContext: Ctx) = ()
  def deprecatedDirectiveArgUsed[Ctx](
      directive: Directive,
      argument: Argument[_],
      ctx: Context[Ctx, _]) = ()
  def deprecatedInputObjectFieldUsed[T, Ctx](
      inputObject: InputObjectType[T],
      field: InputField[_],
      ctx: Context[Ctx, _]) = ()
  def deprecatedFieldArgUsed[Ctx](
      argument: Argument[_],
      ctx: Context[Ctx, _]
  ) = ()
}

class LoggingDeprecationTracker(logFn: String => Unit) extends DeprecationTracker {
  def deprecatedFieldUsed[Ctx](ctx: Context[Ctx, _]) =
    logFn(
      s"Deprecated field '${ctx.parentType.name}.${ctx.field.name}' used at path '${ctx.path}'.")

  def deprecatedEnumValueUsed[T, Ctx](`enum`: EnumType[T], value: T, userContext: Ctx) =
    logFn(s"Deprecated enum value '$value' used of enum '${`enum`.name}'.")

  def deprecatedDirectiveArgUsed[Ctx](
      directive: Directive,
      argument: Argument[_],
      ctx: Context[Ctx, _]) =
    logFn(s"Deprecated argument '${argument.name}' used of directive '${directive.name}'.")

  def deprecatedInputObjectFieldUsed[T, Ctx](
      inputObject: InputObjectType[T],
      field: InputField[_],
      ctx: Context[Ctx, _]) = logFn(
    s"Deprecated field '${field.name}' used of input object '${inputObject.name}'.")

  def deprecatedFieldArgUsed[Ctx](
      argument: Argument[_],
      ctx: Context[Ctx, _]
  ) = logFn(
    s"Deprecated argument '${argument.name}' used of field '${ctx.parentType.name}'.${ctx.field.name}'.")
}
