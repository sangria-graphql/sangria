package sangria.execution

import sangria.schema.{EnumType, Field}

trait DeprecationTracker {
  def deprecatedFieldUsed[Ctx](path: List[String], field: Field[Ctx, _], userContext: Ctx): Unit
  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx): Unit
}

object DeprecationTracker {
  val empty = NilDeprecationTracker
}

object NilDeprecationTracker extends DeprecationTracker {
  def deprecatedFieldUsed[Ctx](path: List[String], field: Field[Ctx, _], userContext: Ctx) = ()
  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx) = ()
}

object PrintingDeprecationTracker extends DeprecationTracker {
  def deprecatedFieldUsed[Ctx](path: List[String], field: Field[Ctx, _], userContext: Ctx) =
    println(s"Deprecated field '${field.name}' used at path '${path mkString "."}'.")

  def deprecatedEnumValueUsed[T, Ctx](enum: EnumType[T], value: T, userContext: Ctx) =
    println(s"Deprecated enum value '$value' used of enum '${enum.name}'.")
}
