package sangria.schema

import language.existentials

object SchemaComparator {
  def compare(oldSchema: Schema[_, _], newSchema: Schema[_, _]): Vector[SchemaChange] =
    findChanges(oldSchema, newSchema)

  private def findChanges(oldSchema: Schema[_, _], newSchema: Schema[_, _]) = {
    val oldTypes = oldSchema.availableTypeNames.toSet
    val newTypes = newSchema.availableTypeNames.toSet

    val removed = oldTypes.diff(newTypes).toVector.map(name ⇒
      SchemaChange.TypeRemoved(oldSchema.types(name)._2))

    val added = newTypes.diff(oldTypes).toVector.map(name ⇒
      SchemaChange.TypeAdded(newSchema.types(name)._2))

    val kindChange = oldTypes.intersect(newTypes).flatMap { name ⇒
      val oldType = oldSchema.types(name)._2
      val newType = newSchema.types(name)._2

      if (oldType.getClass == newType.getClass)
        findChangesInTypes(oldType, newType)
      else
        Vector(SchemaChange.TypeKindChanged(newType, oldType))
    }

    removed ++ added ++ kindChange
  }

  def findChangesInTypes(oldType: Type with Named, newType: Type with Named): Vector[SchemaChange] = {
    val typeChanges = (oldType, newType) match {
      case (oldEnum: EnumType[_], newEnum: EnumType[_]) ⇒ findInEnumTypes(oldEnum, newEnum)
      case _ ⇒ Vector.empty
    }

    typeChanges ++ findDescriptionChanged(oldType, newType, SchemaChange.TypeDescriptionChanged(newType, _, _))
  }

  private def findInEnumTypes(oldType: EnumType[_], newType: EnumType[_]): Vector[SchemaChange] = {
    val oldValues = oldType.values.map(_.name).toSet
    val newValues = newType.values.map(_.name).toSet

    val removed = oldValues.diff(newValues).toVector.map(name ⇒
      SchemaChange.EnumValueRemoved(oldType, oldType.byName(name)))

    val added = newValues.diff(oldValues).toVector.map(name ⇒
      SchemaChange.EnumValueAdded(newType, newType.byName(name)))

    val changed = oldValues.intersect(newValues).flatMap { name ⇒
      val oldValue = oldType.byName(name)
      val newValue = newType.byName(name)

      findDescriptionChanged(oldValue, newValue, SchemaChange.EnumValueDescriptionChanged(newType, newValue, _, _))
      // TODO: deprecation
    }

    removed ++ added ++ changed
  }

  private def findDescriptionChanged(o: Named, n: Named, fn: (Option[String], Option[String]) ⇒ SchemaChange): Vector[SchemaChange] =
    if (o.description != n.description) Vector(fn(o.description, n.description))
    else Vector.empty
}

sealed trait SchemaChange {
  def breakingChange: Boolean
  def description: String
}

sealed trait TypeSchemaChange {
  def tpe: Type with Named
}

object SchemaChange {
  sealed abstract class BreakingChange(val description: String) extends SchemaChange {
    final def breakingChange = true
  }

  case class TypeRemoved(tpe: Type with Named) extends BreakingChange(s"`${tpe.name}` type was removed") with TypeSchemaChange

  case class TypeKindChanged(tpe: Type with Named, oldTpe: Type with Named)
    extends BreakingChange({
      val oldKind = kind(oldTpe)
      val newKind = kind(tpe)

      s"`${tpe.name}` changed from ${article(oldKind)} $oldKind type to ${article(newKind)} $newKind type"
    }) with TypeSchemaChange

  case class EnumValueRemoved(tpe: EnumType[_], value: EnumValue[_])
    extends BreakingChange(s"`${value.name}` was removed from enum `${tpe.name}`") with TypeSchemaChange

  sealed abstract class NonBreakingChange(val description: String) extends SchemaChange {
    final def breakingChange = false
  }

  case class TypeAdded(tpe: Type with Named)
    extends NonBreakingChange(s"`${tpe.name}` type was added") with TypeSchemaChange

  case class TypeDescriptionChanged(tpe: Type with Named, oldDescription: Option[String], newDescription: Option[String])
    extends NonBreakingChange(s"`${tpe.name}` type description is changed") with TypeSchemaChange

  case class EnumValueAdded(tpe: EnumType[_], value: EnumValue[_])
    extends NonBreakingChange(s"`${value.name}` was added to enum `${tpe.name}`") with TypeSchemaChange

  case class EnumValueDeprecated(tpe: EnumType[_], value: EnumValue[_])
    extends NonBreakingChange(s"`${value.name}` was deprecated in enum `${tpe.name}`") with TypeSchemaChange

  case class EnumValueDescriptionChanged(tpe: EnumType[_], value: EnumValue[_], oldDescription: Option[String], newDescription: Option[String])
    extends NonBreakingChange(s"`${tpe.name}.${value.name}` deprecation reason changed") with TypeSchemaChange

  case class EnumValueDeprecationReasonChanged(tpe: EnumType[_], value: EnumValue[_], oldDeprecationReason: Option[String], newDeprecationReason: Option[String])
    extends NonBreakingChange(s"`${tpe.name}.${value.name}` deprecation reason changed") with TypeSchemaChange

  private val AnArticleLetters = Set('a', 'e', 'i', 'o')

  private def kind(tpe: Type) = tpe match {
    case _: ObjectType[_, _] ⇒ "Object"
    case _: InterfaceType[_, _] ⇒ "Interface"
    case _: ScalarType[_] ⇒ "Scalar"
    case _: UnionType[_] ⇒ "Union"
    case _: EnumType[_] ⇒ "Enum"
    case _: InputObjectType[_] ⇒ "InputObject"
    case t ⇒ throw new IllegalStateException(s"Unsupported type kind: $t")
  }

  private def article(word: String) =
    if (AnArticleLetters contains word.head.toLower) "an"
    else "a"
}
