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
      case (o: EnumType[_], n: EnumType[_]) ⇒ findInEnumTypes(o, n)
      case (o: UnionType[_], n: UnionType[_]) ⇒ findInUnionTypes(o, n)
      case _ ⇒ Vector.empty
    }

    typeChanges ++ findDescriptionChanged(oldType, newType, SchemaChange.TypeDescriptionChanged(newType, _, _))
  }

  private def findInUnionTypes(oldType: UnionType[_], newType: UnionType[_]): Vector[SchemaChange] = {
    val oldTypes = oldType.types.map(_.name).toSet
    val newTypes = newType.types.map(_.name).toSet

    val removed = oldTypes.diff(newTypes).toVector.map(name ⇒
      SchemaChange.UnionMemberRemoved(oldType, oldType.types.find(_.name == name).get))

    val added = newTypes.diff(oldTypes).toVector.map(name ⇒
      SchemaChange.UnionMemberAdded(newType, newType.types.find(_.name == name).get))

    removed ++ added
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

      findDescriptionChanged(oldValue, newValue, SchemaChange.EnumValueDescriptionChanged(newType, newValue, _, _)) ++
        findDeprecationChanged(oldValue, newValue, SchemaChange.EnumValueDeprecated(newType, newValue, _, _))
    }

    removed ++ added ++ changed
  }

  private def findDescriptionChanged(o: Named, n: Named, fn: (Option[String], Option[String]) ⇒ SchemaChange): Vector[SchemaChange] =
    if (o.description != n.description) Vector(fn(o.description, n.description))
    else Vector.empty

  private def findDeprecationChanged(o: HasDeprecation, n: HasDeprecation, fn: (Option[String], Option[String]) ⇒ SchemaChange): Vector[SchemaChange] =
    if (o.deprecationReason != n.deprecationReason) Vector(fn(o.deprecationReason, n.deprecationReason))
    else Vector.empty
}

sealed trait SchemaChange {
  def breakingChange: Boolean
  def description: String
}

object SchemaChange {
  sealed trait TypeChange {
    def tpe: Type with Named
  }

  sealed trait DeprecationChange extends TypeChange {
    def oldDeprecationReason: Option[String]
    def newDeprecationReason: Option[String]
  }

  sealed trait DescriptionChange extends TypeChange {
    def oldDescription: Option[String]
    def newDescription: Option[String]
  }

  sealed abstract class BreakingChange(val description: String) extends SchemaChange {
    final def breakingChange = true
  }

  case class TypeRemoved(tpe: Type with Named) extends BreakingChange(s"`${tpe.name}` type was removed") with TypeChange

  case class TypeKindChanged(tpe: Type with Named, oldTpe: Type with Named)
    extends BreakingChange({
      val oldKind = kind(oldTpe)
      val newKind = kind(tpe)

      s"`${tpe.name}` changed from ${article(oldKind)} $oldKind type to ${article(newKind)} $newKind type"
    }) with TypeChange

  case class EnumValueRemoved(tpe: EnumType[_], value: EnumValue[_])
    extends BreakingChange(s"Enum value `${value.name}` was removed from enum `${tpe.name}`") with TypeChange

  case class UnionMemberRemoved(tpe: UnionType[_], member: ObjectType[_, _])
    extends BreakingChange(s"`${member.name}` type was removed from union `${tpe.name}`") with TypeChange

  sealed abstract class NonBreakingChange(val description: String) extends SchemaChange {
    final def breakingChange = false
  }

  case class TypeAdded(tpe: Type with Named)
    extends NonBreakingChange(s"`${tpe.name}` type was added") with TypeChange

  case class TypeDescriptionChanged(tpe: Type with Named, oldDescription: Option[String], newDescription: Option[String])
    extends NonBreakingChange(s"`${tpe.name}` type description is changed") with DescriptionChange

  case class EnumValueAdded(tpe: EnumType[_], value: EnumValue[_])
    extends NonBreakingChange(s"Enum value `${value.name}` was added to enum `${tpe.name}`") with TypeChange

  case class EnumValueDescriptionChanged(tpe: EnumType[_], value: EnumValue[_], oldDescription: Option[String], newDescription: Option[String])
      extends NonBreakingChange(s"`${tpe.name}.${value.name}` description changed") with DescriptionChange

  case class EnumValueDeprecated(tpe: EnumType[_], value: EnumValue[_], oldDeprecationReason: Option[String], newDeprecationReason: Option[String])
    extends NonBreakingChange(s"Enum value `${value.name}` was deprecated in enum `${tpe.name}`") with DeprecationChange

  case class UnionMemberAdded(tpe: UnionType[_], member: ObjectType[_, _])
    extends BreakingChange(s"`${member.name}` type was added to union `${tpe.name}`") with TypeChange

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
