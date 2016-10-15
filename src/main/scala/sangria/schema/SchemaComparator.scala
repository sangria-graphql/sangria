package sangria.schema

import sangria.execution.ValueCoercionHelper
import sangria.renderer.SchemaRenderer

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

    val changed = oldTypes.intersect(newTypes).flatMap { name ⇒
      val oldType = oldSchema.types(name)._2
      val newType = newSchema.types(name)._2

      if (oldType.getClass == newType.getClass)
        findChangesInTypes(oldType, newType)
      else
        Vector(SchemaChange.TypeKindChanged(newType, oldType))
    }

    removed ++ added ++ changed ++ findChangesInSchema(oldSchema, newSchema)
  }

  def findChangesInSchema(oldSchema: Schema[_, _], newSchema: Schema[_, _]): Vector[SchemaChange] = {
    val withQuery =
      if (oldSchema.query.name != newSchema.query.name)
        Vector(SchemaChange.SchemaQueryTypeChanged(oldSchema.query, newSchema.query))
      else
        Vector.empty

    val withMutation =
      if (oldSchema.mutation.map(_.name) != newSchema.mutation.map(_.name))
        withQuery :+ SchemaChange.SchemaMutationTypeChanged(oldSchema.mutation, newSchema.mutation)
      else
        withQuery

    if (oldSchema.subscription.map(_.name) != newSchema.subscription.map(_.name))
      withMutation :+ SchemaChange.SchemaSubscriptionTypeChanged(oldSchema.subscription, newSchema.subscription)
    else
      withMutation
  }

  def findChangesInTypes(oldType: Type with Named, newType: Type with Named): Vector[SchemaChange] = {
    val typeChanges = (oldType, newType) match {
      case (o: EnumType[_], n: EnumType[_]) ⇒ findInEnumTypes(o, n)
      case (o: UnionType[_], n: UnionType[_]) ⇒ findInUnionTypes(o, n)
      case (o: InputObjectType[_], n: InputObjectType[_]) ⇒ findInInputObjectTypes(o, n)
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

  private def findInInputObjectTypes(oldType: InputObjectType[_], newType: InputObjectType[_]): Vector[SchemaChange] = {
    val oldFields = oldType.fields.map(_.name).toSet
    val newFields = newType.fields.map(_.name).toSet

    val removed = oldFields.diff(newFields).toVector.map(name ⇒
      SchemaChange.InputFieldRemoved(oldType, oldType.fieldsByName(name)))

    val added = newFields.diff(oldFields).toVector.map { name ⇒
      val field = newType.fieldsByName(name)

      SchemaChange.InputFieldAdded(newType, field, !isOptional(field))
    }

    val changed = oldFields.intersect(newFields).flatMap { name ⇒
      val oldField = oldType.fieldsByName(name)
      val newField = newType.fieldsByName(name)

      findDescriptionChanged(oldField, newField, SchemaChange.InputFieldDescriptionChanged(newType, newField, _, _)) ++
        findInInputFields(oldType, newType, oldField, newField)
    }

    removed ++ added ++ changed
  }

  private def findInInputFields(oldType: InputObjectType[_], newType: InputObjectType[_], oldField: InputField[_], newField: InputField[_]): Vector[SchemaChange] = {
    val oldDefault = oldField.defaultValue.flatMap(dv ⇒ DefaultValueRenderer.renderInputValueCompact(dv, oldField.fieldType, coercionHelper))
    val newDefault = newField.defaultValue.flatMap(dv ⇒ DefaultValueRenderer.renderInputValueCompact(dv, newField.fieldType, coercionHelper))

    val withDefault =
      if (oldDefault != newDefault)
        Vector(SchemaChange.InputFieldDefaultChanged(newType, newField, oldDefault, newDefault))
      else
        Vector.empty

    val oldFieldType = SchemaRenderer.renderTypeName(oldField.fieldType)
    val newFieldType = SchemaRenderer.renderTypeName(newField.fieldType)

    val withType =
      if (oldFieldType != newFieldType)
        withDefault :+ SchemaChange.InputFieldTypeChanged(newType, newField, SchemaRenderer.renderTypeName(nonContainer(newField)) != oldFieldType,
          oldField.fieldType, newField.fieldType)
      else
        withDefault

    withType
  }

  private def isOptional(field: InputField[_]) = field.fieldType match {
    case _: OptionInputType[_] ⇒ true
    case _ ⇒ false
  }

  private def nonContainer(field: InputField[_]) = field.fieldType match {
    case OptionInputType(ofType) ⇒ ofType
    case tpe ⇒ tpe
  }

  private def findDescriptionChanged(o: Named, n: Named, fn: (Option[String], Option[String]) ⇒ SchemaChange): Vector[SchemaChange] =
    if (o.description != n.description) Vector(fn(o.description, n.description))
    else Vector.empty

  private def findDeprecationChanged(o: HasDeprecation, n: HasDeprecation, fn: (Option[String], Option[String]) ⇒ SchemaChange): Vector[SchemaChange] =
    if (o.deprecationReason != n.deprecationReason) Vector(fn(o.deprecationReason, n.deprecationReason))
    else Vector.empty

  private val coercionHelper = new ValueCoercionHelper[Any]
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

  sealed abstract class AbstractChange(val description: String, val breakingChange: Boolean) extends SchemaChange

  // Breaking changes

  case class TypeRemoved(tpe: Type with Named) extends AbstractChange(s"`${tpe.name}` type was removed", true) with TypeChange

  case class TypeKindChanged(tpe: Type with Named, oldTpe: Type with Named)
    extends AbstractChange({
      val oldKind = kind(oldTpe)
      val newKind = kind(tpe)

      s"`${tpe.name}` changed from ${article(oldKind)} $oldKind type to ${article(newKind)} $newKind type"
    }, true) with TypeChange

  case class EnumValueRemoved(tpe: EnumType[_], value: EnumValue[_])
    extends AbstractChange(s"Enum value `${value.name}` was removed from enum `${tpe.name}`", true) with TypeChange

  case class UnionMemberRemoved(tpe: UnionType[_], member: ObjectType[_, _])
    extends AbstractChange(s"`${member.name}` type was removed from union `${tpe.name}`", true) with TypeChange

  case class InputFieldRemoved(tpe: InputObjectType[_], field: InputField[_])
    extends AbstractChange(s"Input field `${field.name}` was removed from `${tpe.name}` type", true) with TypeChange

  // Non-breaking changes

  case class TypeAdded(tpe: Type with Named)
    extends AbstractChange(s"`${tpe.name}` type was added", false) with TypeChange

  case class TypeDescriptionChanged(tpe: Type with Named, oldDescription: Option[String], newDescription: Option[String])
    extends AbstractChange(s"`${tpe.name}` type description is changed", false) with DescriptionChange

  case class EnumValueAdded(tpe: EnumType[_], value: EnumValue[_])
    extends AbstractChange(s"Enum value `${value.name}` was added to enum `${tpe.name}`", false) with TypeChange

  case class EnumValueDescriptionChanged(tpe: EnumType[_], value: EnumValue[_], oldDescription: Option[String], newDescription: Option[String])
    extends AbstractChange(s"`${tpe.name}.${value.name}` description changed", false) with DescriptionChange

  case class EnumValueDeprecated(tpe: EnumType[_], value: EnumValue[_], oldDeprecationReason: Option[String], newDeprecationReason: Option[String])
    extends AbstractChange(s"Enum value `${value.name}` was deprecated in enum `${tpe.name}`", false) with DeprecationChange

  case class UnionMemberAdded(tpe: UnionType[_], member: ObjectType[_, _])
    extends AbstractChange(s"`${member.name}` type was added to union `${tpe.name}`", false) with TypeChange

  case class InputFieldDescriptionChanged(tpe: InputObjectType[_], field: InputField[_], oldDescription: Option[String], newDescription: Option[String])
    extends AbstractChange(s"`${tpe.name}.${field.name}` description is changed", false) with DescriptionChange

  case class InputFieldDefaultChanged(tpe: InputObjectType[_], field: InputField[_], oldDefault: Option[String], newDefault: Option[String])
    extends AbstractChange(s"`${tpe.name}.${field.name}` default value changed from ${oldDefault.fold("none")("`" + _ + "`")} to ${newDefault.fold("none")("`" + _ + "`")}", false) with TypeChange

  case class SchemaQueryTypeChanged(oldType: ObjectType[_, _], newType: ObjectType[_, _])
    extends AbstractChange(s"Schema query type changed from `${oldType.name}` to `${newType.name}` type", true)

  // May be breaking changes

  case class InputFieldAdded(tpe: InputObjectType[_], field: InputField[_], breaking: Boolean)
    extends AbstractChange(s"Input field `${field.name}` was added to `${tpe.name}` type", breaking) with TypeChange

  case class InputFieldTypeChanged(tpe: InputObjectType[_], field: InputField[_], breaking: Boolean, oldFiledType: InputType[_], newFieldType: InputType[_])
    extends AbstractChange(s"`${tpe.name}.${field.name}` input field type changed from `${SchemaRenderer.renderTypeName(oldFiledType)}` to `${SchemaRenderer.renderTypeName(newFieldType)}`", breaking) with TypeChange

  case class SchemaMutationTypeChanged(oldType: Option[ObjectType[_, _]], newType: Option[ObjectType[_, _]])
    extends AbstractChange(s"Schema mutation type changed from ${oldType.fold("none")(t ⇒ "`" + t.name + "`")} to ${oldType.fold("none")(t ⇒ "`" + t.name + "`")} type", oldType.nonEmpty)

  case class SchemaSubscriptionTypeChanged(oldType: Option[ObjectType[_, _]], newType: Option[ObjectType[_, _]])
    extends AbstractChange(s"Schema subscription type changed from ${oldType.fold("none")(t ⇒ "`" + t.name + "`")} to ${oldType.fold("none")(t ⇒ "`" + t.name + "`")} type", oldType.nonEmpty)

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
