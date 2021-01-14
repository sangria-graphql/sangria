package sangria.schema

import sangria.execution.ValueCoercionHelper
import sangria.renderer.SchemaRenderer
import sangria.ast
import sangria.ast.AstNode

object SchemaComparator {
  def compare[F[_]](oldSchema: Schema[_, _, F], newSchema: Schema[_, _, F]): Vector[SchemaChange] =
    findChanges(oldSchema, newSchema)

  private def findChanges[F[_]](oldSchema: Schema[_, _, F], newSchema: Schema[_, _, F]) = {
    val oldTypes = oldSchema.availableTypeNames.toSet
    val newTypes = newSchema.availableTypeNames.toSet

    val removed = oldTypes
      .diff(newTypes)
      .toVector
      .map(name => SchemaChange.TypeRemoved(oldSchema.types(name)._2))

    val added =
      newTypes.diff(oldTypes).toVector.map(name => SchemaChange.TypeAdded(newSchema.types(name)._2))

    val changed = oldTypes.intersect(newTypes).flatMap { name =>
      val oldType = oldSchema.types(name)._2
      val newType = newSchema.types(name)._2

      if (oldType.getClass == newType.getClass)
        findChangesInTypes(oldType, newType)
      else
        Vector(SchemaChange.TypeKindChanged(newType, oldType))
    }

    removed ++ added ++ changed ++
      findChangesInSchema(oldSchema, newSchema) ++
      findInDirectives(oldSchema, newSchema)
  }

  private def findInDirectives[F[_]](oldSchema: Schema[_, _, F], newSchema: Schema[_, _, F]) = {
    val oldDirs = oldSchema.directives.map(_.name).toSet
    val newDirs = newSchema.directives.map(_.name).toSet

    val removed = oldDirs
      .diff(newDirs)
      .toVector
      .map(name => SchemaChange.DirectiveRemoved(oldSchema.directivesByName(name)))

    val added = newDirs
      .diff(oldDirs)
      .toVector
      .map(name => SchemaChange.DirectiveAdded(newSchema.directivesByName(name)))

    val changed = oldDirs.intersect(newDirs).flatMap { name =>
      val oldDir = oldSchema.directivesByName(name)
      val newDir = newSchema.directivesByName(name)

      findDescriptionChanged(
        oldDir,
        newDir,
        SchemaChange.DirectiveDescriptionChanged(newDir, _, _)) ++
        findInDirective(oldDir, newDir)
    }

    removed ++ added ++ changed
  }

  private def findInDirective(oldDir: Directive, newDir: Directive): Vector[SchemaChange] = {
    val locationChanges = findInDirectiveLocations(oldDir, newDir)
    val fieldChanges = findInArgs(
      oldDir.arguments,
      newDir.arguments,
      added = SchemaChange.DirectiveArgumentAdded(newDir, _, _),
      removed = SchemaChange.DirectiveArgumentRemoved(oldDir, _),
      description = SchemaChange.DirectiveArgumentDescriptionChanged(newDir, _, _, _),
      default = SchemaChange.DirectiveArgumentDefaultChanged(newDir, _, _, _),
      typeChange = SchemaChange.DirectiveArgumentTypeChanged(newDir, _, _, _, _),
      dirAdded = SchemaChange.DirectiveArgumentAstDirectiveAdded(newDir, _, _),
      dirRemoved = SchemaChange.DirectiveArgumentAstDirectiveRemoved(newDir, _, _)
    )

    locationChanges ++ fieldChanges
  }

  private def findInDirectiveLocations(
      oldDir: Directive,
      newDir: Directive): Vector[SchemaChange] = {
    val oldLocs = oldDir.locations
    val newLocs = newDir.locations

    val removed =
      oldLocs.diff(newLocs).toVector.map(loc => SchemaChange.DirectiveLocationRemoved(oldDir, loc))

    val added =
      newLocs.diff(oldLocs).toVector.map(loc => SchemaChange.DirectiveLocationAdded(newDir, loc))

    removed ++ added
  }

  def findChangesInSchema[F[_]](
      oldSchema: Schema[_, _, F],
      newSchema: Schema[_, _, F]): Vector[SchemaChange] = {
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

    val withSubscription =
      if (oldSchema.subscription.map(_.name) != newSchema.subscription.map(_.name))
        withMutation :+ SchemaChange.SchemaSubscriptionTypeChanged(
          oldSchema.subscription,
          newSchema.subscription)
      else
        withMutation

    val directiveChanges = findInAstDirs(
      oldSchema.astDirectives,
      newSchema.astDirectives,
      added = SchemaChange.SchemaAstDirectiveAdded(newSchema, _),
      removed = SchemaChange.SchemaAstDirectiveRemoved(newSchema, _)
    )

    val descriptionChanges =
      findDescriptionChanged(oldSchema, newSchema, SchemaChange.SchemaDescriptionChanged)

    withSubscription ++ directiveChanges ++ descriptionChanges
  }

  def findChangesInTypes(
      oldType: Type with Named,
      newType: Type with Named): Vector[SchemaChange] = {
    val typeChanges = (oldType, newType) match {
      case (o: EnumType[_], n: EnumType[_]) => findInEnumTypes(o, n)
      case (o: UnionType[_, _], n: UnionType[_, _]) => findInUnionTypes(o, n)
      case (o: ScalarType[_], n: ScalarType[_]) => findInScalarTypes(o, n)
      case (o: InputObjectType[_], n: InputObjectType[_]) => findInInputObjectTypes(o, n)
      case (o: ObjectType[_, _, _], n: ObjectType[_, _, _]) => findInObjectTypes(o, n)
      case (o: InterfaceType[_, _, _], n: InterfaceType[_, _, _]) => findInInterfaceTypes(o, n)
      case _ => Vector.empty
    }

    typeChanges ++ findDescriptionChanged(
      oldType,
      newType,
      SchemaChange.TypeDescriptionChanged(newType, _, _))
  }

  private def findInUnionTypes[F[_]](
      oldType: UnionType[_, F],
      newType: UnionType[_, F]): Vector[SchemaChange] = {
    val oldTypes = oldType.types.map(_.name).toSet
    val newTypes = newType.types.map(_.name).toSet

    val removed = oldTypes
      .diff(newTypes)
      .toVector
      .map(name => SchemaChange.UnionMemberRemoved(oldType, oldType.types.find(_.name == name).get))

    val added = newTypes
      .diff(oldTypes)
      .toVector
      .map(name => SchemaChange.UnionMemberAdded(newType, newType.types.find(_.name == name).get))

    val directiveChanges = findInAstDirs(
      oldType.astDirectives,
      newType.astDirectives,
      added = SchemaChange.UnionTypeAstDirectiveAdded(newType, _),
      removed = SchemaChange.UnionTypeAstDirectiveRemoved(newType, _)
    )

    removed ++ added ++ directiveChanges
  }

  private def findInScalarTypes(
      oldType: ScalarType[_],
      newType: ScalarType[_]): Vector[SchemaChange] =
    findInAstDirs(
      oldType.astDirectives,
      newType.astDirectives,
      added = SchemaChange.ScalarTypeAstDirectiveAdded(newType, _),
      removed = SchemaChange.ScalarTypeAstDirectiveRemoved(newType, _)
    )

  private def findInEnumTypes(oldType: EnumType[_], newType: EnumType[_]): Vector[SchemaChange] = {
    val oldValues = oldType.values.map(_.name).toSet
    val newValues = newType.values.map(_.name).toSet

    val removed = oldValues
      .diff(newValues)
      .toVector
      .map(name => SchemaChange.EnumValueRemoved(oldType, oldType.byName(name)))

    val added = newValues
      .diff(oldValues)
      .toVector
      .map(name => SchemaChange.EnumValueAdded(newType, newType.byName(name)))

    val changed = oldValues.intersect(newValues).flatMap { name =>
      val oldValue = oldType.byName(name)
      val newValue = newType.byName(name)

      val directiveChanges = findInAstDirs(
        oldValue.astDirectives,
        newValue.astDirectives,
        added = SchemaChange.EnumValueAstDirectiveAdded(newType, newValue, _),
        removed = SchemaChange.EnumValueAstDirectiveRemoved(newType, newValue, _)
      )

      findDescriptionChanged(
        oldValue,
        newValue,
        SchemaChange.EnumValueDescriptionChanged(newType, newValue, _, _)) ++
        findDeprecationChanged(
          oldValue,
          newValue,
          SchemaChange.EnumValueDeprecated(newType, newValue, _, _)) ++
        directiveChanges
    }

    val directiveChanges = findInAstDirs(
      oldType.astDirectives,
      newType.astDirectives,
      added = SchemaChange.EnumTypeAstDirectiveAdded(newType, _),
      removed = SchemaChange.EnumTypeAstDirectiveRemoved(newType, _)
    )

    removed ++ added ++ changed ++ directiveChanges
  }

  private def findInInputObjectTypes(
      oldType: InputObjectType[_],
      newType: InputObjectType[_]): Vector[SchemaChange] = {
    val oldFields = oldType.fields.map(_.name).toSet
    val newFields = newType.fields.map(_.name).toSet

    val removed = oldFields
      .diff(newFields)
      .toVector
      .map(name => SchemaChange.InputFieldRemoved(oldType, oldType.fieldsByName(name)))

    val added = newFields.diff(oldFields).toVector.map { name =>
      val field = newType.fieldsByName(name)

      SchemaChange.InputFieldAdded(newType, field, !isOptional(field))
    }

    val changed = oldFields.intersect(newFields).flatMap { name =>
      val oldField = oldType.fieldsByName(name)
      val newField = newType.fieldsByName(name)

      findDescriptionChanged(
        oldField,
        newField,
        SchemaChange.InputFieldDescriptionChanged(newType, newField, _, _)) ++
        findInInputFields(oldType, newType, oldField, newField)
    }

    val directiveChanges = findInAstDirs(
      oldType.astDirectives,
      newType.astDirectives,
      added = SchemaChange.InputObjectTypeAstDirectiveAdded(newType, _),
      removed = SchemaChange.InputObjectTypeAstDirectiveRemoved(newType, _)
    )

    removed ++ added ++ changed ++ directiveChanges
  }

  private def findInObjectTypes[F[_]](
      oldType: ObjectType[_, _, F],
      newType: ObjectType[_, _, F]): Vector[SchemaChange] = {
    val interfaceChanges = findInObjectTypeInterfaces(oldType, newType)
    val fieldChanges = findInObjectTypeFields(oldType, newType)
    val directiveChanges = findInAstDirs(
      oldType.astDirectives,
      newType.astDirectives,
      added = SchemaChange.ObjectTypeAstDirectiveAdded(newType, _),
      removed = SchemaChange.ObjectTypeAstDirectiveRemoved(newType, _)
    )

    interfaceChanges ++ fieldChanges ++ directiveChanges
  }

  private def findInInterfaceTypes[F[_]](
      oldType: InterfaceType[_, _, F],
      newType: InterfaceType[_, _, F]): Vector[SchemaChange] =
    findInObjectTypeFields(oldType, newType) ++
      findInAstDirs(
        oldType.astDirectives,
        newType.astDirectives,
        added = SchemaChange.InterfaceTypeAstDirectiveAdded(newType, _),
        removed = SchemaChange.InterfaceTypeAstDirectiveRemoved(newType, _)
      )

  private def findInObjectTypeInterfaces[F[_]](
      oldType: ObjectType[_, _, F],
      newType: ObjectType[_, _, F]): Vector[SchemaChange] = {
    val oldInts = oldType.allInterfaces.map(_.name).toSet
    val newInts = newType.allInterfaces.map(_.name).toSet

    val removed = oldInts
      .diff(newInts)
      .toVector
      .map(name =>
        SchemaChange
          .ObjectTypeInterfaceRemoved(oldType, oldType.interfaces.find(_.name == name).get))

    val added = newInts
      .diff(oldInts)
      .toVector
      .map(name =>
        SchemaChange.ObjectTypeInterfaceAdded(newType, newType.interfaces.find(_.name == name).get))

    removed ++ added
  }

  private def findInObjectTypeFields[F[_]](
      oldType: ObjectLikeType[_, _, F],
      newType: ObjectLikeType[_, _, F]): Vector[SchemaChange] = {
    val oldFields = oldType.fields.map(_.name).toSet
    val newFields = newType.fields.map(_.name).toSet

    val removed = oldFields
      .diff(newFields)
      .toVector
      .map(name => SchemaChange.FieldRemoved(oldType, oldType.fieldsByName(name).head))

    val added = newFields
      .diff(oldFields)
      .toVector
      .map(name => SchemaChange.FieldAdded(newType, newType.fieldsByName(name).head))

    val changed = oldFields.intersect(newFields).flatMap { name =>
      val oldField = oldType.fieldsByName(name).head
      val newField = newType.fieldsByName(name).head

      findDescriptionChanged(
        oldField,
        newField,
        SchemaChange.FieldDescriptionChanged(newType, newField, _, _)) ++
        findDeprecationChanged(
          oldField,
          newField,
          SchemaChange.FieldDeprecationChanged(newType, newField, _, _)) ++
        findInFields(oldType, newType, oldField, newField)
    }

    removed ++ added ++ changed
  }

  private def findInFields[F[_]](
      oldType: ObjectLikeType[_, _, F],
      newType: ObjectLikeType[_, _, F],
      oldField: Field[_, _, F],
      newField: Field[_, _, F]): Vector[SchemaChange] = {
    val oldFieldType = SchemaRenderer.renderTypeName(oldField.fieldType)
    val newFieldType = SchemaRenderer.renderTypeName(newField.fieldType)

    val typeChanges =
      if (oldFieldType != newFieldType)
        Vector(
          SchemaChange.FieldTypeChanged(
            newType,
            newField,
            SchemaRenderer.renderTypeName(nonContainer(oldField)) != newFieldType,
            oldField.fieldType,
            newField.fieldType))
      else
        Vector.empty

    val directiveChanges = findInAstDirs(
      oldField.astDirectives,
      newField.astDirectives,
      added = SchemaChange.FieldAstDirectiveAdded(newType, newField, _),
      removed = SchemaChange.FieldAstDirectiveRemoved(newType, newField, _)
    )

    val argChanges = findInArgs(
      oldField.arguments,
      newField.arguments,
      added = SchemaChange.ObjectTypeArgumentAdded(newType, newField, _, _),
      removed = SchemaChange.ObjectTypeArgumentRemoved(oldType, oldField, _),
      description = SchemaChange.ObjectTypeArgumentDescriptionChanged(newType, newField, _, _, _),
      default = SchemaChange.ObjectTypeArgumentDefaultChanged(newType, newField, _, _, _),
      typeChange = SchemaChange.ObjectTypeArgumentTypeChanged(newType, newField, _, _, _, _),
      dirAdded = SchemaChange.FieldArgumentAstDirectiveAdded(newType, newField, _, _),
      dirRemoved = SchemaChange.FieldArgumentAstDirectiveRemoved(newType, newField, _, _)
    )

    typeChanges ++ argChanges ++ directiveChanges
  }

  private def findInArgs(
      oldArgs: List[Argument[_]],
      newArgs: List[Argument[_]],
      added: (Argument[_], Boolean) => SchemaChange,
      removed: Argument[_] => SchemaChange,
      description: (Argument[_], Option[String], Option[String]) => SchemaChange,
      default: (Argument[_], Option[ast.Value], Option[ast.Value]) => SchemaChange,
      typeChange: (Argument[_], Boolean, InputType[_], InputType[_]) => SchemaChange,
      dirAdded: (Argument[_], ast.Directive) => SchemaChange,
      dirRemoved: (Argument[_], ast.Directive) => SchemaChange
  ): Vector[SchemaChange] = {
    val oldA = oldArgs.map(_.name).toSet
    val newA = newArgs.map(_.name).toSet

    val remove = oldA.diff(newA).toVector.map(name => removed(oldArgs.find(_.name == name).get))

    val add = newA.diff(oldA).toVector.map { name =>
      val arg = newArgs.find(_.name == name).get

      added(arg, !isOptional(arg))
    }

    val changed = oldA.intersect(newA).flatMap { name =>
      val oldArg = oldArgs.find(_.name == name).get
      val newArg = newArgs.find(_.name == name).get

      findDescriptionChanged(oldArg, newArg, description(newArg, _, _)) ++
        findInArg(
          oldArg,
          newArg,
          default(newArg, _, _),
          typeChange(newArg, _, _, _),
          dirAdded(newArg, _),
          dirRemoved(newArg, _))
    }

    remove ++ add ++ changed
  }

  private def findInAstDirs(
      oldDirectives: Vector[ast.Directive],
      newDirectives: Vector[ast.Directive],
      added: (ast.Directive) => SchemaChange,
      removed: (ast.Directive) => SchemaChange
  ): Vector[SchemaChange] = {
    val oldD = oldDirectives.map(AstNode.withoutAstLocations(_)).toSet
    val newD = newDirectives.map(AstNode.withoutAstLocations(_)).toSet

    val remove = oldD.diff(newD).toVector.map(removed)
    val add = newD.diff(oldD).toVector.map(added)

    remove ++ add
  }

  private def findInArg(
      oldArg: Argument[_],
      newArg: Argument[_],
      default: (Option[ast.Value], Option[ast.Value]) => SchemaChange,
      typeChange: (Boolean, InputType[_], InputType[_]) => SchemaChange,
      dirAdded: ast.Directive => SchemaChange,
      dirRemoved: ast.Directive => SchemaChange
  ): Vector[SchemaChange] = {
    val oldDefault = oldArg.defaultValue.flatMap(dv =>
      DefaultValueRenderer
        .renderInputValue(dv, oldArg.argumentType, coercionHelper)
        .map(v => AstNode.withoutAstLocations(v)))
    val newDefault = newArg.defaultValue.flatMap(dv =>
      DefaultValueRenderer
        .renderInputValue(dv, newArg.argumentType, coercionHelper)
        .map(v => AstNode.withoutAstLocations(v)))

    val withDefault =
      if (oldDefault != newDefault)
        Vector(default(oldDefault, newDefault))
      else
        Vector.empty

    val oldArgType = SchemaRenderer.renderTypeName(oldArg.argumentType)
    val newArgType = SchemaRenderer.renderTypeName(newArg.argumentType)

    val withType =
      if (oldArgType != newArgType)
        withDefault :+ typeChange(
          SchemaRenderer.renderTypeName(nonContainer(newArg)) != oldArgType,
          oldArg.argumentType,
          newArg.argumentType)
      else
        withDefault

    val directiveChanges = findInAstDirs(
      oldArg.astDirectives,
      newArg.astDirectives,
      added = dirAdded,
      removed = dirRemoved)

    withType ++ directiveChanges
  }

  private def findInInputFields(
      oldType: InputObjectType[_],
      newType: InputObjectType[_],
      oldField: InputField[_],
      newField: InputField[_]): Vector[SchemaChange] = {
    val oldDefault = oldField.defaultValue.flatMap(dv =>
      DefaultValueRenderer
        .renderInputValue(dv, oldField.fieldType, coercionHelper)
        .map(v => AstNode.withoutAstLocations(v)))
    val newDefault = newField.defaultValue.flatMap(dv =>
      DefaultValueRenderer
        .renderInputValue(dv, newField.fieldType, coercionHelper)
        .map(v => AstNode.withoutAstLocations(v)))

    val withDefault =
      if (oldDefault != newDefault)
        Vector(SchemaChange.InputFieldDefaultChanged(newType, newField, oldDefault, newDefault))
      else
        Vector.empty

    val oldFieldType = SchemaRenderer.renderTypeName(oldField.fieldType)
    val newFieldType = SchemaRenderer.renderTypeName(newField.fieldType)

    val withType =
      if (oldFieldType != newFieldType)
        withDefault :+ SchemaChange.InputFieldTypeChanged(
          newType,
          newField,
          SchemaRenderer.renderTypeName(nonContainer(newField)) != oldFieldType,
          oldField.fieldType,
          newField.fieldType)
      else
        withDefault

    val directiveChanges = findInAstDirs(
      oldField.astDirectives,
      newField.astDirectives,
      added = SchemaChange.InputFieldAstDirectiveAdded(newType, newField, _),
      removed = SchemaChange.InputFieldAstDirectiveRemoved(newType, newField, _)
    )

    withType ++ directiveChanges
  }

  private def isOptional(field: InputField[_]) = field.fieldType match {
    case _: OptionInputType[_] => true
    case _ => false
  }

  private def isOptional(argument: Argument[_]) = argument.argumentType match {
    case _: OptionInputType[_] => true
    case _ => false
  }

  private def nonContainer(field: InputField[_]) = field.fieldType match {
    case OptionInputType(ofType) => ofType
    case tpe => tpe
  }

  private def nonContainer[F[_]](field: Field[_, _, F]) = field.fieldType match {
    case OptionType(ofType) => ofType
    case tpe => tpe
  }

  private def nonContainer(argument: Argument[_]) = argument.argumentType match {
    case OptionInputType(ofType) => ofType
    case tpe => tpe
  }

  private def findDescriptionChanged(
      o: HasDescription,
      n: HasDescription,
      fn: (Option[String], Option[String]) => SchemaChange): Vector[SchemaChange] =
    if (o.description != n.description) Vector(fn(o.description, n.description))
    else Vector.empty

  private def findDeprecationChanged(
      o: HasDeprecation,
      n: HasDeprecation,
      fn: (Option[String], Option[String]) => SchemaChange): Vector[SchemaChange] =
    if (o.deprecationReason != n.deprecationReason)
      Vector(fn(o.deprecationReason, n.deprecationReason))
    else Vector.empty

  private val coercionHelper = new ValueCoercionHelper[Any]
}

sealed trait SchemaChange {
  def breakingChange: Boolean
  def dangerousChange: Boolean
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

  sealed trait DescriptionChange {
    def oldDescription: Option[String]
    def newDescription: Option[String]
  }

  sealed abstract class AbstractChange(
      val description: String,
      val breakingChange: Boolean,
      val dangerousChange: Boolean = false)
      extends SchemaChange

  // Breaking changes

  case class TypeRemoved(tpe: Type with Named)
      extends AbstractChange(s"`${tpe.name}` type was removed", true)
      with TypeChange

  case class DirectiveRemoved(directive: Directive)
      extends AbstractChange(s"`${directive.name}` directive was removed", true)

  case class TypeKindChanged(tpe: Type with Named, oldTpe: Type with Named)
      extends AbstractChange(
        {
          val oldKind = kind(oldTpe)
          val newKind = kind(tpe)

          s"`${tpe.name}` changed from ${article(oldKind)} $oldKind type to ${article(newKind)} $newKind type"
        },
        true)
      with TypeChange

  case class EnumValueRemoved(tpe: EnumType[_], value: EnumValue[_])
      extends AbstractChange(
        s"Enum value `${value.name}` was removed from enum `${tpe.name}`",
        true)
      with TypeChange

  case class UnionMemberRemoved[F[_]](tpe: UnionType[_, F], member: ObjectType[_, _, F])
      extends AbstractChange(s"`${member.name}` type was removed from union `${tpe.name}`", true)
      with TypeChange

  case class InputFieldRemoved(tpe: InputObjectType[_], field: InputField[_])
      extends AbstractChange(
        s"Input field `${field.name}` was removed from `${tpe.name}` type",
        true)
      with TypeChange

  case class ObjectTypeArgumentRemoved[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      argument: Argument[_])
      extends AbstractChange(
        s"Argument `${argument.name}` was removed from `${tpe.name}.${field.name}` field",
        true)
      with TypeChange

  case class DirectiveArgumentRemoved(directive: Directive, argument: Argument[_])
      extends AbstractChange(
        s"Argument `${argument.name}` was removed from `${directive.name}` directive",
        true)

  case class SchemaQueryTypeChanged[F[_]](
      oldType: ObjectType[_, _, F],
      newType: ObjectType[_, _, F])
      extends AbstractChange(
        s"Schema query type changed from `${oldType.name}` to `${newType.name}` type",
        true)

  case class FieldRemoved[F[_]](tpe: ObjectLikeType[_, _, F], field: Field[_, _, F])
      extends AbstractChange(s"Field `${field.name}` was removed from `${tpe.name}` type", true)
      with TypeChange

  case class DirectiveLocationRemoved(directive: Directive, location: DirectiveLocation.Value)
      extends AbstractChange(
        s"`$location` directive location removed from `${directive.name}` directive",
        true)

  case class ObjectTypeInterfaceRemoved[F[_]](
      tpe: ObjectType[_, _, F],
      interface: InterfaceType[_, _, F])
      extends AbstractChange(
        s"`${tpe.name}` object type no longer implements `${interface.name}` interface",
        true)
      with TypeChange

  // Non-breaking changes

  case class TypeAdded(tpe: Type with Named)
      extends AbstractChange(s"`${tpe.name}` type was added", false)
      with TypeChange

  case class DirectiveAdded(directive: Directive)
      extends AbstractChange(s"`${directive.name}` directive was added", false)

  case class TypeDescriptionChanged(
      tpe: Type with Named,
      oldDescription: Option[String],
      newDescription: Option[String])
      extends AbstractChange(s"`${tpe.name}` type description is changed", false)
      with DescriptionChange
      with TypeChange

  case class EnumValueAdded(tpe: EnumType[_], value: EnumValue[_])
      extends AbstractChange(
        s"Enum value `${value.name}` was added to enum `${tpe.name}`",
        false,
        true)
      with TypeChange

  case class EnumValueDescriptionChanged(
      tpe: EnumType[_],
      value: EnumValue[_],
      oldDescription: Option[String],
      newDescription: Option[String])
      extends AbstractChange(s"`${tpe.name}.${value.name}` description changed", false)
      with DescriptionChange
      with TypeChange

  case class SchemaDescriptionChanged(
      oldDescription: Option[String],
      newDescription: Option[String])
      extends AbstractChange(s"Schema description changed", false)
      with DescriptionChange

  case class EnumValueDeprecated(
      tpe: EnumType[_],
      value: EnumValue[_],
      oldDeprecationReason: Option[String],
      newDeprecationReason: Option[String])
      extends AbstractChange(
        s"Enum value `${value.name}` was deprecated in enum `${tpe.name}`",
        false)
      with DeprecationChange

  case class UnionMemberAdded[F[_]](tpe: UnionType[_, F], member: ObjectType[_, _, F])
      extends AbstractChange(s"`${member.name}` type was added to union `${tpe.name}`", false, true)
      with TypeChange

  case class InputFieldDescriptionChanged(
      tpe: InputObjectType[_],
      field: InputField[_],
      oldDescription: Option[String],
      newDescription: Option[String])
      extends AbstractChange(s"`${tpe.name}.${field.name}` description is changed", false)
      with DescriptionChange
      with TypeChange

  case class DirectiveDescriptionChanged(
      directive: Directive,
      oldDescription: Option[String],
      newDescription: Option[String])
      extends AbstractChange(s"`${directive.name}` directive description is changed", false)

  case class FieldDescriptionChanged[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      oldDescription: Option[String],
      newDescription: Option[String])
      extends AbstractChange(s"`${tpe.name}.${field.name}` description is changed", false)
      with DescriptionChange
      with TypeChange

  case class ObjectTypeArgumentDescriptionChanged[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      argument: Argument[_],
      oldDescription: Option[String],
      newDescription: Option[String])
      extends AbstractChange(
        s"`${tpe.name}.${field.name}(${argument.name})` description is changed",
        false)
      with DescriptionChange
      with TypeChange

  case class DirectiveArgumentDescriptionChanged(
      directive: Directive,
      argument: Argument[_],
      oldDescription: Option[String],
      newDescription: Option[String])
      extends AbstractChange(s"`${directive.name}(${argument.name})` description is changed", false)
      with DescriptionChange

  case class FieldDeprecationChanged[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      oldDeprecationReason: Option[String],
      newDeprecationReason: Option[String])
      extends AbstractChange(s"Field `${field.name}` was deprecated in `${tpe.name}` type", false)
      with DeprecationChange

  case class InputFieldDefaultChanged(
      tpe: InputObjectType[_],
      field: InputField[_],
      oldDefault: Option[ast.Value],
      newDefault: Option[ast.Value])
      extends AbstractChange(
        s"`${tpe.name}.${field.name}` default value changed from ${oldDefault.fold("none")(d =>
          "`" + d.renderCompact + "`")} to ${newDefault.fold("none")(d => "`" + d.renderCompact + "`")}",
        false)
      with TypeChange

  case class ObjectTypeArgumentDefaultChanged[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      argument: Argument[_],
      oldDefault: Option[ast.Value],
      newDefault: Option[ast.Value])
      extends AbstractChange(
        s"`${tpe.name}.${field.name}(${argument.name})` default value changed from ${oldDefault
          .fold("none")(d => "`" + d.renderCompact + "`")} to ${newDefault.fold("none")(d =>
          "`" + d.renderCompact + "`")}",
        false,
        true)
      with TypeChange

  case class DirectiveArgumentDefaultChanged(
      directive: Directive,
      argument: Argument[_],
      oldDefault: Option[ast.Value],
      newDefault: Option[ast.Value])
      extends AbstractChange(
        s"`${directive.name}(${argument.name})` default value changed from ${oldDefault.fold("none")(
          d => "`" + d.renderCompact + "`")} to ${newDefault.fold("none")(d => "`" + d.renderCompact + "`")}",
        false,
        true)

  case class ObjectTypeInterfaceAdded[F[_]](
      tpe: ObjectType[_, _, F],
      interface: InterfaceType[_, _, F])
      extends AbstractChange(
        s"`${tpe.name}` object type now implements `${interface.name}` interface",
        false,
        true)
      with TypeChange

  case class FieldAdded[F[_]](tpe: ObjectLikeType[_, _, F], field: Field[_, _, F])
      extends AbstractChange(s"Field `${field.name}` was added to `${tpe.name}` type", false)
      with TypeChange

  case class DirectiveLocationAdded(directive: Directive, location: DirectiveLocation.Value)
      extends AbstractChange(
        s"`$location` directive location added to `${directive.name}` directive",
        false)

  trait AstDirectiveAdded extends SchemaChange {
    def directive: ast.Directive
    def location: DirectiveLocation.Value
  }

  trait AstDirectiveRemoved extends SchemaChange {
    def directive: ast.Directive
    def location: DirectiveLocation.Value
  }

  abstract class AbstractAstDirectiveAdded(
      val description: String,
      val location: DirectiveLocation.Value)
      extends AstDirectiveAdded {
    val breakingChange = false
    val dangerousChange = false
  }

  abstract class AbstractAstDirectiveRemoved(
      val description: String,
      val location: DirectiveLocation.Value)
      extends AstDirectiveRemoved {
    val breakingChange = false
    val dangerousChange = false
  }

  case class FieldAstDirectiveAdded[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on a field `${tpe.name}.${field.name}`",
        DirectiveLocation.FieldDefinition)

  case class FieldAstDirectiveRemoved[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from a field `${tpe.name}.${field.name}`",
        DirectiveLocation.FieldDefinition)

  case class EnumValueAstDirectiveAdded(
      tpe: EnumType[_],
      value: EnumValue[_],
      directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on an enum value `${tpe.name}.${value.name}`",
        DirectiveLocation.EnumValue)

  case class EnumValueAstDirectiveRemoved(
      tpe: EnumType[_],
      value: EnumValue[_],
      directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from a enum value `${tpe.name}.${value.name}`",
        DirectiveLocation.EnumValue)

  case class InputFieldAstDirectiveAdded(
      tpe: InputObjectType[_],
      field: InputField[_],
      directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on an input field `${tpe.name}.${field.name}`",
        DirectiveLocation.InputFieldDefinition)

  case class InputFieldAstDirectiveRemoved(
      tpe: InputObjectType[_],
      field: InputField[_],
      directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from a input field `${tpe.name}.${field.name}`",
        DirectiveLocation.InputFieldDefinition)

  case class DirectiveArgumentAstDirectiveAdded(
      dir: Directive,
      argument: Argument[_],
      directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on a directive argument `${dir.name}.${argument.name}`",
        DirectiveLocation.ArgumentDefinition)

  case class DirectiveArgumentAstDirectiveRemoved(
      dir: Directive,
      argument: Argument[_],
      directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from a directive argument `${dir.name}.${argument.name}`",
        DirectiveLocation.ArgumentDefinition)

  case class FieldArgumentAstDirectiveAdded[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      argument: Argument[_],
      directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on a field argument `${tpe.name}.${field.name}[${argument.name}]`",
        DirectiveLocation.ArgumentDefinition)

  case class FieldArgumentAstDirectiveRemoved[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      argument: Argument[_],
      directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from a field argument `${tpe.name}.${field.name}[${argument.name}]`",
        DirectiveLocation.ArgumentDefinition)

  case class ObjectTypeAstDirectiveAdded[F[_]](tpe: ObjectType[_, _, F], directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on an object type `${tpe.name}`",
        DirectiveLocation.Object)

  case class ObjectTypeAstDirectiveRemoved[F[_]](tpe: ObjectType[_, _, F], directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from an object type `${tpe.name}`",
        DirectiveLocation.Object)

  case class InterfaceTypeAstDirectiveAdded[F[_]](
      tpe: InterfaceType[_, _, F],
      directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on an interface type `${tpe.name}`",
        DirectiveLocation.Interface)

  case class InterfaceTypeAstDirectiveRemoved[F[_]](
      tpe: InterfaceType[_, _, F],
      directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from an interface type `${tpe.name}`",
        DirectiveLocation.Interface)

  case class UnionTypeAstDirectiveAdded[F[_]](tpe: UnionType[_, F], directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on a union type `${tpe.name}`",
        DirectiveLocation.Union)

  case class UnionTypeAstDirectiveRemoved[F[_]](tpe: UnionType[_, F], directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from a union type `${tpe.name}`",
        DirectiveLocation.Union)

  case class EnumTypeAstDirectiveAdded(tpe: EnumType[_], directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on an enum type `${tpe.name}`",
        DirectiveLocation.Enum)

  case class EnumTypeAstDirectiveRemoved(tpe: EnumType[_], directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from an enum type `${tpe.name}`",
        DirectiveLocation.Enum)

  case class ScalarTypeAstDirectiveAdded(tpe: ScalarType[_], directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on a scalar type `${tpe.name}`",
        DirectiveLocation.Scalar)

  case class ScalarTypeAstDirectiveRemoved(tpe: ScalarType[_], directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from a scalar type `${tpe.name}`",
        DirectiveLocation.Scalar)

  case class InputObjectTypeAstDirectiveAdded(tpe: InputObjectType[_], directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on an input type `${tpe.name}`",
        DirectiveLocation.InputObject)

  case class InputObjectTypeAstDirectiveRemoved(tpe: InputObjectType[_], directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from an input type `${tpe.name}`",
        DirectiveLocation.InputObject)

  case class SchemaAstDirectiveAdded[F[_]](schema: Schema[_, _, F], directive: ast.Directive)
      extends AbstractAstDirectiveAdded(
        s"Directive `${directive.renderCompact}` added on a schema",
        DirectiveLocation.Schema)

  case class SchemaAstDirectiveRemoved[F[_]](schema: Schema[_, _, F], directive: ast.Directive)
      extends AbstractAstDirectiveRemoved(
        s"Directive `${directive.renderCompact}` removed from a schema",
        DirectiveLocation.Schema)

  // May be a breaking change

  case class InputFieldAdded(tpe: InputObjectType[_], field: InputField[_], breaking: Boolean)
      extends AbstractChange(
        s"Input field `${field.name}` was added to `${tpe.name}` type",
        breaking)
      with TypeChange

  case class ObjectTypeArgumentAdded[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      argument: Argument[_],
      breaking: Boolean)
      extends AbstractChange(
        s"Argument `${argument.name}` was added to `${tpe.name}.${field.name}` field",
        breaking)
      with TypeChange

  case class DirectiveArgumentAdded(directive: Directive, argument: Argument[_], breaking: Boolean)
      extends AbstractChange(
        s"Argument `${argument.name}` was added to `${directive.name}` directive",
        breaking)

  case class InputFieldTypeChanged(
      tpe: InputObjectType[_],
      field: InputField[_],
      breaking: Boolean,
      oldFieldType: InputType[_],
      newFieldType: InputType[_])
      extends AbstractChange(
        s"`${tpe.name}.${field.name}` input field type changed from `${SchemaRenderer
          .renderTypeName(oldFieldType)}` to `${SchemaRenderer.renderTypeName(newFieldType)}`",
        breaking)
      with TypeChange

  case class ObjectTypeArgumentTypeChanged[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      argument: Argument[_],
      breaking: Boolean,
      oldFieldType: InputType[_],
      newFieldType: InputType[_])
      extends AbstractChange(
        s"`${tpe.name}.${field.name}(${argument.name})` type changed from `${SchemaRenderer
          .renderTypeName(oldFieldType)}` to `${SchemaRenderer.renderTypeName(newFieldType)}`",
        breaking)
      with TypeChange

  case class DirectiveArgumentTypeChanged(
      directive: Directive,
      argument: Argument[_],
      breaking: Boolean,
      oldFieldType: InputType[_],
      newFieldType: InputType[_])
      extends AbstractChange(
        s"`${directive.name}(${argument.name})` type changed from `${SchemaRenderer.renderTypeName(
          oldFieldType)}` to `${SchemaRenderer.renderTypeName(newFieldType)}`",
        breaking)

  case class FieldTypeChanged[F[_]](
      tpe: ObjectLikeType[_, _, F],
      field: Field[_, _, F],
      breaking: Boolean,
      oldFieldType: OutputType[_],
      newFieldType: OutputType[_])
      extends AbstractChange(
        s"`${tpe.name}.${field.name}` field type changed from `${SchemaRenderer.renderTypeName(
          oldFieldType)}` to `${SchemaRenderer.renderTypeName(newFieldType)}`",
        breaking)
      with TypeChange

  case class SchemaMutationTypeChanged[F[_]](
      oldType: Option[ObjectType[_, _, F]],
      newType: Option[ObjectType[_, _, F]])
      extends AbstractChange(
        s"Schema mutation type changed from ${oldType.fold("none")(t =>
          "`" + t.name + "`")} to ${newType.fold("none")(t => "`" + t.name + "`")} type",
        oldType.nonEmpty)

  case class SchemaSubscriptionTypeChanged[F[_]](
      oldType: Option[ObjectType[_, _, F]],
      newType: Option[ObjectType[_, _, F]])
      extends AbstractChange(
        s"Schema subscription type changed from ${oldType.fold("none")(t =>
          "`" + t.name + "`")} to ${newType.fold("none")(t => "`" + t.name + "`")} type",
        oldType.nonEmpty)

  private val AnArticleLetters = Set('a', 'e', 'i', 'o')

  private def kind(tpe: Type) = tpe match {
    case _: ObjectType[_, _, _] => "Object"
    case _: InterfaceType[_, _, _] => "Interface"
    case _: ScalarType[_] => "Scalar"
    case _: ScalarAlias[_, _] => "Scalar"
    case _: UnionType[_, _] => "Union"
    case _: EnumType[_] => "Enum"
    case _: InputObjectType[_] => "InputObject"
    case t => throw new IllegalStateException(s"Unsupported type kind: $t")
  }

  private def article(word: String) =
    if (AnArticleLetters contains word.head.toLower) "an"
    else "a"
}
