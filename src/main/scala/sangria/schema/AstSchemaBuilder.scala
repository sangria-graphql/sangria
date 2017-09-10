package sangria.schema

import sangria.ast
import sangria.execution.FieldTag
import sangria.marshalling.{FromInput, MarshallerCapability, ScalarValueInfo, ToInput}
import sangria.validation.Violation

import scala.reflect.ClassTag

trait AstSchemaBuilder[Ctx] {
  def additionalTypeExtensionDefs: List[ast.TypeExtensionDefinition]
  def additionalTypes: List[MaterializedType]
  def additionalDirectiveDefs: List[ast.DirectiveDefinition]

  def resolveNameConflict(fromOrigin: MatOrigin, types: Vector[MaterializedType]): MaterializedType

  def buildSchema(
    definition: Option[ast.SchemaDefinition],
    queryType: ObjectType[Ctx, Any],
    mutationType: Option[ObjectType[Ctx, Any]],
    subscriptionType: Option[ObjectType[Ctx, Any]],
    additionalTypes: List[Type with Named],
    directives: List[Directive],
    mat: AstSchemaMaterializer[Ctx]): Schema[Ctx, Any]

  def extendSchema[Val](
    originalSchema: Schema[Ctx, Val],
    queryType: ObjectType[Ctx, Val],
    mutationType: Option[ObjectType[Ctx, Val]],
    subscriptionType: Option[ObjectType[Ctx, Val]],
    additionalTypes: List[Type with Named],
    directives: List[Directive],
    mat: AstSchemaMaterializer[Ctx]): Schema[Ctx, Val]

  def buildObjectType(
    origin: MatOrigin,
    definition: ast.ObjectTypeDefinition,
    extensions: List[ast.TypeExtensionDefinition],
    fields: () ⇒ List[Field[Ctx, Any]],
    interfaces: List[InterfaceType[Ctx, Any]],
    mat: AstSchemaMaterializer[Ctx]): Option[ObjectType[Ctx, Any]]

  def extendObjectType(
    origin: MatOrigin,
    existing: ObjectType[Ctx, _],
    extensions: List[ast.TypeExtensionDefinition],
    fields: () ⇒ List[Field[Ctx, Any]],
    interfaces: List[InterfaceType[Ctx, Any]],
    mat: AstSchemaMaterializer[Ctx]): ObjectType[Ctx, Any]

  def buildInputObjectType(
    origin: MatOrigin,
    definition: ast.InputObjectTypeDefinition,
    fields: () ⇒ List[InputField[_]],
    mat: AstSchemaMaterializer[Ctx]): Option[InputObjectType[InputObjectType.DefaultInput]]

  def transformInputObjectType[T](
    origin: MatOrigin,
    existing: InputObjectType[T],
    mat: AstSchemaMaterializer[Ctx]): InputObjectType[T]

  def buildInterfaceType(
    origin: MatOrigin,
    definition: ast.InterfaceTypeDefinition,
    extensions: List[ast.TypeExtensionDefinition],
    fields: () ⇒ List[Field[Ctx, Any]],
    mat: AstSchemaMaterializer[Ctx]): Option[InterfaceType[Ctx, Any]]

  def extendInterfaceType(
    origin: MatOrigin,
    existing: InterfaceType[Ctx, _],
    extensions: List[ast.TypeExtensionDefinition],
    fields: () ⇒ List[Field[Ctx, Any]],
    mat: AstSchemaMaterializer[Ctx]): InterfaceType[Ctx, Any]

  def buildUnionType(
    origin: MatOrigin,
    definition: ast.UnionTypeDefinition,
    types: List[ObjectType[Ctx, _]],
    mat: AstSchemaMaterializer[Ctx]): Option[UnionType[Ctx]]

  def extendUnionType(
    origin: MatOrigin,
    existing: UnionType[Ctx],
    types: List[ObjectType[Ctx, _]],
    mat: AstSchemaMaterializer[Ctx]): UnionType[Ctx]

  def extendScalarAlias[T, ST](
    origin: MatOrigin,
    existing: ScalarAlias[T, ST],
    aliasFor: ScalarType[ST],
    mat: AstSchemaMaterializer[Ctx]): ScalarAlias[T, ST]

  def buildScalarType(
    origin: MatOrigin,
    definition: ast.ScalarTypeDefinition,
    mat: AstSchemaMaterializer[Ctx]): Option[ScalarType[Any]]

  def transformScalarType[T](
    origin: MatOrigin,
    existing: ScalarType[T],
    mat: AstSchemaMaterializer[Ctx]): ScalarType[T]

  def buildEnumType(
    origin: MatOrigin,
    definition: ast.EnumTypeDefinition,
    values: List[EnumValue[Any]],
    mat: AstSchemaMaterializer[Ctx]): Option[EnumType[Any]]

  def transformEnumType[T](
    origin: MatOrigin,
    existing: EnumType[T],
    mat: AstSchemaMaterializer[Ctx]): EnumType[T]

  def buildField(
    origin: MatOrigin,
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.TypeExtensionDefinition],
    definition: ast.FieldDefinition,
    fieldType: OutputType[_],
    arguments: List[Argument[_]],
    mat: AstSchemaMaterializer[Ctx]): Option[Field[Ctx, Any]]

  def buildAdditionalFields(
    origin: MatOrigin,
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.TypeExtensionDefinition],
    mat: AstSchemaMaterializer[Ctx]): List[MaterializedField[Ctx, Any]]

  def buildFieldType(
    origin: MatOrigin,
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.TypeExtensionDefinition],
    definition: ast.FieldDefinition,
    arguments: List[Argument[_]],
    mat: AstSchemaMaterializer[Ctx]): OutputType[Any]

  def extendField(
    origin: MatOrigin,
    typeDefinition: Option[ObjectLikeType[Ctx, _]],
    existing: Field[Ctx, Any],
    fieldType: OutputType[_],
    mat: AstSchemaMaterializer[Ctx]): Field[Ctx, Any]

  def extendFieldType(
    origin: MatOrigin,
    typeDefinition: Option[ObjectLikeType[Ctx, _]],
    existing: Field[Ctx, Any],
    mat: AstSchemaMaterializer[Ctx]): OutputType[Any]

  def buildInputField(
    origin: MatOrigin,
    typeDefinition: ast.InputObjectTypeDefinition,
    definition: ast.InputValueDefinition,
    tpe: InputType[_],
    defaultValue: Option[(_, ToInput[_, _])],
    mat: AstSchemaMaterializer[Ctx]): Option[InputField[Any]]

  def buildInputFieldType(
    origin: MatOrigin,
    typeDefinition: ast.InputObjectTypeDefinition,
    definition: ast.InputValueDefinition,
    defaultValue: Option[(_, ToInput[_, _])],
    mat: AstSchemaMaterializer[Ctx]): InputType[Any]

  def buildArgument(
    origin: MatOrigin,
    typeDefinition: ast.TypeSystemDefinition,
    fieldDefinition: Option[ast.FieldDefinition],
    definition: ast.InputValueDefinition,
    tpe: InputType[_],
    defaultValue: Option[(_, ToInput[_, _])],
    mat: AstSchemaMaterializer[Ctx]): Option[Argument[Any]]

  def buildArgumentType(
    origin: MatOrigin,
    typeDefinition: ast.TypeSystemDefinition,
    fieldDefinition: Option[ast.FieldDefinition],
    definition: ast.InputValueDefinition,
    defaultValue: Option[(_, ToInput[_, _])],
    mat: AstSchemaMaterializer[Ctx]): InputType[Any]

  def buildEnumValue(
    origin: MatOrigin,
    typeDefinition: ast.EnumTypeDefinition,
    definition: ast.EnumValueDefinition,
    mat: AstSchemaMaterializer[Ctx]): Option[EnumValue[Any]]

  def buildDirective(
    origin: MatOrigin,
    definition: ast.DirectiveDefinition,
    arguments: List[Argument[_]],
    locations: Set[DirectiveLocation.Value],
    mat: AstSchemaMaterializer[Ctx]): Option[Directive]

  def transformDirective(
    origin: MatOrigin,
    existing: Directive,
    mat: AstSchemaMaterializer[Ctx]): Directive
}

object AstSchemaBuilder {
  def default[Ctx] = new DefaultAstSchemaBuilder[Ctx]
  def resolverBased[Ctx](resolvers: AstSchemaResolver[Ctx]*) = new ResolverBasedAstSchemaBuilder[Ctx](resolvers)

  object TypeName {
    def unapply(definition: ast.TypeDefinition): Option[String] =
      Some(definition.name)

    def unapply(named: Named): Option[String] =
      Some(named.name)
  }

  object FieldName {
    def unapply(definition: ast.FieldDefinition): Option[String] =
      Some(definition.name)

    def unapply(field: Field[_, _]): Option[String] =
      Some(field.name)
  }

  object ScalarName {
    def unapply(definition: ast.ScalarTypeDefinition): Option[String] =
      Some(definition.name)
  }

  def extractDescription(node: ast.WithComments): Option[String] =
    if (node.comments.nonEmpty) {
      node.position.map(_.line).orElse(node.comments.last.position.map(_.line + 1)) match {
        case Some(nodeLine) ⇒
          val (_, relevantComments) = node.comments.foldRight((nodeLine - 1, Vector.empty[String])) {
            case (c, (expectedLine, acc)) if c.position.isDefined && c.position.get.line == expectedLine ⇒
              (expectedLine - 1) → (c.text +: acc)
            case (c, acc ) ⇒ acc
          }

          extractDescription(relevantComments)
        case None ⇒
          extractDescription(node.comments map (_.text))
      }
    } else None

  def extractDescription(comments: Seq[String]): Option[String] =
    if (comments.nonEmpty)
      Some(comments.map(_.trim) mkString "\n")
    else
      None
}

class DefaultAstSchemaBuilder[Ctx] extends AstSchemaBuilder[Ctx] {
  def additionalDirectiveDefs: List[ast.DirectiveDefinition] = Nil
  def additionalTypeExtensionDefs: List[ast.TypeExtensionDefinition] = Nil
  def additionalTypes: List[MaterializedType] = Nil

  def resolveNameConflict(fromOrigin: MatOrigin, types: Vector[MaterializedType]): MaterializedType =
    throw SchemaMaterializationException(s"Conflicting types with name '${types.head.name}' found in ${types.map(_.origin) mkString ", "}")

  def buildSchema(
      definition: Option[ast.SchemaDefinition],
      queryType: ObjectType[Ctx, Any],
      mutationType: Option[ObjectType[Ctx, Any]],
      subscriptionType: Option[ObjectType[Ctx, Any]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx]) =
    Schema[Ctx, Any](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      directives = directives,
      astDirectives = definition.fold(Vector.empty[ast.Directive])(_.directives))

  def extendSchema[Val](
      originalSchema: Schema[Ctx, Val],
      queryType: ObjectType[Ctx, Val],
      mutationType: Option[ObjectType[Ctx, Val]],
      subscriptionType: Option[ObjectType[Ctx, Val]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx]) =
    Schema[Ctx, Val](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      directives = directives,
      validationRules = originalSchema.validationRules)

  def buildObjectType(
      origin: MatOrigin,
      definition: ast.ObjectTypeDefinition,
      extensions: List[ast.TypeExtensionDefinition],
      fields: () ⇒ List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]) = {
    val directives = definition.directives ++ extensions.flatMap(_.definition.directives)

    val objectType =
      objectTypeInstanceCheck(origin, definition, extensions) match {
        case Some(fn) ⇒
          ObjectType[Ctx, Any](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = Named.checkObjFields(typeName(definition), fields),
            interfaces = interfaces,
            instanceCheck = (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any]) ⇒ fn(value, clazz),
            astDirectives = directives)
        case None ⇒
          ObjectType[Ctx, Any](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = Named.checkObjFields(typeName(definition), fields),
            interfaces = interfaces,
            instanceCheck = ObjectType.defaultInstanceCheck[Ctx, Any],
            astDirectives = directives)
      }

    Some(objectType)
  }

  def extendObjectType(
      origin: MatOrigin,
      existing: ObjectType[Ctx, _],
      extensions: List[ast.TypeExtensionDefinition],
      fields: () ⇒ List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]) =
    extendedObjectTypeInstanceCheck(origin, existing, extensions) match {
      case Some(fn) ⇒
        existing.copy(
          fieldsFn = Named.checkObjFields(existing.name, fields),
          interfaces = interfaces,
          instanceCheck = (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any]) ⇒ fn(value, clazz))(ClassTag(existing.valClass))
      case None ⇒
        existing.copy(
          fieldsFn = Named.checkObjFields(existing.name, fields),
          interfaces = interfaces,
          instanceCheck = existing.instanceCheck.asInstanceOf[(Any, Class[_], ObjectType[Ctx, _]) ⇒ Boolean])(ClassTag(existing.valClass))
    }

  def buildInputObjectType(
      origin: MatOrigin,
      definition: ast.InputObjectTypeDefinition,
      fields: () ⇒ List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(InputObjectType(
      name = typeName(definition),
      description = typeDescription(definition),
      fieldsFn = Named.checkIntFields(typeName(definition), fields),
      astDirectives = definition.directives))

  def buildInterfaceType(
      origin: MatOrigin,
      definition: ast.InterfaceTypeDefinition,
      extensions: List[ast.TypeExtensionDefinition],
      fields: () ⇒ List[Field[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]) = {
    val directives = definition.directives ++ extensions.flatMap(_.definition.directives)

    Some(InterfaceType[Ctx, Any](
      name = typeName(definition),
      description = typeDescription(definition),
      fieldsFn = Named.checkIntFields(typeName(definition), fields),
      interfaces = Nil,
      manualPossibleTypes = () ⇒ Nil,
      astDirectives = directives))
  }

  def extendInterfaceType(
      origin: MatOrigin,
      existing: InterfaceType[Ctx, _],
      extensions: List[ast.TypeExtensionDefinition],
      fields: () ⇒ List[Field[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]) =
    existing.copy(fieldsFn = fields, manualPossibleTypes = () ⇒ Nil, interfaces = Nil)

  def buildUnionType(
      origin: MatOrigin,
      definition: ast.UnionTypeDefinition,
      types: List[ObjectType[Ctx, _]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(UnionType[Ctx](
      name = typeName(definition),
      description = typeDescription(definition),
      types = types,
      astDirectives = definition.directives))

  def extendUnionType(
      origin: MatOrigin,
      existing: UnionType[Ctx],
      types: List[ObjectType[Ctx, _]],
      mat: AstSchemaMaterializer[Ctx]) =
    existing.copy(types = types)

  def extendScalarAlias[T, ST](
      origin: MatOrigin,
      existing: ScalarAlias[T, ST],
      aliasFor: ScalarType[ST],
      mat: AstSchemaMaterializer[Ctx]) =
    existing.copy(aliasFor = aliasFor)

  def buildScalarType(
      origin: MatOrigin,
      definition: ast.ScalarTypeDefinition,
      mat: AstSchemaMaterializer[Ctx]) =
    Some(ScalarType[Any](
      name = typeName(definition),
      description = typeDescription(definition),
      coerceUserInput = scalarCoerceUserInput(definition),
      coerceOutput = scalarCoerceOutput(definition),
      coerceInput = scalarCoerceInput(definition),
      complexity = scalarComplexity(definition),
      scalarInfo = scalarValueInfo(definition),
      astDirectives = definition.directives))

  def buildEnumType(
      origin: MatOrigin,
      definition: ast.EnumTypeDefinition,
      values: List[EnumValue[Any]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(EnumType[Any](
      name = typeName(definition),
      description = typeDescription(definition),
      values = values,
      astDirectives = definition.directives))

  def buildEnumValue(
      origin: MatOrigin,
      typeDefinition: ast.EnumTypeDefinition,
      definition: ast.EnumValueDefinition,
      mat: AstSchemaMaterializer[Ctx]) =
    Some(EnumValue[String](
      name = enumValueName(definition),
      description = enumValueDescription(definition),
      value = enumValue(definition),
      deprecationReason = enumValueDeprecationReason(definition),
      astDirectives = definition.directives))

  def buildField(
      origin: MatOrigin,
      typeDefinition: ast.TypeDefinition,
      extensions: Vector[ast.TypeExtensionDefinition],
      definition: ast.FieldDefinition,
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(Field[Ctx, Any](
      name = fieldName(definition),
      description = fieldDescription(definition),
      fieldType = fieldType,
      arguments = arguments,
      resolve = resolveField(origin, typeDefinition, extensions, definition, mat),
      tags = fieldTags(typeDefinition, definition),
      deprecationReason = fieldDeprecationReason(definition),
      complexity = fieldComplexity(typeDefinition, definition),
      manualPossibleTypes = () ⇒ Nil,
      astDirectives = definition.directives))

  def buildFieldType(
      origin: MatOrigin,
      typeDefinition: ast.TypeDefinition,
      extensions: Vector[ast.TypeExtensionDefinition],
      definition: ast.FieldDefinition,
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]): OutputType[Any] =
    mat.getOutputType(origin, definition.fieldType)

  def buildAdditionalFields(
    origin: MatOrigin,
    typeDefinition: ast.TypeDefinition,
    extensions: Vector[ast.TypeExtensionDefinition],
    mat: AstSchemaMaterializer[Ctx]): List[MaterializedField[Ctx, Any]] = Nil

  def extendField(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      existing: Field[Ctx, Any],
      fieldType: OutputType[_],
      mat: AstSchemaMaterializer[Ctx]) =
    existing.copy(fieldType = fieldType, resolve = extendFieldResolver(origin, typeDefinition, existing, fieldType, mat), manualPossibleTypes = () ⇒ Nil)

  def extendFieldType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      existing: Field[Ctx, Any],
      mat: AstSchemaMaterializer[Ctx]): OutputType[Any] =
    mat.getTypeFromExistingType(origin, existing.fieldType)

  def buildInputField(
      origin: MatOrigin,
      typeDefinition: ast.InputObjectTypeDefinition,
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(InputField(
      name = inputFieldName(definition),
      description = inputFieldDescription(definition),
      fieldType = tpe,
      defaultValue = defaultValue,
      astDirectives = definition.directives))

  def buildInputFieldType(
      origin: MatOrigin,
      typeDefinition: ast.InputObjectTypeDefinition,
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any] =
    mat.getInputType(origin, definition.valueType)

  def buildArgument(
      origin: MatOrigin,
      typeDefinition: ast.TypeSystemDefinition,
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(Argument(
      name = argumentName(definition),
      description = argumentDescription(definition),
      argumentType = tpe,
      defaultValue = defaultValue,
      fromInput = argumentFromInput(typeDefinition, fieldDefinition, definition),
      astDirectives = definition.directives))

  def buildArgumentType(
      origin: MatOrigin,
      typeDefinition: ast.TypeSystemDefinition,
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any] =
    mat.getInputType(origin, definition.valueType)

  def buildDirective(
      origin: MatOrigin,
      definition: ast.DirectiveDefinition,
      arguments: List[Argument[_]],
      locations: Set[DirectiveLocation.Value],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(Directive(
      name = directiveName(definition),
      description = directiveDescription(definition),
      locations = locations,
      arguments = arguments,
      shouldInclude = directiveShouldInclude(definition)))

  def transformInputObjectType[T](
    origin: MatOrigin,
    existing: InputObjectType[T],
    mat: AstSchemaMaterializer[Ctx]) = existing

  def transformEnumType[T](
    origin: MatOrigin,
    existing: EnumType[T],
    mat: AstSchemaMaterializer[Ctx]) = existing

  def transformScalarType[T](
    origin: MatOrigin,
    existing: ScalarType[T],
    mat: AstSchemaMaterializer[Ctx]) = existing

  def transformDirective(
    origin: MatOrigin,
    existing: Directive,
    mat: AstSchemaMaterializer[Ctx]) = existing

  def objectTypeInstanceCheck(origin: MatOrigin, definition: ast.ObjectTypeDefinition, extensions: List[ast.TypeExtensionDefinition]): Option[(Any, Class[_]) ⇒ Boolean] =
    None

  def extendedObjectTypeInstanceCheck(origin: MatOrigin, tpe: ObjectType[Ctx, _], extensions: List[ast.TypeExtensionDefinition]): Option[(Any, Class[_]) ⇒ Boolean] =
    None

  def directiveShouldInclude(definition: ast.DirectiveDefinition): DirectiveContext ⇒ Boolean =
    Function.const(true)

  def argumentFromInput(
      typeDefinition: ast.TypeSystemDefinition,
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition) =
    FromInput.defaultInput[Any]

  def resolveField(
      origin: MatOrigin,
      typeDefinition: ast.TypeDefinition,
      extensions: Vector[ast.TypeExtensionDefinition],
      definition: ast.FieldDefinition,
      mat: AstSchemaMaterializer[Ctx]): Context[Ctx, _] ⇒ Action[Ctx, _] =
    (ctx) ⇒ throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException

  def extendFieldResolver(
    origin: MatOrigin,
    typeDefinition: Option[ObjectLikeType[Ctx, _]],
    existing: Field[Ctx, Any],
    fieldType: OutputType[_],
    mat: AstSchemaMaterializer[Ctx]): Context[Ctx, Any] ⇒ Action[Ctx, _]  = existing.resolve

  def fieldTags(typeDefinition: ast.TypeDefinition, definition: ast.FieldDefinition): List[FieldTag] =
    Nil

  def scalarCoerceUserInput(definition: ast.ScalarTypeDefinition): Any ⇒ Either[Violation, Any] =
    _ ⇒ Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)

  def scalarCoerceInput(definition: ast.ScalarTypeDefinition): ast.Value ⇒ Either[Violation, Any] =
    _ ⇒ Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)

  def scalarCoerceOutput(definition: ast.ScalarTypeDefinition): (Any, Set[MarshallerCapability]) ⇒ Any =
    (_, _) ⇒ throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException

  def scalarValueInfo(definition: ast.ScalarTypeDefinition): Set[ScalarValueInfo] =
    Set.empty

  def scalarComplexity(definition: ast.ScalarTypeDefinition): Double =
    0.0D

  def fieldComplexity(typeDefinition: ast.TypeDefinition, definition: ast.FieldDefinition): Option[(Ctx, Args, Double) ⇒ Double] =
    None

  def enumValueDeprecationReason(definition: ast.EnumValueDefinition): Option[String] =
    deprecationReason(definition.directives.toList)

  def fieldDeprecationReason(definition: ast.FieldDefinition): Option[String] =
    deprecationReason(definition.directives.toList)

  def deprecationReason(dirs: List[ast.Directive]): Option[String] =
    dirs.find(_.name == DeprecatedDirective.name).flatMap { d ⇒
      d.arguments.find(_.name == ReasonArg.name) match {
        case Some(reason) ⇒
          reason.value match {
            case ast.StringValue(value, _, _) ⇒ Some(value)
            case _ ⇒ None
          }
        case None ⇒ Some(DefaultDeprecationReason)
      }
    }

  def typeName(definition: ast.TypeDefinition): String =
    Named.checkName(definition.name)

  def fieldName(definition: ast.FieldDefinition): String =
    Named.checkName(definition.name)

  def enumValueName(definition: ast.EnumValueDefinition): String =
    Named.checkName(definition.name)

  def argumentName(definition: ast.InputValueDefinition): String =
    Named.checkName(definition.name)

  def inputFieldName(definition: ast.InputValueDefinition): String =
    Named.checkName(definition.name)

  def directiveName(definition: ast.DirectiveDefinition): String =
    Named.checkName(definition.name)

  def commentDescription(node: ast.WithComments): Option[String] =
    AstSchemaBuilder.extractDescription(node)

  def typeDescription(definition: ast.TypeDefinition): Option[String] =
    commentDescription(definition)

  def fieldDescription(definition: ast.FieldDefinition): Option[String] =
    commentDescription(definition)

  def argumentDescription(definition: ast.InputValueDefinition): Option[String] =
    commentDescription(definition)

  def inputFieldDescription(definition: ast.InputValueDefinition): Option[String] =
    commentDescription(definition)

  def enumValueDescription(definition: ast.EnumValueDefinition): Option[String] =
    commentDescription(definition)

  def directiveDescription(definition: ast.DirectiveDefinition): Option[String] =
    commentDescription(definition)

  def enumValue(definition: ast.EnumValueDefinition): String =
    definition.name
}