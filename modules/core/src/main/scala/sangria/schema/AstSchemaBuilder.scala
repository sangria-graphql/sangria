package sangria.schema

import sangria.ast
import sangria.execution.FieldTag
import sangria.marshalling.{FromInput, MarshallerCapability, ScalarValueInfo, ToInput}
import sangria.schema.InputObjectType.DefaultInput
import sangria.validation.Violation

import scala.reflect.ClassTag

trait AstSchemaBuilder[Ctx] {
  def additionalTypeExtensionDefs: List[ast.TypeExtensionDefinition]
  def additionalTypes: List[MaterializedType]
  def additionalDirectiveDefs: List[ast.DirectiveDefinition]

  def resolveNameConflict(fromOrigin: MatOrigin, types: Vector[MaterializedType]): MaterializedType

  def buildSchema(
      definition: Option[ast.SchemaDefinition],
      extensions: List[ast.SchemaExtensionDefinition],
      queryType: ObjectType[Ctx, Any],
      mutationType: Option[ObjectType[Ctx, Any]],
      subscriptionType: Option[ObjectType[Ctx, Any]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx]): Schema[Ctx, Any]

  def extendSchema[Val](
      originalSchema: Schema[Ctx, Val],
      extensions: List[ast.SchemaExtensionDefinition],
      queryType: ObjectType[Ctx, Val],
      mutationType: Option[ObjectType[Ctx, Val]],
      subscriptionType: Option[ObjectType[Ctx, Val]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx]): Schema[Ctx, Val]

  def buildObjectType(
      origin: MatOrigin,
      definition: ast.ObjectTypeDefinition,
      extensions: List[ast.ObjectTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]): Option[ObjectType[Ctx, Any]]

  def extendObjectType(
      origin: MatOrigin,
      existing: ObjectType[Ctx, _],
      extensions: List[ast.ObjectTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]): ObjectType[Ctx, Any]

  def buildInputObjectType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      definition: ast.InputObjectTypeDefinition,
      fields: () => List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx]): Option[InputObjectType[InputObjectType.DefaultInput]]

  def transformInputObjectType[T](
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      existing: InputObjectType[T],
      fields: () => List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx]): InputObjectType[T]

  def buildInterfaceType(
      origin: MatOrigin,
      definition: ast.InterfaceTypeDefinition,
      extensions: List[ast.InterfaceTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]): Option[InterfaceType[Ctx, Any]]

  def extendInterfaceType(
      origin: MatOrigin,
      existing: InterfaceType[Ctx, _],
      extensions: List[ast.InterfaceTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]): InterfaceType[Ctx, Any]

  def buildUnionType(
      origin: MatOrigin,
      extensions: Vector[ast.UnionTypeExtensionDefinition],
      definition: ast.UnionTypeDefinition,
      types: List[ObjectType[Ctx, _]],
      mat: AstSchemaMaterializer[Ctx]): Option[UnionType[Ctx]]

  def extendUnionType(
      origin: MatOrigin,
      extensions: Vector[ast.UnionTypeExtensionDefinition],
      existing: UnionType[Ctx],
      types: List[ObjectType[Ctx, _]],
      mat: AstSchemaMaterializer[Ctx]): UnionType[Ctx]

  def extendScalarAlias[T, ST](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarAlias[T, ST],
      aliasFor: ScalarType[ST],
      mat: AstSchemaMaterializer[Ctx]): ScalarAlias[T, ST]

  def buildScalarType(
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      definition: ast.ScalarTypeDefinition,
      mat: AstSchemaMaterializer[Ctx]): Option[ScalarType[Any]]

  def transformScalarType[T](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarType[T],
      mat: AstSchemaMaterializer[Ctx]): ScalarType[T]

  def buildEnumType(
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      definition: ast.EnumTypeDefinition,
      values: List[EnumValue[Any]],
      mat: AstSchemaMaterializer[Ctx]): Option[EnumType[Any]]

  def transformEnumType[T](
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      existing: EnumType[T],
      mat: AstSchemaMaterializer[Ctx]): EnumType[T]

  def buildField(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]): Option[Field[Ctx, Any]]

  def buildAdditionalFields(
      origin: MatOrigin,
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      typeDefinition: ast.TypeDefinition,
      mat: AstSchemaMaterializer[Ctx]): List[MaterializedField[Ctx, Any]]

  def buildFieldType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]): OutputType[Any]

  def extendField(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      existing: Field[Ctx, Any],
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]): Field[Ctx, Any]

  def extendArgument(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      field: Field[Ctx, Any],
      argument: Argument[Any],
      argumentType: InputType[_],
      mat: AstSchemaMaterializer[Ctx]): Argument[Any]

  def extendInputField(
      origin: MatOrigin,
      typeDefinition: InputObjectType[_],
      existing: InputField[Any],
      fieldType: InputType[_],
      mat: AstSchemaMaterializer[Ctx]): InputField[Any]

  def extendFieldType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      existing: Field[Ctx, Any],
      mat: AstSchemaMaterializer[Ctx]): OutputType[Any]

  def extendArgumentType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      field: Field[Ctx, Any],
      existing: Argument[Any],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any]

  def extendInputFieldType(
      origin: MatOrigin,
      typeDefinition: InputObjectType[_],
      existing: InputField[Any],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any]

  def buildInputField(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): Option[InputField[Any]]

  def buildInputFieldType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any]

  def buildArgument(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): Option[Argument[Any]]

  def buildArgumentType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any]

  def buildEnumValue(
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      typeDefinition: Either[ast.EnumTypeDefinition, EnumType[_]],
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
  def resolverBased[Ctx](resolvers: AstSchemaResolver[Ctx]*) =
    new ResolverBasedAstSchemaBuilder[Ctx](resolvers)

  object TypeName {
    def unapply(definition: ast.TypeDefinition): Option[String] =
      Some(definition.name)

    def unapply(named: Named): Option[String] =
      Some(named.name)

    def unapply(definition: Either[ast.TypeDefinition, Named]): Option[String] =
      definition match {
        case Left(t) => Some(t.name)
        case Right(t) => Some(t.name)
      }
  }

  object FieldName {
    def unapply(definition: ast.FieldDefinition): Option[String] =
      Some(definition.name)

    def unapply(field: Field[_, _]): Option[String] =
      Some(field.name)
  }

  def extractDescription(node: ast.WithComments): Option[String] =
    if (node.comments.nonEmpty) {
      node.location.map(_.line).orElse(node.comments.last.location.map(_.line + 1)) match {
        case Some(nodeLine) =>
          val (_, relevantComments) =
            node.comments.foldRight((nodeLine - 1, Vector.empty[String])) {
              case (c, (expectedLine, acc))
                  if c.location.isDefined && c.location.get.line == expectedLine =>
                (expectedLine - 1) -> (c.text +: acc)
              case (_, acc) => acc
            }

          extractDescription(relevantComments)
        case None =>
          extractDescription(node.comments.map(_.text))
      }
    } else None

  def extractDescription(comments: Seq[String]): Option[String] =
    if (comments.nonEmpty)
      Some(comments.map(_.trim).mkString("\n"))
    else
      None
}

class DefaultAstSchemaBuilder[Ctx] extends AstSchemaBuilder[Ctx] {
  def additionalDirectiveDefs: List[ast.DirectiveDefinition] = Nil
  def additionalTypeExtensionDefs: List[ast.ObjectTypeExtensionDefinition] = Nil
  def additionalTypes: List[MaterializedType] = Nil

  def resolveNameConflict(
      fromOrigin: MatOrigin,
      types: Vector[MaterializedType]): MaterializedType =
    throw SchemaMaterializationException(
      s"Conflicting types with name '${types.head.name}' found in ${types.map(_.origin).mkString(", ")}")

  def buildSchema(
      definition: Option[ast.SchemaDefinition],
      extensions: List[ast.SchemaExtensionDefinition],
      queryType: ObjectType[Ctx, Any],
      mutationType: Option[ObjectType[Ctx, Any]],
      subscriptionType: Option[ObjectType[Ctx, Any]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx]): Schema[Ctx, Any] =
    Schema[Ctx, Any](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      description = definition.flatMap(_.description.map(_.value)),
      directives = directives,
      astDirectives =
        (definition.fold(Vector.empty[ast.Directive])(_.directives) ++ extensions.flatMap(
          _.directives)).asInstanceOf[Vector[ast.Directive with OnSchema]],
      astNodes = Vector(mat.document) ++ extensions ++ definition.toVector
    )

  def extendSchema[Val](
      originalSchema: Schema[Ctx, Val],
      extensions: List[ast.SchemaExtensionDefinition],
      queryType: ObjectType[Ctx, Val],
      mutationType: Option[ObjectType[Ctx, Val]],
      subscriptionType: Option[ObjectType[Ctx, Val]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx]): Schema[Ctx, Val] =
    Schema[Ctx, Val](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      directives = directives,
      description = originalSchema.description,
      validationRules = originalSchema.validationRules,
      astDirectives = (originalSchema.astDirectives ++ extensions.flatMap(_.directives))
        .asInstanceOf[Vector[ast.Directive with OnSchema]],
      astNodes = {
        val (docs, other) = originalSchema.astNodes.partition(_.isInstanceOf[ast.Document])
        val newDoc = ast.Document.merge(docs.asInstanceOf[Vector[ast.Document]] :+ mat.document)

        (newDoc +: other) ++ extensions
      }
    )

  def buildObjectType(
      origin: MatOrigin,
      definition: ast.ObjectTypeDefinition,
      extensions: List[ast.ObjectTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]): Option[ObjectType[Ctx, Any]] = {
    val directives = definition.directives ++ extensions.flatMap(_.directives)

    val objectType =
      objectTypeInstanceCheck(origin, definition, extensions) match {
        case Some(fn) =>
          ObjectType[Ctx, Any](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = fields,
            interfaces = interfaces,
            instanceCheck =
              (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any]) => fn(value, clazz),
            astDirectives = directives.asInstanceOf[Vector[ast.Directive with OnObject]],
            astNodes = (definition +: extensions).toVector
          )
        case None =>
          ObjectType[Ctx, Any](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = fields,
            interfaces = interfaces,
            instanceCheck = ObjectType.defaultInstanceCheck[Ctx, Any],
            astDirectives = directives.asInstanceOf[Vector[ast.Directive with OnObject]],
            astNodes = (definition +: extensions).toVector
          )
      }

    Some(objectType)
  }

  def extendObjectType(
      origin: MatOrigin,
      existing: ObjectType[Ctx, _],
      extensions: List[ast.ObjectTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]): ObjectType[Ctx, Any] =
    extendedObjectTypeInstanceCheck(origin, existing, extensions) match {
      case Some(fn) =>
        existing.copy(
          fieldsFn = fields,
          interfaces = interfaces,
          astDirectives = existing.astDirectives ++ extensions.flatMap(
            _.directives.asInstanceOf[Vector[ast.Directive with OnObject]]),
          astNodes = existing.astNodes ++ extensions,
          instanceCheck = (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any]) => fn(value, clazz)
        )(ClassTag(existing.valClass))
      case None =>
        existing.copy(
          fieldsFn = fields,
          interfaces = interfaces,
          astDirectives = existing.astDirectives ++ extensions.flatMap(
            _.directives.asInstanceOf[Vector[ast.Directive with OnObject]]),
          astNodes = existing.astNodes ++ extensions,
          instanceCheck =
            existing.instanceCheck.asInstanceOf[(Any, Class[_], ObjectType[Ctx, _]) => Boolean]
        )(ClassTag(existing.valClass))
    }

  def buildInputObjectType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      definition: ast.InputObjectTypeDefinition,
      fields: () => List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx]): Option[InputObjectType[DefaultInput]] =
    Some(
      InputObjectType(
        name = typeName(definition),
        description = typeDescription(definition),
        fieldsFn = fields,
        astDirectives = (definition.directives ++ extensions.flatMap(_.directives))
          .asInstanceOf[Vector[ast.Directive with OnInputObjectType]],
        astNodes = definition +: extensions
      ))

  def buildInterfaceType(
      origin: MatOrigin,
      definition: ast.InterfaceTypeDefinition,
      extensions: List[ast.InterfaceTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]): Option[InterfaceType[Ctx, Any]] = {
    val directives = definition.directives ++ extensions.flatMap(_.directives)

    Some(
      InterfaceType[Ctx, Any](
        name = typeName(definition),
        description = typeDescription(definition),
        fieldsFn = fields,
        interfaces = Nil,
        manualPossibleTypes = () => Nil,
        astDirectives = directives.asInstanceOf[Vector[ast.Directive with OnInterface]],
        astNodes = (definition +: extensions).toVector
      ))
  }

  def extendInterfaceType(
      origin: MatOrigin,
      existing: InterfaceType[Ctx, _],
      extensions: List[ast.InterfaceTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]): InterfaceType[Ctx, Any] =
    existing.copy(
      fieldsFn = fields,
      manualPossibleTypes = () => Nil,
      interfaces = Nil,
      astDirectives = (existing.astDirectives ++ extensions
        .flatMap(_.directives)).asInstanceOf[Vector[ast.Directive with OnInterface]],
      astNodes = existing.astNodes ++ extensions
    )

  def buildUnionType(
      origin: MatOrigin,
      extensions: Vector[ast.UnionTypeExtensionDefinition],
      definition: ast.UnionTypeDefinition,
      types: List[ObjectType[Ctx, _]],
      mat: AstSchemaMaterializer[Ctx]): Option[UnionType[Ctx]] =
    Some(
      UnionType[Ctx](
        name = typeName(definition),
        description = typeDescription(definition),
        types = types,
        astDirectives = (definition.directives ++ extensions.flatMap(_.directives))
          .asInstanceOf[Vector[ast.Directive with OnUnion]],
        astNodes = definition +: extensions
      ))

  def extendUnionType(
      origin: MatOrigin,
      extensions: Vector[ast.UnionTypeExtensionDefinition],
      existing: UnionType[Ctx],
      types: List[ObjectType[Ctx, _]],
      mat: AstSchemaMaterializer[Ctx]): UnionType[Ctx] =
    existing.copy(
      typesFn = () => types,
      astDirectives = (existing.astDirectives ++ extensions.flatMap(_.directives))
        .asInstanceOf[Vector[ast.Directive with OnUnion]],
      astNodes = existing.astNodes ++ extensions
    )

  def extendScalarAlias[T, ST](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarAlias[T, ST],
      aliasFor: ScalarType[ST],
      mat: AstSchemaMaterializer[Ctx]): ScalarAlias[T, ST] =
    existing.copy(aliasFor = aliasFor)

  def buildScalarType(
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      definition: ast.ScalarTypeDefinition,
      mat: AstSchemaMaterializer[Ctx]): Option[ScalarType[Any]] =
    Some(
      ScalarType[Any](
        name = typeName(definition),
        description = typeDescription(definition),
        coerceUserInput = scalarCoerceUserInput(definition),
        coerceOutput = scalarCoerceOutput(definition),
        coerceInput = scalarCoerceInput(definition),
        complexity = scalarComplexity(definition),
        scalarInfo = scalarValueInfo(definition),
        astDirectives = (definition.directives ++ extensions.flatMap(_.directives))
          .asInstanceOf[Vector[ast.Directive with OnScalar]],
        astNodes = definition +: extensions
      ))

  def buildEnumType(
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      definition: ast.EnumTypeDefinition,
      values: List[EnumValue[Any]],
      mat: AstSchemaMaterializer[Ctx]): Option[EnumType[Any]] =
    Some(
      EnumType[Any](
        name = typeName(definition),
        description = typeDescription(definition),
        values = values,
        astDirectives = (definition.directives ++ extensions.flatMap(_.directives))
          .asInstanceOf[Vector[ast.Directive with OnEnumType]],
        astNodes = definition +: extensions
      ))

  def buildEnumValue(
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      typeDefinition: Either[ast.EnumTypeDefinition, EnumType[_]],
      definition: ast.EnumValueDefinition,
      mat: AstSchemaMaterializer[Ctx]): Option[EnumValue[String]] =
    Some(
      EnumValue[String](
        name = enumValueName(definition),
        description = enumValueDescription(definition),
        value = enumValue(typeDefinition, definition),
        deprecationReason = enumValueDeprecationReason(definition),
        astDirectives = definition.directives.asInstanceOf[Vector[ast.Directive with OnEnumValue]],
        astNodes = Vector(definition)
      ))

  def buildField(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]): Option[Field[Ctx, Any]] =
    Some(
      Field[Ctx, Any](
        name = fieldName(definition),
        description = fieldDescription(definition),
        fieldType = fieldType,
        arguments = arguments,
        resolve = resolveField(origin, typeDefinition, extensions, definition, mat),
        tags = fieldTags(typeDefinition, definition),
        deprecationReason = fieldDeprecationReason(definition),
        complexity = fieldComplexity(typeDefinition, definition),
        manualPossibleTypes = () => Nil,
        astDirectives = definition.directives.asInstanceOf[Vector[ast.Directive with OnField]],
        astNodes = Vector(definition)
      ))

  def buildFieldType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]): OutputType[Any] =
    mat.getOutputType(origin, definition.fieldType)

  def buildAdditionalFields(
      origin: MatOrigin,
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      typeDefinition: ast.TypeDefinition,
      mat: AstSchemaMaterializer[Ctx]): List[MaterializedField[Ctx, Any]] = Nil

  def extendField(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      existing: Field[Ctx, Any],
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]): Field[Ctx, Any] =
    existing.copy(
      fieldType = fieldType,
      arguments = arguments,
      resolve = extendFieldResolver(origin, typeDefinition, existing, fieldType, mat),
      manualPossibleTypes = () => Nil)

  def extendArgument(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      field: Field[Ctx, Any],
      argument: Argument[Any],
      argumentType: InputType[_],
      mat: AstSchemaMaterializer[Ctx]): Argument[Any] =
    argument.copy(argumentType = argumentType)

  def extendInputField(
      origin: MatOrigin,
      typeDefinition: InputObjectType[_],
      existing: InputField[Any],
      fieldType: InputType[_],
      mat: AstSchemaMaterializer[Ctx]): InputField[Any] =
    existing.copy(fieldType = fieldType)

  def extendFieldType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      existing: Field[Ctx, Any],
      mat: AstSchemaMaterializer[Ctx]): OutputType[Any] =
    mat.getTypeFromExistingType(origin, existing.fieldType)

  def extendArgumentType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      field: Field[Ctx, Any],
      existing: Argument[Any],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any] =
    mat.getInputTypeFromExistingType(origin, existing.argumentType)

  def extendInputFieldType(
      origin: MatOrigin,
      typeDefinition: InputObjectType[_],
      existing: InputField[Any],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any] =
    mat.getInputTypeFromExistingType(origin, existing.fieldType)

  def buildInputField(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): Option[InputField[Any]] =
    Some(
      InputField(
        name = inputFieldName(definition),
        description = inputFieldDescription(definition),
        fieldType = tpe,
        defaultValue = defaultValue,
        astDirectives = definition.directives.asInstanceOf[Vector[ast.Directive with OnInputField]],
        astNodes = Vector(definition)
      ))

  def buildInputFieldType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): InputType[Any] =
    mat.getInputType(origin, definition.valueType)

  def buildArgument(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]): Option[Argument[Any]] =
    Some(
      Argument(
        name = argumentName(definition),
        description = argumentDescription(definition),
        argumentType = tpe,
        defaultValue = defaultValue,
        fromInput = argumentFromInput(typeDefinition, fieldDefinition, definition),
        astDirectives = definition.directives.asInstanceOf[Vector[ast.Directive with OnArgument]],
        astNodes = Vector(definition)
      ))

  def buildArgumentType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _]],
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
      mat: AstSchemaMaterializer[Ctx]): Option[Directive] =
    Some(
      Directive(
        name = directiveName(definition),
        description = directiveDescription(definition),
        locations = locations,
        arguments = arguments,
        repeatable = definition.repeatable,
        shouldInclude = directiveShouldInclude(definition)
      ))

  def transformInputObjectType[T](
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      existing: InputObjectType[T],
      fields: () => List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx]): InputObjectType[T] =
    existing.copy(
      fieldsFn = fields,
      astDirectives = (existing.astDirectives ++ extensions.flatMap(_.directives))
        .asInstanceOf[Vector[ast.Directive with OnInputObjectType]],
      astNodes = existing.astNodes ++ extensions
    )

  def transformEnumType[T](
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      existing: EnumType[T],
      mat: AstSchemaMaterializer[Ctx]): EnumType[T] = {
    val dirs = existing.astDirectives ++ extensions.flatMap(_.directives)

    if (dirs.nonEmpty)
      existing.copy(
        astDirectives = dirs.asInstanceOf[Vector[ast.Directive with OnEnumType]],
        astNodes = existing.astNodes ++ extensions)
    else existing
  }

  def transformScalarType[T](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarType[T],
      mat: AstSchemaMaterializer[Ctx]): ScalarType[T] = {
    val dirs = existing.astDirectives ++ extensions.flatMap(_.directives)

    if (dirs.nonEmpty)
      existing.copy(
        astDirectives = dirs.asInstanceOf[Vector[ast.Directive with OnScalar]],
        astNodes = existing.astNodes ++ extensions)
    else existing
  }

  def transformDirective(
      origin: MatOrigin,
      existing: Directive,
      mat: AstSchemaMaterializer[Ctx]): Directive =
    existing

  def objectTypeInstanceCheck(
      origin: MatOrigin,
      definition: ast.ObjectTypeDefinition,
      extensions: List[ast.ObjectTypeExtensionDefinition]): Option[(Any, Class[_]) => Boolean] =
    None

  def extendedObjectTypeInstanceCheck(
      origin: MatOrigin,
      tpe: ObjectType[Ctx, _],
      extensions: List[ast.ObjectTypeExtensionDefinition]): Option[(Any, Class[_]) => Boolean] =
    None

  def directiveShouldInclude(definition: ast.DirectiveDefinition): DirectiveContext => Boolean =
    Function.const(true)

  def argumentFromInput(
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition): FromInput[Map[String, Any]] =
    FromInput.defaultInput[Any]

  def resolveField(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      mat: AstSchemaMaterializer[Ctx]): Context[Ctx, _] => Action[Ctx, _] =
    _ => throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException

  def extendFieldResolver(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _]],
      existing: Field[Ctx, Any],
      fieldType: OutputType[_],
      mat: AstSchemaMaterializer[Ctx]): Context[Ctx, Any] => Action[Ctx, _] = existing.resolve

  def fieldTags(
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      definition: ast.FieldDefinition): List[FieldTag] =
    Nil

  def scalarCoerceUserInput(definition: ast.ScalarTypeDefinition): Any => Either[Violation, Any] =
    _ => Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)

  def scalarCoerceInput(definition: ast.ScalarTypeDefinition): ast.Value => Either[Violation, Any] =
    _ => Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)

  def scalarCoerceOutput(
      definition: ast.ScalarTypeDefinition): (Any, Set[MarshallerCapability]) => Any =
    (_, _) => throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException

  def scalarValueInfo(definition: ast.ScalarTypeDefinition): Set[ScalarValueInfo] =
    Set.empty

  def scalarComplexity(definition: ast.ScalarTypeDefinition): Double =
    0.0d

  def fieldComplexity(
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _]],
      definition: ast.FieldDefinition): Option[(Ctx, Args, Double) => Double] =
    None

  def enumValueDeprecationReason(definition: ast.EnumValueDefinition): Option[String] =
    deprecationReason(definition.directives.toList)

  def fieldDeprecationReason(definition: ast.FieldDefinition): Option[String] =
    deprecationReason(definition.directives.toList)

  def deprecationReason(dirs: List[ast.Directive]): Option[String] =
    dirs.find(_.name == DeprecatedDirective.name).flatMap { d =>
      d.arguments.find(_.name == ReasonArg.name) match {
        case Some(reason) =>
          reason.value match {
            case ast.StringValue(value, _, _, _, _) => Some(value)
            case _ => None
          }
        case None => Some(DefaultDeprecationReason)
      }
    }

  def typeName(definition: ast.TypeDefinition): String =
    definition.name

  def fieldName(definition: ast.FieldDefinition): String =
    definition.name

  def enumValueName(definition: ast.EnumValueDefinition): String =
    definition.name

  def argumentName(definition: ast.InputValueDefinition): String =
    definition.name

  def inputFieldName(definition: ast.InputValueDefinition): String =
    definition.name

  def directiveName(definition: ast.DirectiveDefinition): String =
    definition.name

  def commentDescription(node: ast.WithComments): Option[String] =
    AstSchemaBuilder.extractDescription(node)

  def typeDescription(definition: ast.TypeDefinition): Option[String] =
    definition.description.map(_.value)

  def fieldDescription(definition: ast.FieldDefinition): Option[String] =
    definition.description.map(_.value)

  def argumentDescription(definition: ast.InputValueDefinition): Option[String] =
    definition.description.map(_.value)

  def inputFieldDescription(definition: ast.InputValueDefinition): Option[String] =
    definition.description.map(_.value)

  def enumValueDescription(definition: ast.EnumValueDefinition): Option[String] =
    definition.description.map(_.value)

  def directiveDescription(definition: ast.DirectiveDefinition): Option[String] =
    definition.description.map(_.value)

  def enumValue(
      typeDefinition: Either[ast.EnumTypeDefinition, EnumType[_]],
      definition: ast.EnumValueDefinition): String =
    definition.name
}
