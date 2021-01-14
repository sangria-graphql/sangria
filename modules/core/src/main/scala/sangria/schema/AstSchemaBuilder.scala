package sangria.schema

import sangria.ast
import sangria.execution.FieldTag
import sangria.marshalling.{FromInput, MarshallerCapability, ScalarValueInfo, ToInput}
import sangria.validation.Violation

import scala.reflect.ClassTag

trait AstSchemaBuilder[Ctx, F[_]] {
  def additionalTypeExtensionDefs: List[ast.TypeExtensionDefinition]
  def additionalTypes: List[MaterializedType]
  def additionalDirectiveDefs: List[ast.DirectiveDefinition]

  def resolveNameConflict(fromOrigin: MatOrigin, types: Vector[MaterializedType]): MaterializedType

  def buildSchema(
      definition: Option[ast.SchemaDefinition],
      extensions: List[ast.SchemaExtensionDefinition],
      queryType: ObjectType[Ctx, Any, F],
      mutationType: Option[ObjectType[Ctx, Any, F]],
      subscriptionType: Option[ObjectType[Ctx, Any, F]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx, F]): Schema[Ctx, Any, F]

  def extendSchema[Val](
      originalSchema: Schema[Ctx, Val, F],
      extensions: List[ast.SchemaExtensionDefinition],
      queryType: ObjectType[Ctx, Val, F],
      mutationType: Option[ObjectType[Ctx, Val, F]],
      subscriptionType: Option[ObjectType[Ctx, Val, F]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx, F]): Schema[Ctx, Val, F]

  def buildObjectType(
      origin: MatOrigin,
      definition: ast.ObjectTypeDefinition,
      extensions: List[ast.ObjectTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any, F]],
      interfaces: List[InterfaceType[Ctx, Any, F]],
      mat: AstSchemaMaterializer[Ctx, F]): Option[ObjectType[Ctx, Any, F]]

  def extendObjectType(
      origin: MatOrigin,
      existing: ObjectType[Ctx, _, F],
      extensions: List[ast.ObjectTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any, F]],
      interfaces: List[InterfaceType[Ctx, Any, F]],
      mat: AstSchemaMaterializer[Ctx, F]): ObjectType[Ctx, Any, F]

  def buildInputObjectType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      definition: ast.InputObjectTypeDefinition,
      fields: () => List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx, F]): Option[InputObjectType[InputObjectType.DefaultInput]]

  def transformInputObjectType[T](
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      existing: InputObjectType[T],
      fields: () => List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx, F]): InputObjectType[T]

  def buildInterfaceType(
      origin: MatOrigin,
      definition: ast.InterfaceTypeDefinition,
      extensions: List[ast.InterfaceTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any, F]],
      mat: AstSchemaMaterializer[Ctx, F]): Option[InterfaceType[Ctx, Any, F]]

  def extendInterfaceType(
      origin: MatOrigin,
      existing: InterfaceType[Ctx, _, F],
      extensions: List[ast.InterfaceTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any, F]],
      mat: AstSchemaMaterializer[Ctx, F]): InterfaceType[Ctx, Any, F]

  def buildUnionType(
      origin: MatOrigin,
      extensions: Vector[ast.UnionTypeExtensionDefinition],
      definition: ast.UnionTypeDefinition,
      types: List[ObjectType[Ctx, _, F]],
      mat: AstSchemaMaterializer[Ctx, F]): Option[UnionType[Ctx, F]]

  def extendUnionType(
      origin: MatOrigin,
      extensions: Vector[ast.UnionTypeExtensionDefinition],
      existing: UnionType[Ctx, F],
      types: List[ObjectType[Ctx, _, F]],
      mat: AstSchemaMaterializer[Ctx, F]): UnionType[Ctx, F]

  def extendScalarAlias[T, ST](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarAlias[T, ST],
      aliasFor: ScalarType[ST],
      mat: AstSchemaMaterializer[Ctx, F]): ScalarAlias[T, ST]

  def buildScalarType(
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      definition: ast.ScalarTypeDefinition,
      mat: AstSchemaMaterializer[Ctx, F]): Option[ScalarType[Any]]

  def transformScalarType[T](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarType[T],
      mat: AstSchemaMaterializer[Ctx, F]): ScalarType[T]

  def buildEnumType(
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      definition: ast.EnumTypeDefinition,
      values: List[EnumValue[Any]],
      mat: AstSchemaMaterializer[Ctx, F]): Option[EnumType[Any]]

  def transformEnumType[T](
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      existing: EnumType[T],
      mat: AstSchemaMaterializer[Ctx, F]): EnumType[T]

  def buildField(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx, F]): Option[Field[Ctx, Any, F]]

  def buildAdditionalFields(
      origin: MatOrigin,
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      typeDefinition: ast.TypeDefinition,
      mat: AstSchemaMaterializer[Ctx, F]): List[MaterializedField[Ctx, Any]]

  def buildFieldType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx, F]): OutputType[Any]

  def extendField(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      existing: Field[Ctx, Any, F],
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx, F]): Field[Ctx, Any, F]

  def extendArgument(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      field: Field[Ctx, Any, F],
      argument: Argument[Any],
      argumentType: InputType[_],
      mat: AstSchemaMaterializer[Ctx, F]): Argument[Any]

  def extendInputField(
      origin: MatOrigin,
      typeDefinition: InputObjectType[_],
      existing: InputField[Any],
      fieldType: InputType[_],
      mat: AstSchemaMaterializer[Ctx, F]): InputField[Any]

  def extendFieldType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      existing: Field[Ctx, Any, F],
      mat: AstSchemaMaterializer[Ctx, F]): OutputType[Any]

  def extendArgumentType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      field: Field[Ctx, Any, F],
      existing: Argument[Any],
      mat: AstSchemaMaterializer[Ctx, F]): InputType[Any]

  def extendInputFieldType(
      origin: MatOrigin,
      typeDefinition: InputObjectType[_],
      existing: InputField[Any],
      mat: AstSchemaMaterializer[Ctx, F]): InputType[Any]

  def buildInputField(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx, F]): Option[InputField[Any]]

  def buildInputFieldType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx, F]): InputType[Any]

  def buildArgument(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _, F]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx, F]): Option[Argument[Any]]

  def buildArgumentType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _, F]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx, F]): InputType[Any]

  def buildEnumValue(
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      typeDefinition: Either[ast.EnumTypeDefinition, EnumType[_]],
      definition: ast.EnumValueDefinition,
      mat: AstSchemaMaterializer[Ctx, F]): Option[EnumValue[Any]]

  def buildDirective(
      origin: MatOrigin,
      definition: ast.DirectiveDefinition,
      arguments: List[Argument[_]],
      locations: Set[DirectiveLocation.Value],
      mat: AstSchemaMaterializer[Ctx, F]): Option[Directive]

  def transformDirective(
      origin: MatOrigin,
      existing: Directive,
      mat: AstSchemaMaterializer[Ctx, F]): Directive
}

object AstSchemaBuilder {
  def default[Ctx, F[_]] = new DefaultAstSchemaBuilder[Ctx, F]
  def resolverBased[Ctx, F[_]](resolvers: AstSchemaResolver[Ctx]*) =
    new ResolverBasedAstSchemaBuilder[Ctx, F](resolvers)

  @deprecated("Please migrate to new string-based description format", "1.3.3")
  def defaultWithLegacyCommentDescriptions[Ctx, F[_]] = new DefaultAstSchemaBuilder[Ctx, F] {
    override def useLegacyCommentDescriptions = true
  }

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

    def unapply[F[_]](field: Field[_, _, F]): Option[String] =
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
              case (c, acc) => acc
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

class DefaultAstSchemaBuilder[Ctx, F[_]] extends AstSchemaBuilder[Ctx, F] {
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
      queryType: ObjectType[Ctx, Any, F],
      mutationType: Option[ObjectType[Ctx, Any, F]],
      subscriptionType: Option[ObjectType[Ctx, Any, F]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Schema[Ctx, Any, F](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      description = definition.flatMap(_.description.map(_.value)),
      directives = directives,
      astDirectives =
        definition.fold(Vector.empty[ast.Directive])(_.directives) ++ extensions.flatMap(
          _.directives),
      astNodes = Vector(mat.document) ++ extensions ++ definition.toVector
    )

  def extendSchema[Val](
      originalSchema: Schema[Ctx, Val, F],
      extensions: List[ast.SchemaExtensionDefinition],
      queryType: ObjectType[Ctx, Val, F],
      mutationType: Option[ObjectType[Ctx, Val, F]],
      subscriptionType: Option[ObjectType[Ctx, Val, F]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Schema[Ctx, Val, F](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      directives = directives,
      description = originalSchema.description,
      validationRules = originalSchema.validationRules,
      astDirectives = originalSchema.astDirectives ++ extensions.flatMap(_.directives),
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
      fields: () => List[Field[Ctx, Any, F]],
      interfaces: List[InterfaceType[Ctx, Any, F]],
      mat: AstSchemaMaterializer[Ctx, F]) = {
    val directives = definition.directives ++ extensions.flatMap(_.directives)

    val objectType =
      objectTypeInstanceCheck(origin, definition, extensions) match {
        case Some(fn) =>
          ObjectType[Ctx, Any, F](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = fields,
            interfaces = interfaces,
            instanceCheck =
              (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any, F]) => fn(value, clazz),
            astDirectives = directives,
            astNodes = (definition +: extensions).toVector
          )
        case None =>
          ObjectType[Ctx, Any, F](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = fields,
            interfaces = interfaces,
            instanceCheck = ObjectType.defaultInstanceCheck[Ctx, Any, F],
            astDirectives = directives,
            astNodes = (definition +: extensions).toVector
          )
      }

    Some(objectType)
  }

  def extendObjectType(
      origin: MatOrigin,
      existing: ObjectType[Ctx, _, F],
      extensions: List[ast.ObjectTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any, F]],
      interfaces: List[InterfaceType[Ctx, Any, F]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    extendedObjectTypeInstanceCheck(origin, existing, extensions) match {
      case Some(fn) =>
        existing.copy(
          fieldsFn = fields,
          interfaces = interfaces,
          astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
          astNodes = existing.astNodes ++ extensions,
          instanceCheck =
            (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any, F]) => fn(value, clazz)
        )(ClassTag(existing.valClass))
      case None =>
        existing.copy(
          fieldsFn = fields,
          interfaces = interfaces,
          astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
          astNodes = existing.astNodes ++ extensions,
          instanceCheck =
            existing.instanceCheck.asInstanceOf[(Any, Class[_], ObjectType[Ctx, _, F]) => Boolean]
        )(ClassTag(existing.valClass))
    }

  def buildInputObjectType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      definition: ast.InputObjectTypeDefinition,
      fields: () => List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      InputObjectType(
        name = typeName(definition),
        description = typeDescription(definition),
        fieldsFn = fields,
        astDirectives = definition.directives ++ extensions.flatMap(_.directives),
        astNodes = definition +: extensions
      ))

  def buildInterfaceType(
      origin: MatOrigin,
      definition: ast.InterfaceTypeDefinition,
      extensions: List[ast.InterfaceTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any, F]],
      mat: AstSchemaMaterializer[Ctx, F]) = {
    val directives = definition.directives ++ extensions.flatMap(_.directives)

    Some(
      InterfaceType[Ctx, Any, F](
        name = typeName(definition),
        description = typeDescription(definition),
        fieldsFn = fields,
        interfaces = Nil,
        manualPossibleTypes = () => Nil,
        astDirectives = directives,
        astNodes = (definition +: extensions).toVector
      ))
  }

  def extendInterfaceType(
      origin: MatOrigin,
      existing: InterfaceType[Ctx, _, F],
      extensions: List[ast.InterfaceTypeExtensionDefinition],
      fields: () => List[Field[Ctx, Any, F]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    existing.copy(
      fieldsFn = fields,
      manualPossibleTypes = () => Nil,
      interfaces = Nil,
      astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
      astNodes = existing.astNodes ++ extensions
    )

  def buildUnionType(
      origin: MatOrigin,
      extensions: Vector[ast.UnionTypeExtensionDefinition],
      definition: ast.UnionTypeDefinition,
      types: List[ObjectType[Ctx, _, F]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      UnionType[Ctx, F](
        name = typeName(definition),
        description = typeDescription(definition),
        types = types,
        astDirectives = definition.directives ++ extensions.flatMap(_.directives),
        astNodes = definition +: extensions
      ))

  def extendUnionType(
      origin: MatOrigin,
      extensions: Vector[ast.UnionTypeExtensionDefinition],
      existing: UnionType[Ctx, F],
      types: List[ObjectType[Ctx, _, F]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    existing.copy(
      typesFn = () => types,
      astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
      astNodes = existing.astNodes ++ extensions)

  def extendScalarAlias[T, ST](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarAlias[T, ST],
      aliasFor: ScalarType[ST],
      mat: AstSchemaMaterializer[Ctx, F]) =
    existing.copy(aliasFor = aliasFor)

  def buildScalarType(
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      definition: ast.ScalarTypeDefinition,
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      ScalarType[Any](
        name = typeName(definition),
        description = typeDescription(definition),
        coerceUserInput = scalarCoerceUserInput(definition),
        coerceOutput = scalarCoerceOutput(definition),
        coerceInput = scalarCoerceInput(definition),
        complexity = scalarComplexity(definition),
        scalarInfo = scalarValueInfo(definition),
        astDirectives = definition.directives ++ extensions.flatMap(_.directives),
        astNodes = definition +: extensions
      ))

  def buildEnumType(
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      definition: ast.EnumTypeDefinition,
      values: List[EnumValue[Any]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      EnumType[Any](
        name = typeName(definition),
        description = typeDescription(definition),
        values = values,
        astDirectives = definition.directives ++ extensions.flatMap(_.directives),
        astNodes = definition +: extensions
      ))

  def buildEnumValue(
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      typeDefinition: Either[ast.EnumTypeDefinition, EnumType[_]],
      definition: ast.EnumValueDefinition,
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      EnumValue[String](
        name = enumValueName(definition),
        description = enumValueDescription(definition),
        value = enumValue(typeDefinition, definition),
        deprecationReason = enumValueDeprecationReason(definition),
        astDirectives = definition.directives,
        astNodes = Vector(definition)
      ))

  def buildField(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      Field[Ctx, Any, F](
        name = fieldName(definition),
        description = fieldDescription(definition),
        fieldType = fieldType,
        arguments = arguments,
        resolve = resolveField(origin, typeDefinition, extensions, definition, mat),
        tags = fieldTags(typeDefinition, definition),
        deprecationReason = fieldDeprecationReason(definition),
        complexity = fieldComplexity(typeDefinition, definition),
        manualPossibleTypes = () => Nil,
        astDirectives = definition.directives,
        astNodes = Vector(definition)
      ))

  def buildFieldType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx, F]): OutputType[Any] =
    mat.getOutputType(origin, definition.fieldType)

  def buildAdditionalFields(
      origin: MatOrigin,
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      typeDefinition: ast.TypeDefinition,
      mat: AstSchemaMaterializer[Ctx, F]): List[MaterializedField[Ctx, Any]] = Nil

  def extendField(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      existing: Field[Ctx, Any, F],
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    existing.copy(
      fieldType = fieldType,
      arguments = arguments,
      resolve = extendFieldResolver(origin, typeDefinition, existing, fieldType, mat),
      manualPossibleTypes = () => Nil)

  def extendArgument(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      field: Field[Ctx, Any, F],
      argument: Argument[Any],
      argumentType: InputType[_],
      mat: AstSchemaMaterializer[Ctx, F]): Argument[Any] =
    argument.copy(argumentType = argumentType)

  def extendInputField(
      origin: MatOrigin,
      typeDefinition: InputObjectType[_],
      existing: InputField[Any],
      fieldType: InputType[_],
      mat: AstSchemaMaterializer[Ctx, F]) =
    existing.copy(fieldType = fieldType)

  def extendFieldType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      existing: Field[Ctx, Any, F],
      mat: AstSchemaMaterializer[Ctx, F]): OutputType[Any] =
    mat.getTypeFromExistingType(origin, existing.fieldType)

  def extendArgumentType(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      field: Field[Ctx, Any, F],
      existing: Argument[Any],
      mat: AstSchemaMaterializer[Ctx, F]): InputType[Any] =
    mat.getInputTypeFromExistingType(origin, existing.argumentType)

  def extendInputFieldType(
      origin: MatOrigin,
      typeDefinition: InputObjectType[_],
      existing: InputField[Any],
      mat: AstSchemaMaterializer[Ctx, F]) =
    mat.getInputTypeFromExistingType(origin, existing.fieldType)

  def buildInputField(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      InputField(
        name = inputFieldName(definition),
        description = inputFieldDescription(definition),
        fieldType = tpe,
        defaultValue = defaultValue,
        astDirectives = definition.directives,
        astNodes = Vector(definition)
      ))

  def buildInputFieldType(
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      typeDefinition: Either[ast.InputObjectTypeDefinition, InputObjectType[_]],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx, F]): InputType[Any] =
    mat.getInputType(origin, definition.valueType)

  def buildArgument(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _, F]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      Argument(
        name = argumentName(definition),
        description = argumentDescription(definition),
        argumentType = tpe,
        defaultValue = defaultValue,
        fromInput = argumentFromInput(typeDefinition, fieldDefinition, definition),
        astDirectives = definition.directives,
        astNodes = Vector(definition)
      ))

  def buildArgumentType(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _, F]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition,
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx, F]): InputType[Any] =
    mat.getInputType(origin, definition.valueType)

  def buildDirective(
      origin: MatOrigin,
      definition: ast.DirectiveDefinition,
      arguments: List[Argument[_]],
      locations: Set[DirectiveLocation.Value],
      mat: AstSchemaMaterializer[Ctx, F]) =
    Some(
      Directive(
        name = directiveName(definition),
        description = directiveDescription(definition),
        locations = locations,
        arguments = arguments,
        shouldInclude = directiveShouldInclude(definition)
      ))

  def transformInputObjectType[T](
      origin: MatOrigin,
      extensions: Vector[ast.InputObjectTypeExtensionDefinition],
      existing: InputObjectType[T],
      fields: () => List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx, F]) =
    existing.copy(
      fieldsFn = fields,
      astDirectives = existing.astDirectives ++ extensions.flatMap(_.directives),
      astNodes = existing.astNodes ++ extensions)

  def transformEnumType[T](
      origin: MatOrigin,
      extensions: Vector[ast.EnumTypeExtensionDefinition],
      existing: EnumType[T],
      mat: AstSchemaMaterializer[Ctx, F]) = {
    val dirs = existing.astDirectives ++ extensions.flatMap(_.directives)

    if (dirs.nonEmpty)
      existing.copy(astDirectives = dirs, astNodes = existing.astNodes ++ extensions)
    else existing
  }

  def transformScalarType[T](
      origin: MatOrigin,
      extensions: Vector[ast.ScalarTypeExtensionDefinition],
      existing: ScalarType[T],
      mat: AstSchemaMaterializer[Ctx, F]) = {
    val dirs = existing.astDirectives ++ extensions.flatMap(_.directives)

    if (dirs.nonEmpty)
      existing.copy(astDirectives = dirs, astNodes = existing.astNodes ++ extensions)
    else existing
  }

  def transformDirective(
      origin: MatOrigin,
      existing: Directive,
      mat: AstSchemaMaterializer[Ctx, F]) =
    existing

  def objectTypeInstanceCheck(
      origin: MatOrigin,
      definition: ast.ObjectTypeDefinition,
      extensions: List[ast.ObjectTypeExtensionDefinition]): Option[(Any, Class[_]) => Boolean] =
    None

  def extendedObjectTypeInstanceCheck(
      origin: MatOrigin,
      tpe: ObjectType[Ctx, _, F],
      extensions: List[ast.ObjectTypeExtensionDefinition]): Option[(Any, Class[_]) => Boolean] =
    None

  def directiveShouldInclude(definition: ast.DirectiveDefinition): DirectiveContext => Boolean =
    Function.const(true)

  def argumentFromInput(
      typeDefinition: Either[ast.TypeSystemDefinition, ObjectLikeType[Ctx, _, F]],
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition) =
    FromInput.defaultInput[Any]

  def resolveField(
      origin: MatOrigin,
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
      extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
      definition: ast.FieldDefinition,
      mat: AstSchemaMaterializer[Ctx, F]): Context[Ctx, _, F] => Action[Ctx, _, F] =
    _ => throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException

  def extendFieldResolver(
      origin: MatOrigin,
      typeDefinition: Option[ObjectLikeType[Ctx, _, F]],
      existing: Field[Ctx, Any, F],
      fieldType: OutputType[_],
      mat: AstSchemaMaterializer[Ctx, F]): Context[Ctx, Any, F] => Action[Ctx, _, F] =
    existing.resolve

  def fieldTags(
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
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
      typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Ctx, _, F]],
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

  @deprecated("Please migrate to new string-based description format", "1.3.3")
  def useLegacyCommentDescriptions: Boolean = false

  def commentDescription(node: ast.WithComments): Option[String] =
    AstSchemaBuilder.extractDescription(node)

  def typeDescription(definition: ast.TypeDefinition): Option[String] =
    if (useLegacyCommentDescriptions) commentDescription(definition)
    else definition.description.map(_.value)

  def fieldDescription(definition: ast.FieldDefinition): Option[String] =
    if (useLegacyCommentDescriptions) commentDescription(definition)
    else definition.description.map(_.value)

  def argumentDescription(definition: ast.InputValueDefinition): Option[String] =
    if (useLegacyCommentDescriptions) commentDescription(definition)
    else definition.description.map(_.value)

  def inputFieldDescription(definition: ast.InputValueDefinition): Option[String] =
    if (useLegacyCommentDescriptions) commentDescription(definition)
    else definition.description.map(_.value)

  def enumValueDescription(definition: ast.EnumValueDefinition): Option[String] =
    if (useLegacyCommentDescriptions) commentDescription(definition)
    else definition.description.map(_.value)

  def directiveDescription(definition: ast.DirectiveDefinition): Option[String] =
    if (useLegacyCommentDescriptions) commentDescription(definition)
    else definition.description.map(_.value)

  def enumValue(
      typeDefinition: Either[ast.EnumTypeDefinition, EnumType[_]],
      definition: ast.EnumValueDefinition): String =
    definition.name
}
