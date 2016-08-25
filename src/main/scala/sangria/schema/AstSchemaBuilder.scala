package sangria.schema

import sangria.ast
import sangria.execution.FieldTag
import sangria.marshalling.{FromInput, ToInput, MarshallerCapability, ScalarValueInfo}
import sangria.validation.Violation

import scala.reflect.ClassTag

trait AstSchemaBuilder[Ctx] {
  def additionalTypeDefs: List[ast.TypeDefinition]
  def additionalTypeExtensionDefs: List[ast.TypeExtensionDefinition]
  def additionalDirectiveDefs: List[ast.DirectiveDefinition]

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
    definition: ast.ObjectTypeDefinition,
    extensions: List[ast.TypeExtensionDefinition],
    fields: () ⇒ List[Field[Ctx, Any]],
    interfaces: List[InterfaceType[Ctx, Any]],
    mat: AstSchemaMaterializer[Ctx]): Option[ObjectType[Ctx, Any]]

  def extendObjectType(
    existing: ObjectType[Ctx, _],
    extensions: List[ast.TypeExtensionDefinition],
    fields: () ⇒ List[Field[Ctx, Any]],
    interfaces: List[InterfaceType[Ctx, Any]],
    mat: AstSchemaMaterializer[Ctx]): ObjectType[Ctx, Any]

  def buildInputObjectType(
    definition: ast.InputObjectTypeDefinition,
    fields: () ⇒ List[InputField[_]],
    mat: AstSchemaMaterializer[Ctx]): Option[InputObjectType[InputObjectType.DefaultInput]]

  def transformInputObjectType[T](
    existing: InputObjectType[T],
    mat: AstSchemaMaterializer[Ctx]): InputObjectType[T]

  def buildInterfaceType(
    definition: ast.InterfaceTypeDefinition,
    extensions: List[ast.TypeExtensionDefinition],
    fields: () ⇒ List[Field[Ctx, Any]],
    mat: AstSchemaMaterializer[Ctx]): Option[InterfaceType[Ctx, Any]]

  def extendInterfaceType(
    existing: InterfaceType[Ctx, _],
    extensions: List[ast.TypeExtensionDefinition],
    fields: () ⇒ List[Field[Ctx, Any]],
    mat: AstSchemaMaterializer[Ctx]): InterfaceType[Ctx, Any]

  def buildUnionType(
    definition: ast.UnionTypeDefinition,
    types: List[ObjectType[Ctx, _]],
    mat: AstSchemaMaterializer[Ctx]): Option[UnionType[Ctx]]

  def extendUnionType(
    existing: UnionType[Ctx],
    types: List[ObjectType[Ctx, _]],
    mat: AstSchemaMaterializer[Ctx]): UnionType[Ctx]

  def buildScalarType(
    definition: ast.ScalarTypeDefinition,
    mat: AstSchemaMaterializer[Ctx]): Option[ScalarType[Any]]

  def transformScalarType[T](
    existing: ScalarType[T],
    mat: AstSchemaMaterializer[Ctx]): ScalarType[T]

  def buildEnumType(
    definition: ast.EnumTypeDefinition,
    values: List[EnumValue[Any]],
    mat: AstSchemaMaterializer[Ctx]): Option[EnumType[Any]]

  def transformEnumType[T](
    existing: EnumType[T],
    mat: AstSchemaMaterializer[Ctx]): EnumType[T]

  def buildField(
    typeDefinition: ast.TypeDefinition,
    definition: ast.FieldDefinition,
    fieldType: OutputType[_],
    arguments: List[Argument[_]],
    mat: AstSchemaMaterializer[Ctx]): Option[Field[Ctx, Any]]

  def extendField(
    typeDefinition: ObjectLikeType[Ctx, _],
    existing: Field[Ctx, Any],
    fieldType: OutputType[_],
    mat: AstSchemaMaterializer[Ctx]): Field[Ctx, Any]

  def buildInputField(
    typeDefinition: ast.InputObjectTypeDefinition,
    definition: ast.InputValueDefinition,
    tpe: InputType[_],
    defaultValue: Option[(_, ToInput[_, _])],
    mat: AstSchemaMaterializer[Ctx]): Option[InputField[Any]]

  def buildArgument(
    typeDefinition: ast.TypeSystemDefinition,
    fieldDefinition: Option[ast.FieldDefinition],
    definition: ast.InputValueDefinition,
    tpe: InputType[_],
    defaultValue: Option[(_, ToInput[_, _])],
    mat: AstSchemaMaterializer[Ctx]): Option[Argument[Any]]

  def buildEnumValue(
    typeDefinition: ast.EnumTypeDefinition,
    definition: ast.EnumValueDefinition,
    mat: AstSchemaMaterializer[Ctx]): Option[EnumValue[Any]]

  def buildDirective(
    definition: ast.DirectiveDefinition,
    arguments: List[Argument[_]],
    locations: Set[DirectiveLocation.Value],
    mat: AstSchemaMaterializer[Ctx]): Option[Directive]

  def transformDirective(
      existing: Directive,
      mat: AstSchemaMaterializer[Ctx]): Directive
}

object AstSchemaBuilder {
  def default[Ctx] = new DefaultAstSchemaBuilder[Ctx]

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
  def additionalDirectiveDefs = Nil
  def additionalTypeExtensionDefs = Nil
  def additionalTypeDefs = Nil

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
      directives = directives)

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
      definition: ast.ObjectTypeDefinition,
      extensions: List[ast.TypeExtensionDefinition],
      fields: () ⇒ List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]) = {
    val objectType =
      objectTypeInstanceCheck(definition, extensions) match {
        case Some(fn) ⇒
          ObjectType[Ctx, Any](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = Named.checkObjFields(fields),
            interfaces = interfaces,
            instanceCheck = (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any]) ⇒ fn(value, clazz))
        case None ⇒
          ObjectType[Ctx, Any](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = Named.checkObjFields(fields),
            interfaces = interfaces,
            instanceCheck = ObjectType.defaultInstanceCheck[Ctx, Any])
      }

    Some(objectType)
  }

  def extendObjectType(
      existing: ObjectType[Ctx, _],
      extensions: List[ast.TypeExtensionDefinition],
      fields: () ⇒ List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]) =
    extendedObjectTypeInstanceCheck(existing, extensions) match {
      case Some(fn) ⇒
        existing.copy(
          fieldsFn = Named.checkObjFields(fields),
          interfaces = interfaces,
          instanceCheck = (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any]) ⇒ fn(value, clazz))(ClassTag(existing.valClass))
      case None ⇒
        existing.copy(
          fieldsFn = Named.checkObjFields(fields),
          interfaces = interfaces,
          instanceCheck = existing.instanceCheck.asInstanceOf[(Any, Class[_], ObjectType[Ctx, _]) ⇒ Boolean])(ClassTag(existing.valClass))
    }

  def buildInputObjectType(
      definition: ast.InputObjectTypeDefinition,
      fields: () ⇒ List[InputField[_]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(InputObjectType(
      name = typeName(definition),
      description = typeDescription(definition),
      fieldsFn = Named.checkIntFields(fields)))

  def buildInterfaceType(
      definition: ast.InterfaceTypeDefinition,
      extensions: List[ast.TypeExtensionDefinition],
      fields: () ⇒ List[Field[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(InterfaceType[Ctx, Any](
      name = typeName(definition),
      description = typeDescription(definition),
      fieldsFn = Named.checkIntFields(fields),
      interfaces = Nil,
      manualPossibleTypes = () ⇒ Nil))

  def extendInterfaceType(
      existing: InterfaceType[Ctx, _],
      extensions: List[ast.TypeExtensionDefinition],
      fields: () ⇒ List[Field[Ctx, Any]],
      mat: AstSchemaMaterializer[Ctx]) =
    existing.copy(fieldsFn = fields, manualPossibleTypes = () ⇒ Nil, interfaces = Nil)

  def buildUnionType(
      definition: ast.UnionTypeDefinition,
      types: List[ObjectType[Ctx, _]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(UnionType[Ctx](
      name = typeName(definition),
      description = typeDescription(definition),
      types = types))

  def extendUnionType(
      existing: UnionType[Ctx],
      types: List[ObjectType[Ctx, _]],
      mat: AstSchemaMaterializer[Ctx]) =
    existing.copy(types = types)

  def buildScalarType(
      definition: ast.ScalarTypeDefinition,
      mat: AstSchemaMaterializer[Ctx]) =
    Some(ScalarType[Any](
      name = typeName(definition),
      description = typeDescription(definition),
      coerceUserInput = scalarCoerceUserInput(definition),
      coerceOutput = scalarCoerceOutput(definition),
      coerceInput = scalarCoerceInput(definition),
      complexity = scalarComplexity(definition),
      scalarInfo = scalarValueInfo(definition)))

  def buildEnumType(
      definition: ast.EnumTypeDefinition,
      values: List[EnumValue[Any]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(EnumType[Any](
      name = typeName(definition),
      description = typeDescription(definition),
      values = values))

  def buildEnumValue(
      typeDefinition: ast.EnumTypeDefinition,
      definition: ast.EnumValueDefinition,
      mat: AstSchemaMaterializer[Ctx]) =
    Some(EnumValue[String](
      name = enumValueName(definition),
      description = enumValueDescription(definition),
      value = enumValue(definition),
      deprecationReason = enumValueDeprecationReason(definition)))

  def buildField(
      typeDefinition: ast.TypeDefinition,
      definition: ast.FieldDefinition,
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(Field[Ctx, Any](
      name = fieldName(definition),
      description = fieldDescription(definition),
      fieldType = fieldType,
      arguments = arguments,
      resolve = resolveField(typeDefinition, definition),
      tags = fieldTags(typeDefinition, definition),
      deprecationReason = fieldDeprecationReason(definition),
      complexity = fieldComplexity(typeDefinition, definition),
      manualPossibleTypes = () ⇒ Nil))

  def extendField(
      typeDefinition: ObjectLikeType[Ctx, _],
      existing: Field[Ctx, Any],
      fieldType: OutputType[_],
      mat: AstSchemaMaterializer[Ctx]) =
    existing.copy(fieldType = fieldType, manualPossibleTypes = () ⇒ Nil)

  def buildInputField(
      typeDefinition: ast.InputObjectTypeDefinition,
      definition: ast.InputValueDefinition,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: AstSchemaMaterializer[Ctx]) =
    Some(InputField(
      name = inputFieldName(definition),
      description = inputFieldDescription(definition),
      fieldType = tpe,
      defaultValue = defaultValue))

  def buildArgument(
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
      fromInput = argumentFromInput(typeDefinition, fieldDefinition, definition)))

  def buildDirective(
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
    existing: InputObjectType[T],
    mat: AstSchemaMaterializer[Ctx]) = existing

  def transformEnumType[T](
    existing: EnumType[T],
    mat: AstSchemaMaterializer[Ctx]) = existing

  def transformScalarType[T](
    existing: ScalarType[T],
    mat: AstSchemaMaterializer[Ctx]) = existing

  def transformDirective(
      existing: Directive,
      mat: AstSchemaMaterializer[Ctx]) = existing

  def objectTypeInstanceCheck(definition: ast.ObjectTypeDefinition, extensions: List[ast.TypeExtensionDefinition]): Option[(Any, Class[_]) ⇒ Boolean] =
    None

  def extendedObjectTypeInstanceCheck(tpe: ObjectType[Ctx, _], extensions: List[ast.TypeExtensionDefinition]): Option[(Any, Class[_]) ⇒ Boolean] =
    None

  def directiveShouldInclude(definition: ast.DirectiveDefinition): DirectiveContext ⇒ Boolean =
    Function.const(true)

  def argumentFromInput(
      typeDefinition: ast.TypeSystemDefinition,
      fieldDefinition: Option[ast.FieldDefinition],
      definition: ast.InputValueDefinition) =
    FromInput.defaultInput[Any]

  def resolveField(typeDefinition: ast.TypeDefinition, definition: ast.FieldDefinition): Context[Ctx, _] ⇒ Action[Ctx, _] =
    (ctx) ⇒ throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException

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
    deprecationReason(definition.directives)

  def fieldDeprecationReason(definition: ast.FieldDefinition): Option[String] =
    deprecationReason(definition.directives)

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