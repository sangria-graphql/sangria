package sangria.schema

import sangria.ast
import sangria.execution.{UserFacingError, FieldTag}
import sangria.introspection._
import sangria.marshalling._
import sangria.validation.Violation

import scala.util.Try

trait IntrospectionSchemaBuilder[Ctx] {
  def additionalTypeDefs: List[IntrospectionType]
  def additionalDirectiveDefs: List[IntrospectionDirective]

  def buildSchema(
    definition: IntrospectionSchema,
    queryType: ObjectType[Ctx, Any],
    mutationType: Option[ObjectType[Ctx, Any]],
    subscriptionType: Option[ObjectType[Ctx, Any]],
    additionalTypes: List[Type with Named],
    directives: List[Directive],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Schema[Ctx, Any]

  def buildObjectType(
    definition: IntrospectionObjectType,
    fields: () ⇒ List[Field[Ctx, Any]],
    interfaces: List[InterfaceType[Ctx, Any]],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[ObjectType[Ctx, Any]]

  def buildInputObjectType(
    definition: IntrospectionInputObjectType,
    fields: () ⇒ List[InputField[_]],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[InputObjectType[InputObjectType.DefaultInput]]

  def buildInterfaceType(
    definition: IntrospectionInterfaceType,
    fields: () ⇒ List[Field[Ctx, Any]],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[InterfaceType[Ctx, Any]]

  def buildUnionType(
    definition: IntrospectionUnionType,
    types: List[ObjectType[Ctx, _]],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[UnionType[Ctx]]

  def buildScalarType(
    definition: IntrospectionScalarType,
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[ScalarType[Any]]

  def buildEnumType(
    definition: IntrospectionEnumType,
    values: List[EnumValue[Any]],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[EnumType[Any]]

  def buildField(
    typeDefinition: IntrospectionType,
    definition: IntrospectionField,
    fieldType: OutputType[_],
    arguments: List[Argument[_]],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[Field[Ctx, Any]]

  def buildInputField(
    typeDefinition: IntrospectionInputObjectType,
    definition: IntrospectionInputValue,
    tpe: InputType[_],
    defaultValue: Option[(_, ToInput[_, _])],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[InputField[Any]]

  def buildArgument(
    fieldDefinition: Option[IntrospectionField],
    definition: IntrospectionInputValue,
    tpe: InputType[_],
    defaultValue: Option[(_, ToInput[_, _])],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[Argument[Any]]

  def buildEnumValue(
    typeDefinition: IntrospectionEnumType,
    definition: IntrospectionEnumValue,
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[EnumValue[Any]]

  def buildDirective(
    definition: IntrospectionDirective,
    arguments: List[Argument[_]],
    mat: IntrospectionSchemaMaterializer[Ctx, _]): Option[Directive]
}

object IntrospectionSchemaBuilder {
  def default[Ctx] = new DefaultIntrospectionSchemaBuilder[Ctx]
}

class DefaultIntrospectionSchemaBuilder[Ctx] extends IntrospectionSchemaBuilder[Ctx] {
  def additionalDirectiveDefs = Nil
  def additionalTypeDefs = Nil

  def buildSchema(
      definition: IntrospectionSchema,
      queryType: ObjectType[Ctx, Any],
      mutationType: Option[ObjectType[Ctx, Any]],
      subscriptionType: Option[ObjectType[Ctx, Any]],
      additionalTypes: List[Type with Named],
      directives: List[Directive],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Schema[Ctx, Any](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = additionalTypes,
      directives = directives)

  def buildObjectType(
      definition: IntrospectionObjectType,
      fields: () ⇒ List[Field[Ctx, Any]],
      interfaces: List[InterfaceType[Ctx, Any]],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) = {
    val objectType =
      objectTypeInstanceCheck(definition) match {
        case Some(fn) ⇒
          ObjectType[Ctx, Any](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = fields,
            interfaces = interfaces,
            instanceCheck = (value: Any, clazz: Class[_], _: ObjectType[Ctx, Any]) ⇒ fn(value, clazz))
        case None ⇒
          ObjectType[Ctx, Any](
            name = typeName(definition),
            description = typeDescription(definition),
            fieldsFn = fields,
            interfaces = interfaces,
            instanceCheck = ObjectType.defaultInstanceCheck[Ctx, Any])
      }

    Some(objectType)
  }

  def buildInputObjectType(
      definition: IntrospectionInputObjectType,
      fields: () ⇒ List[InputField[_]],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(InputObjectType(
      name = typeName(definition),
      description = typeDescription(definition),
      fieldsFn = fields))

  def buildInterfaceType(
      definition: IntrospectionInterfaceType,
      fields: () ⇒ List[Field[Ctx, Any]],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(InterfaceType[Ctx, Any](
      name = typeName(definition),
      description = typeDescription(definition),
      fieldsFn = fields,
      interfaces = Nil,
      manualPossibleTypes = () ⇒ Nil))

  def buildUnionType(
      definition: IntrospectionUnionType,
      types: List[ObjectType[Ctx, _]],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(UnionType[Ctx](
      name = typeName(definition),
      description = typeDescription(definition),
      types = types))

  def buildScalarType(
      definition: IntrospectionScalarType,
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(ScalarType[Any](
      name = typeName(definition),
      description = typeDescription(definition),
      coerceUserInput = scalarCoerceUserInput(definition),
      coerceOutput = scalarCoerceOutput(definition),
      coerceInput = scalarCoerceInput(definition),
      complexity = scalarComplexity(definition),
      scalarInfo = scalarValueInfo(definition)))

  def buildEnumType(
      definition: IntrospectionEnumType,
      values: List[EnumValue[Any]],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(EnumType[Any](
      name = typeName(definition),
      description = typeDescription(definition),
      values = values))

  def buildEnumValue(
      typeDefinition: IntrospectionEnumType,
      definition: IntrospectionEnumValue,
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(EnumValue[String](
      name = enumValueName(definition),
      description = enumValueDescription(definition),
      value = enumValue(definition),
      deprecationReason = enumValueDeprecationReason(definition)))

  def buildField(
      typeDefinition: IntrospectionType,
      definition: IntrospectionField,
      fieldType: OutputType[_],
      arguments: List[Argument[_]],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
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

  def buildInputField(
      typeDefinition: IntrospectionInputObjectType,
      definition: IntrospectionInputValue,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(InputField(
      name = inputFieldName(definition),
      description = inputFieldDescription(definition),
      fieldType = tpe,
      defaultValue = defaultValue))

  def buildArgument(
      fieldDefinition: Option[IntrospectionField],
      definition: IntrospectionInputValue,
      tpe: InputType[_],
      defaultValue: Option[(_, ToInput[_, _])],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(Argument(
      name = argumentName(definition),
      description = argumentDescription(definition),
      argumentType = tpe,
      defaultValue = defaultValue,
      fromInput = argumentFromInput(fieldDefinition, definition)))

  def buildDirective(
      definition: IntrospectionDirective,
      arguments: List[Argument[_]],
      mat: IntrospectionSchemaMaterializer[Ctx, _]) =
    Some(Directive(
      name = directiveName(definition),
      description = directiveDescription(definition),
      locations = definition.locations,
      arguments = arguments,
      shouldInclude = directiveShouldInclude(definition)))

  def objectTypeInstanceCheck(definition: IntrospectionObjectType): Option[(Any, Class[_]) ⇒ Boolean] =
    None

  def directiveShouldInclude(definition: IntrospectionDirective): DirectiveContext ⇒ Boolean =
    Function.const(true)

  def argumentFromInput(fieldDefinition: Option[IntrospectionField], definition: IntrospectionInputValue) =
    FromInput.defaultInput[Any]

  def resolveField(typeDefinition: IntrospectionType, definition: IntrospectionField): Context[Ctx, _] ⇒ Action[Ctx, _] =
    (ctx) ⇒ throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException

  def fieldTags(typeDefinition: IntrospectionType, definition: IntrospectionField): List[FieldTag] =
    Nil

  def scalarCoerceUserInput(definition: IntrospectionScalarType): Any ⇒ Either[Violation, Any] =
    _ ⇒ Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)

  def scalarCoerceInput(definition: IntrospectionScalarType): ast.Value ⇒ Either[Violation, Any] =
    _ ⇒ Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)

  def scalarCoerceOutput(definition: IntrospectionScalarType): (Any, Set[MarshallerCapability]) ⇒ Any =
    (_, _) ⇒ throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException

  def scalarValueInfo(definition: IntrospectionScalarType): Set[ScalarValueInfo] =
    Set.empty

  def scalarComplexity(definition: IntrospectionScalarType): Double =
    0.0D

  def fieldComplexity(typeDefinition: IntrospectionType, definition: IntrospectionField): Option[(Ctx, Args, Double) ⇒ Double] =
    None

  def directiveName(definition: IntrospectionDirective): String =
    definition.name

  def fieldName(definition: IntrospectionField): String =
    definition.name

  def typeName(definition: IntrospectionType): String =
    definition.name

  def enumValueName(definition: IntrospectionEnumValue): String =
    definition.name

  def argumentName(definition: IntrospectionInputValue): String =
    definition.name

  def inputFieldName(definition: IntrospectionInputValue): String =
    definition.name

  def directiveDescription(definition: IntrospectionDirective): Option[String] =
    definition.description
  
  def fieldDescription(definition: IntrospectionField): Option[String] =
    definition.description

  def typeDescription(definition: IntrospectionType): Option[String] =
    definition.description

  def enumValueDescription(definition: IntrospectionEnumValue): Option[String] =
    definition.description

  def argumentDescription(definition: IntrospectionInputValue): Option[String] =
    definition.description

  def inputFieldDescription(definition: IntrospectionInputValue): Option[String] =
    definition.description

  def enumValue(definition: IntrospectionEnumValue): String =
    definition.name

  def fieldDeprecationReason(definition: IntrospectionField): Option[String] =
    definition.deprecationReason orElse (if (definition.isDeprecated) Some(DefaultDeprecationReason) else None)
  
  def enumValueDeprecationReason(definition: IntrospectionEnumValue): Option[String] =
    definition.deprecationReason orElse (if (definition.isDeprecated) Some(DefaultDeprecationReason) else None)

  def defaultValueParser: Option[String ⇒ Try[(Any, InputUnmarshaller[Any])]] =
    None
}

object DefaultIntrospectionSchemaBuilder {
  val MaterializedSchemaErrorMessage = "Schema was materialized and cannot be used for any queries except introspection queries."

  case object MaterializedSchemaException extends Exception(MaterializedSchemaErrorMessage) with UserFacingError
  case object MaterializedSchemaViolation extends Violation {
    val errorMessage = MaterializedSchemaErrorMessage
  }
} 