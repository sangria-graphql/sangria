package sangria.schema

import sangria.ast
import sangria.execution.{FieldTag, UserFacingError}
import sangria.introspection._
import sangria.marshalling.{InputParser, ToInput, FromInput, InputUnmarshaller}
import sangria.parser.DeliveryScheme.Throw
import sangria.renderer.SchemaRenderer
import sangria.validation.Violation

import scala.collection.concurrent.TrieMap
import scala.util.{Try, Failure, Success}

class IntrospectionSchemaMaterializer[Ctx, T : InputUnmarshaller] private (introspectionResult: T, logic: MaterializationLogic[Ctx]) {
  private val typeDefCache = TrieMap[String, Type with Named]()

  private lazy val schemaDef = IntrospectionParser.parse(introspectionResult)

  lazy val build: Schema[Ctx, Unit] = {
    val queryType = getObjectType(schemaDef.queryType)
    val mutationType = schemaDef.mutationType map getObjectType
    val subscriptionType = schemaDef.subscriptionType map getObjectType
    val directives = schemaDef.directives.toList map buildDirective

    Schema[Ctx, Any](
      query = queryType,
      mutation = mutationType,
      subscription = subscriptionType,
      additionalTypes = findUnusedTypes(schemaDef.types),
      directives = directives).asInstanceOf[Schema[Ctx, Unit]]
  }

  private def findUnusedTypes(allTypes: Seq[IntrospectionType]): List[Type with Named] = {
    // first init all lazy fields. TODO: think about better solution
    typeDefCache.values.foreach {
      case o: ObjectLikeType[_, _] ⇒ o.fields
      case o: InputObjectType[_] ⇒ o.fields
      case _ ⇒ // do nothing
    }

    val referenced = typeDefCache.keySet
    val notReferenced = allTypes.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))

    notReferenced.toList map (tpe ⇒ getNamedType(tpe.name))
  }

  private def buildDefault(defaultValue: Option[String]) =
    defaultValue flatMap (dv ⇒ logic.defaultValueParser flatMap { parser ⇒
      parser(dv) match {
        case Success((parsed, unmarshaller)) ⇒ Some(parsed → DefaultMaterializationLogic.ConstantToInput(unmarshaller))
        case Failure(error) ⇒ throw new SchemaMaterializationException(s"Unable to parse default value '$dv'.", error)
      }
    })

  private def buildArgument(value: IntrospectionInputValue) =
    Argument(
      name = value.name,
      argumentType = getInputType(value.tpe),
      description = value.description,
      defaultValue = buildDefault(value.defaultValue),
      fromInput = FromInput.defaultInput[Any])

  private def buildInputField(value: IntrospectionInputValue) =
    InputField(
      name = value.name,
      fieldType = getInputType(value.tpe),
      description = value.description,
      defaultValue = buildDefault(value.defaultValue))

  private def buildDirective(directive: IntrospectionDirective) =
    BuiltinDirectives.find(_.name == directive.name) getOrElse
      Directive(
        name = directive.name,
        description = directive.description,
        locations = directive.locations,
        arguments = directive.args.toList map buildArgument,
        shouldInclude = Function.const(true))

  private def getObjectType(typeRef: IntrospectionTypeRef): ObjectType[Ctx, Any] =
    getOutputType(typeRef, false) match {
      case obj: ObjectType[_, _] ⇒ obj.asInstanceOf[ObjectType[Ctx, Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${SchemaRenderer.renderTypeName(typeRef)}' is not an object type.")
    }

  private def getInterfaceType(typeRef: IntrospectionTypeRef) =
    getOutputType(typeRef, false) match {
      case obj: InterfaceType[_, _] ⇒ obj.asInstanceOf[InterfaceType[Ctx, Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${SchemaRenderer.renderTypeName(typeRef)}' is not an interface type.")
    }

  private def getInputType(typeRef: IntrospectionTypeRef, optional: Boolean = true): InputType[_] =
    typeRef match {
      case IntrospectionListTypeRef(ofType) if optional ⇒ OptionInputType(ListInputType(getInputType(ofType, true)))
      case IntrospectionListTypeRef(ofType) ⇒ ListInputType(getInputType(ofType, true))
      case IntrospectionNonNullTypeRef(ofType) ⇒ getInputType(ofType, false)
      case IntrospectionNamedTypeRef(_, name) ⇒
        getNamedType(name) match {
          case input: InputType[_] if optional ⇒ OptionInputType(input)
          case input: InputType[_] ⇒ input
          case _ ⇒ throw new SchemaMaterializationException(s"Type '$name' is not an input type, but was used in input type position!")
        }
    }

  private def getOutputType(typeRef: IntrospectionTypeRef, optional: Boolean = true): OutputType[_] =
    typeRef match {
      case IntrospectionListTypeRef(ofType) if optional ⇒ OptionType(ListType(getOutputType(ofType, true)))
      case IntrospectionListTypeRef(ofType) ⇒ ListType(getOutputType(ofType, true))
      case IntrospectionNonNullTypeRef(ofType) ⇒ getOutputType(ofType, false)
      case IntrospectionNamedTypeRef(_, name) ⇒
        getNamedType(name) match {
          case input: OutputType[_] if optional ⇒ OptionType(input)
          case input: OutputType[_] ⇒ input
          case _ ⇒ throw new SchemaMaterializationException(s"Type '$name' is not an output type, but was used in output type position!")
        }
    }

  private def getNamedType(typeName: String): Type with Named =
    typeDefCache.getOrElseUpdate(typeName, Schema.getBuiltInType(typeName) getOrElse (
      schemaDef.types.find(_.name == typeName) map buildType getOrElse (
        throw new SchemaMaterializationException(
          s"Invalid or incomplete schema, unknown type: $typeName. Ensure that a full introspection query is used in order to build a client schema."))))

  private def buildType(tpe: IntrospectionType) = tpe match {
    case o: IntrospectionObjectType ⇒ buildObjectDef(o)
    case i: IntrospectionInterfaceType ⇒ buildInterfaceDef(i)
    case u: IntrospectionUnionType ⇒ buildUnionDef(u)
    case io: IntrospectionInputObjectType ⇒ buildInputObjectDef(io)
    case s: IntrospectionScalarType ⇒ buildScalarDef(s)
    case e: IntrospectionEnumType ⇒ buildEnumDef(e)
  }

  private def buildField(field: IntrospectionField) =
    Field[Ctx, Any](
      name = field.name,
      fieldType = getOutputType(field.tpe),
      description = field.description,
      arguments = field.args.toList map buildArgument,
      resolve = logic.resolveField,
      tags = logic.fieldTags(field.name),
      deprecationReason = field.deprecationReason orElse (if (field.isDeprecated) Some("") else None),
      complexity = None,
      manualPossibleTypes = () ⇒ Nil)

  private def buildObjectDef(tpe: IntrospectionObjectType) =
    ObjectType[Ctx, Any](
      name = logic.rewriteTypeName(tpe.kind, tpe.name),
      description = tpe.description,
      fieldsFn = () ⇒ tpe.fields.toList map buildField,
      interfaces = tpe.interfaces.toList map getInterfaceType)

  private def buildInterfaceDef(tpe: IntrospectionInterfaceType) =
    InterfaceType[Ctx, Any](
      name = logic.rewriteTypeName(tpe.kind, tpe.name),
      description = tpe.description,
      fieldsFn = () ⇒ tpe.fields.toList map buildField,
      interfaces = Nil,
      manualPossibleTypes = () ⇒ Nil)

  private def buildUnionDef(tpe: IntrospectionUnionType) =
    UnionType[Ctx](
      name = logic.rewriteTypeName(tpe.kind, tpe.name),
      description = tpe.description,
      types = tpe.possibleTypes.toList map getObjectType)

  private def buildInputObjectDef(tpe: IntrospectionInputObjectType) =
    InputObjectType(
      name = logic.rewriteTypeName(tpe.kind, tpe.name),
      description = tpe.description,
      fieldsFn = () ⇒ tpe.inputFields.toList map buildInputField)

  private def buildScalarDef(tpe: IntrospectionScalarType): ScalarType[_] =
    ScalarType[Any](
      name = logic.rewriteTypeName(tpe.kind, tpe.name),
      description = tpe.description,
      coerceUserInput = value ⇒ logic.coerceScalarUserInput(tpe.name, value),
      coerceOutput = coerced ⇒ logic.coerceScalarOutput(tpe.name, coerced),
      coerceInput = value ⇒ logic.coerceScalarInput(tpe.name, value))

  private def buildEnumDef(tpe: IntrospectionEnumType) =
    EnumType[String](
      name = logic.rewriteTypeName(tpe.kind, tpe.name),
      description = tpe.description,
      values = tpe.enumValues.toList map buildEnumValue)

  private def buildEnumValue(value: IntrospectionEnumValue) =
    EnumValue[String](
      name = value.name,
      description = value.description,
      value = value.name,
      deprecationReason = value.deprecationReason orElse (if (value.isDeprecated) Some("") else None))
}

object IntrospectionSchemaMaterializer {
  /**
    * Build a `Schema` for use by client tools.
    *
    * Given the result of a client running the introspection query, creates and
    * returns a `Schema` instance which can be then used with all sangria
    * tools, but cannot be used to execute a query, as introspection does not
    * represent the "resolver", "parse" or "serialize" functions or any other
    * server-internal mechanisms.
    *
    * @param introspectionResult the result of introspection query
    */
  def buildSchema[T : InputUnmarshaller](introspectionResult: T): Schema[Unit, Unit] = {
    buildSchema[Unit, T](introspectionResult, MaterializationLogic.default)
  }

  /**
    * Build a `Schema` for use by client tools.
    *
    * Given the result of a client running the introspection query, creates and
    * returns a `Schema` instance which can be then used with all sangria
    * tools, but cannot be used to execute a query, as introspection does not
    * represent the "resolver", "parse" or "serialize" functions or any other
    * server-internal mechanisms.
    *
    * @param introspectionResult the result of introspection query
    * @param logic custom schema logic that would be used for all materialized fields. By default `MaterializedSchemaException` would be thrown.
    */
  def buildSchema[Ctx, T : InputUnmarshaller](introspectionResult: T, logic: MaterializationLogic[Ctx]): Schema[Ctx, Unit] =
    new IntrospectionSchemaMaterializer[Ctx, T](introspectionResult, logic).build
}

trait MaterializationLogic[Ctx] {
  def resolveField(ctx: Context[Ctx, _]): Action[Ctx, _]

  def coerceScalarUserInput(scalarName:String, value: Any): Either[Violation, Any]
  def coerceScalarInput(scalarName:String, value: ast.Value): Either[Violation, Any]
  def coerceScalarOutput(scalarName:String, coerced: Any): ast.Value

  def rewriteTypeName(kind: TypeKind.Value, name: String): String

  def fieldTags(fieldName: String): List[FieldTag]

  def defaultValueParser: Option[String ⇒ Try[(Any, InputUnmarshaller[Any])]]
}

object MaterializationLogic {
  def default[Ctx] = new DefaultMaterializationLogic[Ctx]

  def withDefaultValues[Ctx, T : InputUnmarshaller : InputParser] = new DefaultMaterializationLogic[Ctx] {
    override val defaultValueParser =
      Some((raw: String) ⇒ implicitly[InputParser[T]].parse(raw) map (_ → implicitly[InputUnmarshaller[T]].asInstanceOf[InputUnmarshaller[Any]]))
  }
}

class DefaultMaterializationLogic[Ctx] extends MaterializationLogic[Ctx] {
  import DefaultMaterializationLogic._

  def resolveField(ctx: Context[Ctx, _]): Action[Ctx, _] = throw MaterializedSchemaException

  def coerceScalarUserInput(scalarName: String, value: Any): Either[Violation, Any] = Left(MaterializedSchemaViolation)
  def coerceScalarOutput(scalarName: String, coerced: Any): ast.Value = throw MaterializedSchemaException
  def coerceScalarInput(scalarName: String, value: ast.Value): Either[Violation, Any] = Left(MaterializedSchemaViolation)

  def rewriteTypeName(kind: TypeKind.Value, name: String) = name

  def fieldTags(fieldName: String) = Nil

  /**
    * By default all default values are ignored because there is no knowledge how to parse them
    */
  def defaultValueParser: Option[String ⇒ Try[(Any, InputUnmarshaller[Any])]] = None
}

object DefaultMaterializationLogic {
  case object MaterializedSchemaException extends Exception("Schema was materialized and cannot be used for any queries except introspection queries.") with UserFacingError
  case object MaterializedSchemaViolation extends Violation {
    val errorMessage = "Schema was materialized and cannot be used for any queries except introspection queries."
  }

  case class ConstantToInput[T](iu: InputUnmarshaller[T]) extends ToInput[T, T] {
    def toInput(value: T) = value → iu
  }
}

case class SchemaMaterializationException(message: String, cause: Throwable = null) extends Exception(message, cause)
