package sangria.schema

import sangria.introspection._
import sangria.marshalling._
import sangria.parser.DeliveryScheme.Throw
import sangria.renderer.SchemaRenderer

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success}

class IntrospectionSchemaMaterializer[Ctx, T : InputUnmarshaller](introspectionResult: T, builder: IntrospectionSchemaBuilder[Ctx]) {
  private val typeDefCache = TrieMap[String, Type with Named]()

  private lazy val schemaDef = IntrospectionParser.parse(introspectionResult)

  lazy val build: Schema[Ctx, Any] = {
    val queryType = getObjectType(schemaDef.queryType)
    val mutationType = schemaDef.mutationType map getObjectType
    val subscriptionType = schemaDef.subscriptionType map getObjectType
    val directives = (schemaDef.directives.toList ++ builder.additionalDirectiveDefs) filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) flatMap buildDirective

    builder.buildSchema(schemaDef, queryType, mutationType, subscriptionType,
      findUnusedTypes(schemaDef.types ++ builder.additionalTypeDefs),
      BuiltinDirectives ++ directives,
      this)
  }

  def findUnusedTypes(allTypes: Seq[IntrospectionType]): List[Type with Named] = {
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

  def buildDirective(directive: IntrospectionDirective) =
    BuiltinDirectives.find(_.name == directive.name) orElse
      builder.buildDirective(directive, directive.args.toList flatMap (buildArgument(None, _)), this)

  def getObjectType(typeRef: IntrospectionTypeRef): ObjectType[Ctx, Any] =
    getOutputType(typeRef, false) match {
      case obj: ObjectType[_, _] ⇒ obj.asInstanceOf[ObjectType[Ctx, Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${SchemaRenderer.renderTypeName(typeRef)}' is not an object type.")
    }

  def getInterfaceType(typeRef: IntrospectionTypeRef) =
    getOutputType(typeRef, false) match {
      case obj: InterfaceType[_, _] ⇒ obj.asInstanceOf[InterfaceType[Ctx, Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${SchemaRenderer.renderTypeName(typeRef)}' is not an interface type.")
    }

  def getInputType(typeRef: IntrospectionTypeRef, optional: Boolean = true): InputType[_] =
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

  def getOutputType(typeRef: IntrospectionTypeRef, optional: Boolean = true): OutputType[_] =
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

  def getNamedType(typeName: String): Type with Named =
    typeDefCache.getOrElseUpdate(typeName, Schema.getBuiltInType(typeName) getOrElse (
      schemaDef.types.find(_.name == typeName) flatMap buildType getOrElse (
        throw new SchemaMaterializationException(
          s"Invalid or incomplete schema, unknown type: $typeName. Ensure that a full introspection query is used in order to build a client schema."))))

  def buildType(tpe: IntrospectionType): Option[Type with Named] = tpe match {
    case o: IntrospectionObjectType ⇒ buildObjectDef(o)
    case i: IntrospectionInterfaceType ⇒ buildInterfaceDef(i)
    case u: IntrospectionUnionType ⇒ buildUnionDef(u)
    case io: IntrospectionInputObjectType ⇒ buildInputObjectDef(io)
    case s: IntrospectionScalarType ⇒ buildScalarDef(s)
    case e: IntrospectionEnumType ⇒ buildEnumDef(e)
  }

  def buildField(typeDef: IntrospectionType, field: IntrospectionField) =
    builder.buildField(typeDef, field, getOutputType(field.tpe), field.args.toList flatMap (buildArgument(Some(field), _)), this)

  def buildObjectDef(tpe: IntrospectionObjectType) =
    builder.buildObjectType(tpe, () ⇒ tpe.fields.toList flatMap (buildField(tpe, _)), tpe.interfaces.toList map getInterfaceType, this)

  def buildInterfaceDef(tpe: IntrospectionInterfaceType) =
    builder.buildInterfaceType(tpe, () ⇒ tpe.fields.toList flatMap (buildField(tpe, _)), this)

  def buildUnionDef(tpe: IntrospectionUnionType) =
    builder.buildUnionType(tpe, tpe.possibleTypes.toList map getObjectType, this)

  def buildInputObjectDef(tpe: IntrospectionInputObjectType) =
    builder.buildInputObjectType(tpe, () ⇒ tpe.inputFields.toList flatMap (buildInputField(tpe, _)), this)

  def buildScalarDef(tpe: IntrospectionScalarType) =
    builder.buildScalarType(tpe, this)

  def buildEnumDef(tpe: IntrospectionEnumType) =
    builder.buildEnumType(tpe, tpe.enumValues.toList flatMap (buildEnumValue(tpe, _)), this)

  def buildEnumValue(tpe: IntrospectionEnumType, value: IntrospectionEnumValue) =
    builder.buildEnumValue(tpe, value, this)

  def buildDefault(defaultValue: Option[String]) =
    defaultValue map (dv ⇒ sangria.marshalling.queryAst.QueryAstInputParser.parse(dv) match {
      case Success(parsed) ⇒ parsed → sangria.marshalling.queryAst.queryAstToInput
      case Failure(error) ⇒ throw new SchemaMaterializationException(s"Unable to parse default value '$dv'.", error)
    })

  def buildArgument(fieldDef: Option[IntrospectionField], value: IntrospectionInputValue) =
    builder.buildArgument(fieldDef, value, getInputType(value.tpe), buildDefault(value.defaultValue), this)

  def buildInputField(tpe: IntrospectionInputObjectType, value: IntrospectionInputValue) =
    builder.buildInputField(tpe, value, getInputType(value.tpe), buildDefault(value.defaultValue), this)
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
  def buildSchema[T : InputUnmarshaller](introspectionResult: T): Schema[Any, Any] = {
    buildSchema[Any, T](introspectionResult, IntrospectionSchemaBuilder.default)
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
    * @param builder custom schema construction logic. By default `MaterializedSchemaException` would be thrown from a `resolve` function.
    */
  def buildSchema[Ctx, T : InputUnmarshaller](introspectionResult: T, builder: IntrospectionSchemaBuilder[Ctx]): Schema[Ctx, Any] =
    new IntrospectionSchemaMaterializer[Ctx, T](introspectionResult, builder).build
}
