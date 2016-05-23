package sangria.schema

import language.postfixOps

import sangria.ast
import sangria.ast.{DirectiveDefinition, TypeDefinition, SchemaDefinition, OperationType}
import sangria.execution.{FieldTag, UserFacingError}
import sangria.introspection._
import sangria.marshalling._
import sangria.parser.DeliveryScheme.Throw
import sangria.parser.QueryParser
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.validation.Violation

import scala.collection.concurrent.TrieMap
import scala.util.{Try, Failure, Success}

class AstSchemaMaterializer[Ctx] private (document: ast.Document, builder: AstSchemaBuilder[Ctx]) {
  import AstSchemaMaterializer.SchemaInfo

  private val typeDefCache = TrieMap[String, Type with Named]()

  private lazy val typeDefs: List[TypeDefinition] = document.definitions.collect {
    case d: ast.TypeDefinition ⇒ d
  } ++ builder.additionalTypeDefs

  private lazy val directiveDefs: List[DirectiveDefinition] = document.definitions.collect {
    case d: ast.DirectiveDefinition ⇒ d
  } ++ builder.additionalDirectiveDefs

  lazy val schemaInfo: SchemaInfo = {
    val schemas = document.definitions.collect {case s: ast.SchemaDefinition ⇒ s}

    if (schemas.isEmpty)
      throw new SchemaMaterializationException("Must provide a schema definition.")
    else if (schemas.size > 1)
      throw new SchemaMaterializationException("Must provide only one schema definition.")
    else {
      val schema = schemas.head

      val queries = schema.operationTypes.collect {case ast.OperationTypeDefinition(OperationType.Query, tpe, _, _) ⇒ tpe}
      val mutations = schema.operationTypes.collect {case ast.OperationTypeDefinition(OperationType.Mutation, tpe, _, _) ⇒ tpe}
      val subscriptions = schema.operationTypes.collect {case ast.OperationTypeDefinition(OperationType.Subscription, tpe, _, _) ⇒ tpe}

      if (queries.size != 1)
        throw new SchemaMaterializationException("Must provide one query type in schema.")

      if (mutations.size > 1)
        throw new SchemaMaterializationException("Must provide only one mutation type in schema.")

      if (subscriptions.size > 1)
        throw new SchemaMaterializationException("Must provide only one mutation type in schema.")

      SchemaInfo(queries.head, mutations.headOption, subscriptions.headOption, schema)
    }
  }

  lazy val build: Schema[Ctx, Unit] = {
    val queryType = getObjectType(schemaInfo.query)
    val mutationType = schemaInfo.mutation map getObjectType
    val subscriptionType = schemaInfo.subscription map getObjectType
    val directives = directiveDefs flatMap buildDirective

    builder.buildSchema(schemaInfo.definition, queryType, mutationType, subscriptionType, findUnusedTypes(), directives, this)
  }

  def findUnusedTypes(): List[Type with Named] = {
    // first init all lazy fields. TODO: think about better solution
    typeDefCache.values.foreach {
      case o: ObjectLikeType[_, _] ⇒ o.fields
      case o: InputObjectType[_] ⇒ o.fields
      case _ ⇒ // do nothing
    }

    val referenced = typeDefCache.keySet
    val notReferenced = typeDefs.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))

    notReferenced map (tpe ⇒ getNamedType(tpe.name))
  }

  def buildDirective(directive: ast.DirectiveDefinition) =
    BuiltinDirectives.find(_.name == directive.name) orElse
      builder.buildDirective(
        directive,
        directive.arguments flatMap (buildArgument(directive, None, _)),
        directive.locations map buildDirectiveLocation toSet,
        this)

  def getObjectType(tpe: ast.NamedType): ObjectType[Ctx, Any] =
    getOutputType(tpe, false) match {
      case obj: ObjectType[_, _] ⇒ obj.asInstanceOf[ObjectType[Ctx, Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${QueryRenderer.render(tpe)}' is not an object type.")
    }

  def getInterfaceType(tpe: ast.NamedType) =
    getOutputType(tpe, false) match {
      case obj: InterfaceType[_, _] ⇒ obj.asInstanceOf[InterfaceType[Ctx, Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${QueryRenderer.render(tpe)}' is not an interface type.")
    }

  def getInputType(tpe: ast.Type, optional: Boolean = true): InputType[_] =
    tpe match {
      case ast.ListType(ofType, _) if optional ⇒ OptionInputType(ListInputType(getInputType(ofType, true)))
      case ast.ListType(ofType, _) ⇒ ListInputType(getInputType(ofType, true))
      case ast.NotNullType(ofType, _) ⇒ getInputType(ofType, false)
      case ast.NamedType(name, _) ⇒
        getNamedType(name) match {
          case input: InputType[_] if optional ⇒ OptionInputType(input)
          case input: InputType[_] ⇒ input
          case _ ⇒ throw new SchemaMaterializationException(s"Type '$name' is not an input type, but was used in input type position!")
        }
    }

  def getOutputType(tpe: ast.Type, optional: Boolean = true): OutputType[_] =
    tpe match {
      case ast.ListType(ofType, _) if optional ⇒ OptionType(ListType(getOutputType(ofType, true)))
      case ast.ListType(ofType, _) ⇒ ListType(getOutputType(ofType, true))
      case ast.NotNullType(ofType, _) ⇒ getOutputType(ofType, false)
      case ast.NamedType(name, _) ⇒
        getNamedType(name) match {
          case input: OutputType[_] if optional ⇒ OptionType(input)
          case input: OutputType[_] ⇒ input
          case _ ⇒ throw new SchemaMaterializationException(s"Type '$name' is not an output type, but was used in output type position!")
        }
    }

  def getNamedType(typeName: String): Type with Named =
    typeDefCache.getOrElseUpdate(typeName, Schema.getBuiltInType(typeName) getOrElse (
      typeDefs.find(_.name == typeName) flatMap buildType getOrElse (
        throw new SchemaMaterializationException(
          s"Invalid or incomplete schema, unknown type: $typeName."))))

  def buildType(definition: TypeDefinition): Option[Type with Named] = definition match {
    case d: ast.ObjectTypeDefinition ⇒ buildObjectDef(d)
    case d: ast.InterfaceTypeDefinition ⇒ buildInterfaceDef(d)
    case d: ast.UnionTypeDefinition ⇒ buildUnionDef(d)
    case d: ast.InputObjectTypeDefinition ⇒ buildInputObjectDef(d)
    case d: ast.ScalarTypeDefinition ⇒ buildScalarDef(d)
    case d: ast.EnumTypeDefinition ⇒ buildEnumDef(d)
  }

  def buildField(typeDef: ast.TypeDefinition, field: ast.FieldDefinition) =
    builder.buildField(typeDef, field, getOutputType(field.fieldType), field.arguments flatMap (buildArgument(typeDef, Some(field), _)), this)

  def buildObjectDef(tpe: ast.ObjectTypeDefinition) =
    builder.buildObjectType(
      tpe,
      () ⇒ tpe.fields flatMap (buildField(tpe, _)),
      tpe.interfaces map getInterfaceType,
      this)

  def buildInterfaceDef(tpe: ast.InterfaceTypeDefinition) =
    builder.buildInterfaceType(tpe, () ⇒ tpe.fields flatMap (buildField(tpe, _)), this)

  def buildUnionDef(tpe: ast.UnionTypeDefinition) =
    builder.buildUnionType(tpe, tpe.types map getObjectType, this)

  def buildInputObjectDef(tpe: ast.InputObjectTypeDefinition) =
    builder.buildInputObjectType(tpe, () ⇒ tpe.fields flatMap (buildInputField(tpe, _)), this)

  def buildScalarDef(tpe: ast.ScalarTypeDefinition) =
    builder.buildScalarType(tpe, this)

  private def buildEnumDef(tpe: ast.EnumTypeDefinition) =
    builder.buildEnumType(tpe, tpe.values flatMap (buildEnumValue(tpe, _)), this)

  private def buildEnumValue(typeDef: ast.EnumTypeDefinition, value: ast.EnumValueDefinition) =
    builder.buildEnumValue(typeDef, value, this)

  def buildDefault(defaultValue: Option[ast.Value]) =
    defaultValue map (dv ⇒ dv → sangria.marshalling.queryAst.queryAstToInput)

  def buildArgument(typeDef: ast.TypeSystemDefinition, fieldDef: Option[ast.FieldDefinition], value: ast.InputValueDefinition) =
    builder.buildArgument(typeDef, fieldDef, value, getInputType(value.valueType), buildDefault(value.defaultValue), this)

  def buildInputField(typeDef: ast.InputObjectTypeDefinition, value: ast.InputValueDefinition) =
    builder.buildInputField(typeDef, value, getInputType(value.valueType), buildDefault(value.defaultValue), this)

  def buildDirectiveLocation(loc: ast.DirectiveLocation) =
    try {
      DirectiveLocation.fromString(loc.name)
    } catch {
      case e: MatchError ⇒ throw new SchemaMaterializationException(s"Unknown directive location '${loc.name}'.")
    }
}

object AstSchemaMaterializer {
  case class SchemaInfo(query: ast.NamedType, mutation: Option[ast.NamedType], subscription: Option[ast.NamedType], definition: ast.SchemaDefinition)

  def buildSchema(document: ast.Document): Schema[Unit, Unit] = {
    buildSchema[Unit](document, AstSchemaBuilder.default)
  }

  def buildSchema[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]): Schema[Ctx, Unit] =
    new AstSchemaMaterializer[Ctx](document, builder).build
}