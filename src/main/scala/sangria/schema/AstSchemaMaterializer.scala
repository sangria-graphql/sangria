package sangria.schema

import language.postfixOps

import sangria.ast
import sangria.ast.{TypeDefinition, OperationType}
import sangria.renderer.QueryRenderer

import scala.collection.concurrent.TrieMap

class AstSchemaMaterializer[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]) {
  import AstSchemaMaterializer.SchemaInfo

  private val typeDefCache = TrieMap[String, Type with Named]()

  private lazy val typeDefs: List[ast.TypeDefinition] = document.definitions.collect {
    case d: ast.TypeDefinition ⇒ d
  } ++ builder.additionalTypeDefs

  private lazy val typeExtensionDefs: List[ast.TypeExtensionDefinition] = document.definitions.collect {
    case d: ast.TypeExtensionDefinition ⇒ d
  } ++ builder.additionalTypeExtensionDefs

  private lazy val directiveDefs: List[ast.DirectiveDefinition] = document.definitions.collect {
    case d: ast.DirectiveDefinition ⇒ d
  } ++ builder.additionalDirectiveDefs

  private lazy val directiveDefsMap = directiveDefs.groupBy(_.name)
  private lazy val typeDefsMap = typeDefs.groupBy(_.name)

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
        throw new SchemaMaterializationException("Must provide only one subscription type in schema.")

      SchemaInfo(queries.head, mutations.headOption, subscriptions.headOption, schema)
    }
  }

  def extend[Val](schema: Schema[Ctx, Val]): Schema[Ctx, Val] = {
    validateExtensions(schema)

    if (typeDefs.isEmpty && typeExtensionDefs.isEmpty)
      schema
    else {
      val queryType = getTypeFromDef(schema.query)
      val mutationType = schema.mutation map getTypeFromDef
      val subscriptionType = schema.subscription map getTypeFromDef
      val directives = directiveDefs flatMap buildDirective

      builder.buildSchema(
        None,
        queryType.asInstanceOf[ObjectType[Ctx, Any]],
        mutationType.asInstanceOf[Option[ObjectType[Ctx, Any]]],
        subscriptionType.asInstanceOf[Option[ObjectType[Ctx, Any]]],
        findUnusedTypes() ++ findUnusedTypes(schema),
        schema.directives ++ directives,
        schema.validationRules,
        this).asInstanceOf[Schema[Ctx, Val]]
    }
  }

  lazy val build: Schema[Ctx, Any] = {
    validateDefinitions()

    val queryType = getObjectType(schemaInfo.query)
    val mutationType = schemaInfo.mutation map getObjectType
    val subscriptionType = schemaInfo.subscription map getObjectType
    val directives = directiveDefs filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) flatMap buildDirective

    builder.buildSchema(
      Some(schemaInfo.definition),
      queryType,
      mutationType,
      subscriptionType,
      findUnusedTypes(),
      BuiltinDirectives ++ directives,
      SchemaValidationRule.default,
      this)
  }

  def validateExtensions(schema: Schema[Ctx, _]): Unit = {
    typeDefsMap foreach {
      case (name, defs) if defs.size > 1 ⇒
        throw new SchemaMaterializationException(s"Type '$name' is defined more than once.")
      case (name, _) if schema.allTypes contains name ⇒
        throw new SchemaMaterializationException(
          s"Type '$name' already exists in the schema. It cannot also be defined in this type definition.")
    }

    directiveDefsMap foreach {
      case (name, defs) if defs.size > 1 ⇒
        throw new SchemaMaterializationException(s"Directive '$name' is defined more than once.")
      case (name, _) if schema.directivesByName contains name ⇒
        throw new SchemaMaterializationException(s"Directive '$name' already exists in the schema.")
    }

    typeExtensionDefs.foreach { ext ⇒
      typeDefsMap.get(ext.definition.name).map(_.head) match {
        case Some(tpe: ast.ObjectTypeDefinition) ⇒ // everything is fine
        case Some(tpe) ⇒ throw new SchemaMaterializationException(s"Cannot extend non-object type '${tpe.name}'.")
        case None ⇒
          schema.allTypes.get(ext.definition.name) match {
            case Some(tpe: ObjectType[_, _]) ⇒ // everything is fine
            case Some(tpe) ⇒ throw new SchemaMaterializationException(s"Cannot extend non-object type '${tpe.name}'.")
            case None ⇒ throw new SchemaMaterializationException(s"Cannot extend type '${ext.definition.name}' because it does not exist.")
          }
      }
    }
  }

  def validateDefinitions(): Unit = {
    typeDefsMap.find(_._2.size > 1) foreach { case (name, _) ⇒
      throw new SchemaMaterializationException(s"Type '$name' is defined more than once.")
    }

    directiveDefsMap.find(_._2.size > 1) foreach { case (name, _) ⇒
      throw new SchemaMaterializationException(s"Directive '$name' is defined more than once.")
    }

    typeExtensionDefs.foreach { ext ⇒
      typeDefsMap.get(ext.definition.name).map(_.head) match {
        case Some(tpe: ast.ObjectTypeDefinition) ⇒ // everything is fine
        case Some(tpe) ⇒ throw new SchemaMaterializationException(s"Cannot extend non-object type '${tpe.name}'.")
        case None ⇒ throw new SchemaMaterializationException(s"Cannot extend type '${ext.definition.name}' because it does not exist.")
      }
    }
  }

  def findUnusedTypes(): List[Type with Named] = {
    resolveAllLazyFields()

    val referenced = typeDefCache.keySet
    val notReferenced = typeDefs.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))

    notReferenced map (tpe ⇒ getNamedType(tpe.name))
  }

  def findUnusedTypes(schema: Schema[_, _]): List[Type with Named] = {
    resolveAllLazyFields()

    val referenced = typeDefCache.keySet
    val notReferenced = schema.typeList.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))

    notReferenced map (tpe ⇒ getTypeFromDef(tpe))
  }

  // TODO: think about better solution
  def resolveAllLazyFields(): Unit =  {
    typeDefCache.values.foreach {
      case o: ObjectLikeType[_, _] ⇒ o.fields
      case o: InputObjectType[_] ⇒ o.fields
      case _ ⇒ // do nothing
    }
  }

  def getTypeFromDef[T <: Type](tpe: T): T = tpe // TODO

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

  def buildObjectDef(tpe: ast.ObjectTypeDefinition) = {
    val extensions = findExtensions(tpe.name)

    builder.buildObjectType(
      tpe,
      extensions,
      () ⇒ buildFields(tpe, tpe.fields, extensions),
      buildInterfaces(tpe, tpe.interfaces, extensions),
      this)
  }

  def buildInterfaceDef(tpe: ast.InterfaceTypeDefinition) = {
    val extensions = findExtensions(tpe.name)

    if (extensions.exists(_.definition.interfaces.nonEmpty))
      throw new SchemaMaterializationException(s"Extension of interface type '${tpe.name}' implements interfaces which is not allowed.")

    builder.buildInterfaceType(tpe, extensions, () ⇒ buildFields(tpe, tpe.fields, extensions), this)
  }

  def buildInterfaces(tpe: ast.ObjectTypeDefinition, interfaces: List[ast.NamedType], extensions: List[ast.TypeExtensionDefinition]) = {
    val extraInts = extensions.flatMap(_.definition.interfaces)

    val allInts = extraInts.foldLeft(interfaces) {
      case (acc, interface) if acc.exists(_.name == interface.name) ⇒
        throw new SchemaMaterializationException(s"Type '${tpe.name}' already implements '${interface.name}'. It cannot also be implemented in this type extension.")
      case (acc, interface) ⇒ acc :+ interface
    }

    allInts map getInterfaceType
  }

  def buildFields(tpe: TypeDefinition, fieldDefs: List[ast.FieldDefinition], extensions: List[ast.TypeExtensionDefinition]) = {
    val extraFields = extensions.flatMap(_.definition.fields)

    val allFields = extraFields.foldLeft(fieldDefs) {
      case (acc, field) if acc.exists(_.name == field.name) ⇒
        throw new SchemaMaterializationException(s"Field '${tpe.name}.${field.name}' already exists in the schema. It cannot also be defined in this type extension.")
      case (acc, field) ⇒ acc :+ field
    }

    allFields flatMap (buildField(tpe, _))
  }

  def findExtensions(typeName: String) =
    typeExtensionDefs.filter(_.definition.name == typeName)

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

  def buildSchema(document: ast.Document): Schema[Any, Any] = {
    buildSchema[Any](document, AstSchemaBuilder.default)
  }

  def buildSchema[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]): Schema[Ctx, Any] =
    new AstSchemaMaterializer[Ctx](document, builder).build
}