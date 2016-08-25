package sangria.schema

import language.postfixOps
import sangria.ast
import sangria.ast.{OperationType, TypeDefinition}
import sangria.renderer.QueryRenderer

import scala.collection.concurrent.TrieMap

class AstSchemaMaterializer[Ctx] private (document: ast.Document, builder: AstSchemaBuilder[Ctx]) {
  import AstSchemaMaterializer.extractSchemaInfo

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

  // maybe it would work an effort to find more elegant way
  private var existingSchema: Option[Schema[_, _]] = None

  def extend[Val](schema: Schema[Ctx, Val]): Schema[Ctx, Val] = {
    validateExtensions(schema)

    if (typeDefs.isEmpty && typeExtensionDefs.isEmpty)
      schema
    else {
      existingSchema = Some(schema)

      val queryType = getTypeFromDef(schema.query)
      val mutationType = schema.mutation map getTypeFromDef
      val subscriptionType = schema.subscription map getTypeFromDef
      val directives = directiveDefs flatMap buildDirective

      val (referenced, notReferenced) = findUnusedTypes()

      builder.extendSchema[Val](
        schema,
        queryType,
        mutationType,
        subscriptionType,
        notReferenced ++ findUnusedTypes(schema, referenced),
        schema.directives.map(builder.transformDirective(_, this)) ++ directives,
        this)
    }
  }

  lazy val build: Schema[Ctx, Any] = {
    validateDefinitions()

    val schemaInfo = extractSchemaInfo(document, typeDefs)
    val queryType = getObjectType(schemaInfo.query)
    val mutationType = schemaInfo.mutation map getObjectType
    val subscriptionType = schemaInfo.subscription map getObjectType
    val directives = directiveDefs filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) flatMap buildDirective

    builder.buildSchema(
      schemaInfo.definition,
      queryType,
      mutationType,
      subscriptionType,
      findUnusedTypes()._2,
      BuiltinDirectives ++ directives,
      this)
  }

  def validateExtensions(schema: Schema[Ctx, _]): Unit = {
    typeDefsMap foreach {
      case (name, defs) if defs.size > 1 ⇒
        throw new SchemaMaterializationException(s"Type '$name' is defined more than once.")
      case (name, _) if schema.allTypes contains name ⇒
        throw new SchemaMaterializationException(
          s"Type '$name' already exists in the schema. It cannot also be defined in this type definition.")
      case _ ⇒ // everything is fine
    }

    directiveDefsMap foreach {
      case (name, defs) if defs.size > 1 ⇒
        throw new SchemaMaterializationException(s"Directive '$name' is defined more than once.")
      case (name, _) if schema.directivesByName contains name ⇒
        throw new SchemaMaterializationException(s"Directive '$name' already exists in the schema.")
      case _ ⇒ // everything is fine
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

  def findUnusedTypes(): (Set[String], List[Type with Named]) = {
    resolveAllLazyFields()

    val referenced = typeDefCache.keySet
    val notReferenced = typeDefs.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))

    referenced.toSet → notReferenced.map(tpe ⇒ getNamedType(tpe.name))
  }

  def findUnusedTypes(schema: Schema[_, _], referenced: Set[String]): List[Type with Named] = {
    resolveAllLazyFields()

    val notReferenced = schema.typeList.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))

    notReferenced map (tpe ⇒ getTypeFromDef(tpe))
  }

  // TODO: think about better solution
  def resolveAllLazyFields(): Unit =  {
    var prevCount = 0
    var newCount = 0
    var iteration = 0

    do {
      prevCount = typeDefCache.size
      iteration += 1

      typeDefCache.values.foreach {
        case o: ObjectLikeType[_, _] ⇒ o.fields
        case o: InputObjectType[_] ⇒ o.fields
        case _ ⇒ // do nothing
      }

      newCount = typeDefCache.size
    } while(prevCount != newCount && iteration < 20)
  }

  def getTypeFromExistingType(tpe: OutputType[_]): OutputType[Any] = tpe match {
    case ListType(ofType) ⇒ ListType(getTypeFromExistingType(ofType))
    case OptionType(ofType) ⇒ OptionType(getTypeFromExistingType(ofType))
    case t: Named ⇒ getTypeFromDef(t)
  }

  def getTypeFromDef[T <: Type with Named](tpe: T): T =
    getNamedType(tpe.name).asInstanceOf[T]

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
      existingSchema.flatMap(_.allTypes.get(typeName)).map(extendType) orElse typeDefs.find(_.name == typeName).flatMap(buildType) getOrElse (
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

  def extendType(existingType: Type with Named): Type with Named = existingType match {
    case tpe: ScalarType[_] ⇒ builder.transformScalarType(tpe, this)
    case tpe: EnumType[_] ⇒ builder.transformEnumType(tpe, this)
    case tpe: InputObjectType[_] ⇒ builder.transformInputObjectType(tpe, this)
    case tpe: UnionType[Ctx] ⇒ extendUnionType(tpe)
    case tpe: ObjectType[Ctx, _] ⇒ extendObjectType(tpe)
    case tpe: InterfaceType[Ctx, _] ⇒ extendInterfaceType(tpe)
  }

  def buildField(typeDefinition: ast.TypeDefinition, field: ast.FieldDefinition) =
    builder.buildField(
      typeDefinition,
      field,
      getOutputType(field.fieldType),
      field.arguments flatMap (buildArgument(typeDefinition, Some(field), _)),
      this)

  def extendField(tpe: ObjectLikeType[Ctx, _], field: Field[Ctx, _]) =
    builder.extendField(tpe, field.asInstanceOf[Field[Ctx, Any]], getTypeFromExistingType(field.fieldType), this)

  def buildObjectDef(tpe: ast.ObjectTypeDefinition) = {
    val extensions = findExtensions(tpe.name)

    builder.buildObjectType(
      tpe,
      extensions,
      () ⇒ buildFields(tpe, tpe.fields, extensions),
      buildInterfaces(tpe, tpe.interfaces, extensions),
      this)
  }

  def extendObjectType(tpe: ObjectType[Ctx, _]) = {
    val extensions = findExtensions(tpe.name)

    builder.extendObjectType(
      tpe,
      extensions,
      () ⇒ extendFields(tpe, extensions),
      extendInterfaces(tpe, extensions),
      this)
  }

  def buildInterfaceDef(tpe: ast.InterfaceTypeDefinition) = {
    val extensions = findExtensions(tpe.name)

    if (extensions.exists(_.definition.interfaces.nonEmpty))
      throw new SchemaMaterializationException(s"Extension of interface type '${tpe.name}' implements interfaces which is not allowed.")

    builder.buildInterfaceType(tpe, extensions, () ⇒ buildFields(tpe, tpe.fields, extensions), this)
  }

  def extendInterfaceType(tpe: InterfaceType[Ctx, _]) = {
    val extensions = findExtensions(tpe.name)

    if (extensions.exists(_.definition.interfaces.nonEmpty))
      throw new SchemaMaterializationException(s"Extension of interface type '${tpe.name}' implements interfaces which is not allowed.")

    builder.extendInterfaceType(tpe, extensions, () ⇒ extendFields(tpe, extensions), this)
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

  def extendInterfaces(tpe: ObjectType[Ctx, _], extensions: List[ast.TypeExtensionDefinition]) = {
    val extraInts = extensions.flatMap(_.definition.interfaces)

    val allInts = extraInts.foldLeft(List.empty[ast.NamedType]) {
      case (acc, interface) if tpe.allInterfaces.exists(_.name == interface.name) || acc.exists(_.name == interface.name) ⇒
        throw new SchemaMaterializationException(s"Type '${tpe.name}' already implements '${interface.name}'. It cannot also be implemented in this type extension.")
      case (acc, interface) ⇒ acc :+ interface
    }

    val ei = allInts map getInterfaceType
    val oi = tpe.interfaces map (getTypeFromDef(_).asInstanceOf[InterfaceType[Ctx, Any]])

    ei ++ oi
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

  def extendFields(tpe: ObjectLikeType[Ctx, _], extensions: List[ast.TypeExtensionDefinition]) = {
    val extraFields = extensions.flatMap(e ⇒ e.definition.fields map (e → _))

    val extensionFields = extraFields.foldLeft(List.empty[(ast.TypeExtensionDefinition, ast.FieldDefinition)]) {
      case (acc, field) if tpe.fieldsByName.contains(field._2.name) || acc.exists(_._2.name == field._2.name) ⇒
        throw new SchemaMaterializationException(s"Field '${tpe.name}.${field._2.name}' already exists in the schema. It cannot also be defined in this type extension.")
      case (acc, field) ⇒ acc :+ field
    }

    val ef = extensionFields flatMap (f ⇒ buildField(f._1.definition, f._2))
    val of = tpe.uniqueFields.toList map (extendField(tpe, _))

    of ++ ef
  }

  def findExtensions(typeName: String) =
    typeExtensionDefs.filter(_.definition.name == typeName)

  def buildUnionDef(tpe: ast.UnionTypeDefinition) =
    builder.buildUnionType(tpe, tpe.types map getObjectType, this)

  def extendUnionType(tpe: UnionType[Ctx]) =
    builder.extendUnionType(tpe, tpe.types map getTypeFromDef, this)

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

  def buildArgument(typeDefinition: ast.TypeSystemDefinition, fieldDef: Option[ast.FieldDefinition], value: ast.InputValueDefinition) =
    builder.buildArgument(typeDefinition, fieldDef, value, getInputType(value.valueType), buildDefault(value.defaultValue), this)

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
  case class SchemaInfo(query: ast.NamedType, mutation: Option[ast.NamedType], subscription: Option[ast.NamedType], definition: Option[ast.SchemaDefinition])

  def extractSchemaInfo(document: ast.Document, typeDefs: List[ast.TypeDefinition]): SchemaInfo = {
    val schemas = document.definitions.collect {case s: ast.SchemaDefinition ⇒ s}

//    throw new SchemaMaterializationException("Must provide a schema definition.")

    if (schemas.size > 1)
      throw new SchemaMaterializationException("Must provide only one schema definition.")
    else if (schemas.nonEmpty) {
      val schema = schemas.head

      val queries = schema.operationTypes.collect {case ast.OperationTypeDefinition(OperationType.Query, tpe, _, _) ⇒ tpe}
      val mutations = schema.operationTypes.collect {case ast.OperationTypeDefinition(OperationType.Mutation, tpe, _, _) ⇒ tpe}
      val subscriptions = schema.operationTypes.collect {case ast.OperationTypeDefinition(OperationType.Subscription, tpe, _, _) ⇒ tpe}

      if (queries.size != 1)
        throw new SchemaMaterializationException("Must provide only one query type in schema.")

      if (mutations.size > 1)
        throw new SchemaMaterializationException("Must provide only one mutation type in schema.")

      if (subscriptions.size > 1)
        throw new SchemaMaterializationException("Must provide only one subscription type in schema.")

      SchemaInfo(queries.head, mutations.headOption, subscriptions.headOption, Some(schema))
    } else {
      val query = typeDefs.find(_.name == "Query") getOrElse (
          throw new SchemaMaterializationException("Must provide schema definition with query type or a type named Query."))
      val mutation = typeDefs.find(_.name == "Mutation") map (t ⇒ ast.NamedType(t.name))
      val subscription = typeDefs.find(_.name == "Subscription") map (t ⇒ ast.NamedType(t.name))

      SchemaInfo(ast.NamedType(query.name), mutation, subscription, None)
    }
  }

  def buildSchema(document: ast.Document): Schema[Any, Any] = {
    buildSchema[Any](document, AstSchemaBuilder.default)
  }

  def buildSchema[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]): Schema[Ctx, Any] =
    new AstSchemaMaterializer[Ctx](document, builder).build

  def extendSchema[Ctx, Val](schema: Schema[Ctx, Val], document: ast.Document, builder: AstSchemaBuilder[Ctx] = AstSchemaBuilder.default): Schema[Ctx, Val] =
    new AstSchemaMaterializer[Ctx](document, builder).extend(schema)
}