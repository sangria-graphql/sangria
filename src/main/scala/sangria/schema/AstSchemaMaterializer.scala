package sangria.schema

import language.postfixOps

import sangria.ast
import sangria.ast.{OperationType, TypeDefinition}
import sangria.renderer.QueryRenderer

import scala.collection.concurrent.TrieMap

class AstSchemaMaterializer[Ctx] private (document: ast.Document, builder: AstSchemaBuilder[Ctx]) {
  import AstSchemaMaterializer.extractSchemaInfo

  private val typeDefCache = TrieMap[(MatOrigin, String), Type with Named]()
  private val scalarAliasCache = TrieMap[ScalarAlias[_, _], ScalarAlias[_, _]]()

  private lazy val typeDefs: Vector[ast.TypeDefinition] = document.definitions.collect {
    case d: ast.TypeDefinition ⇒ d
  }

  private lazy val typeDefsMat: Vector[MaterializedType] = typeDefs.map(MaterializedType(SDLOrigin, _))

  private lazy val typeExtensionDefs: Vector[ast.TypeExtensionDefinition] = document.definitions.collect {
    case d: ast.TypeExtensionDefinition ⇒ d
  } ++ builder.additionalTypeExtensionDefs

  private lazy val directiveDefs: Vector[ast.DirectiveDefinition] = document.definitions.collect {
    case d: ast.DirectiveDefinition ⇒ d
  } ++ builder.additionalDirectiveDefs

  private lazy val directiveDefsMap = directiveDefs.groupBy(_.name)
  private lazy val typeDefsMap = typeDefs.groupBy(_.name)

  // maybe it would worth an effort to find more elegant way
  private var existingSchema: Option[Schema[_, _]] = None
  private var existingDefsMat: Map[String, MaterializedType] = Map.empty

  def extend[Val](schema: Schema[Ctx, Val]): Schema[Ctx, Val] = {
    validateExtensions(schema)

    if (typeDefs.isEmpty && typeExtensionDefs.isEmpty)
      schema
    else {
      existingSchema = Some(schema)
      existingDefsMat = schema.allTypes.mapValues(MaterializedType(ExistingOrigin, _))

      val queryType = getTypeFromDef(ExistingOrigin, schema.query)
      val mutationType = schema.mutation map (getTypeFromDef(ExistingOrigin, _))
      val subscriptionType = schema.subscription map (getTypeFromDef(ExistingOrigin, _))
      val directives = directiveDefs flatMap (buildDirective(ExistingOrigin, _))

      val (referenced, notReferenced) = findUnusedTypes()

      builder.extendSchema[Val](
        schema,
        queryType,
        mutationType,
        subscriptionType,
        (notReferenced ++ findUnusedTypes(schema, referenced)).toList,
        schema.directives.map(builder.transformDirective(ExistingOrigin, _, this)) ++ directives,
        this)
    }
  }

  lazy val build: Schema[Ctx, Any] = {
    validateDefinitions()

    val schemaInfo = extractSchemaInfo(document, typeDefs)
    val queryType = getObjectType(SDLOrigin, schemaInfo.query)
    val mutationType = schemaInfo.mutation map (getObjectType(SDLOrigin, _))
    val subscriptionType = schemaInfo.subscription map (getObjectType(SDLOrigin, _))
    val directives = directiveDefs filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) flatMap (buildDirective(SDLOrigin, _))

    builder.buildSchema(
      schemaInfo.definition,
      queryType,
      mutationType,
      subscriptionType,
      findUnusedTypes()._2.toList,
      BuiltinDirectives ++ directives,
      this)
  }

  lazy val definitions: Vector[Named] = {
    validateDefinitions()

    val directives = directiveDefs filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) flatMap (buildDirective(SDLOrigin, _))
    val unused = findUnusedTypes()

    unused._1.toVector.map(getNamedType(SDLOrigin, _)) ++ unused._2 ++ directives
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

  def findUnusedTypes(): (Set[String], Vector[Type with Named]) = {
    resolveAllLazyFields()

    val referenced = typeDefCache.map(_._2.name).toSet
    val notReferenced = typeDefs.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))
    val notReferencedAdd = builder.additionalTypes.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))

    referenced → (notReferenced.map(tpe ⇒ getNamedType(SDLOrigin, tpe.name)) ++ notReferencedAdd.map(tpe ⇒ getNamedType(tpe.origin, tpe.name)))
  }

  def findUnusedTypes(schema: Schema[_, _], referenced: Set[String]): Vector[Type with Named] = {
    resolveAllLazyFields()

    val notReferenced = schema.typeList.filterNot(tpe ⇒ Schema.isBuiltInType(tpe.name) || referenced.contains(tpe.name))

    notReferenced map (tpe ⇒ getTypeFromDef(ExistingOrigin, tpe))
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

  def getTypeFromExistingType(origin: MatOrigin, tpe: OutputType[_]): OutputType[Any] = tpe match {
    case ListType(ofType) ⇒ ListType(getTypeFromExistingType(origin, ofType))
    case OptionType(ofType) ⇒ OptionType(getTypeFromExistingType(origin, ofType))
    case t: Named ⇒ getTypeFromDef(origin, t)
  }

  def getTypeFromDef[T <: Type with Named](origin: MatOrigin, tpe: T): T =
    tpe match {
      case alias: ScalarAlias[Any, Any] @unchecked ⇒
        scalarAliasCache.getOrElseUpdate(alias, {
          extendScalarAlias(origin, alias)
        }).asInstanceOf[T]
      case _ ⇒
        getNamedType(origin, tpe.name).asInstanceOf[T]
    }


  def buildDirective(origin: MatOrigin, directive: ast.DirectiveDefinition) =
    BuiltinDirectives.find(_.name == directive.name) orElse
      builder.buildDirective(
        origin,
        directive,
        directive.arguments flatMap (buildArgument(origin, directive, None, _)) toList,
        directive.locations map buildDirectiveLocation toSet,
        this)

  def getObjectType(origin: MatOrigin, tpe: ast.NamedType): ObjectType[Ctx, Any] =
    getOutputType(origin, tpe, false) match {
      case obj: ObjectType[_, _] ⇒ obj.asInstanceOf[ObjectType[Ctx, Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${QueryRenderer.render(tpe)}' is not an object type.")
    }

  def getScalarType(origin: MatOrigin, tpe: ast.NamedType): ScalarType[Any] =
    getOutputType(origin, tpe, false) match {
      case obj: ScalarType[_] ⇒ obj.asInstanceOf[ScalarType[Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${QueryRenderer.render(tpe)}' is not a scalar type.")
    }

  def getInterfaceType(origin: MatOrigin, tpe: ast.NamedType) =
    getOutputType(origin, tpe, false) match {
      case obj: InterfaceType[_, _] ⇒ obj.asInstanceOf[InterfaceType[Ctx, Any]]
      case _ ⇒ throw new SchemaMaterializationException(s"Type '${QueryRenderer.render(tpe)}' is not an interface type.")
    }

  def getInputType(origin: MatOrigin, tpe: ast.Type, optional: Boolean = true): InputType[_] =
    tpe match {
      case ast.ListType(ofType, _) if optional ⇒ OptionInputType(ListInputType(getInputType(origin, ofType, true)))
      case ast.ListType(ofType, _) ⇒ ListInputType(getInputType(origin, ofType, true))
      case ast.NotNullType(ofType, _) ⇒ getInputType(origin, ofType, false)
      case ast.NamedType(name, _) ⇒
        getNamedType(origin, name) match {
          case input: InputType[_] if optional ⇒ OptionInputType(input)
          case input: InputType[_] ⇒ input
          case _ ⇒ throw new SchemaMaterializationException(s"Type '$name' is not an input type, but was used in input type position!")
        }
    }

  def getOutputType(origin: MatOrigin, tpe: ast.Type, optional: Boolean = true): OutputType[_] =
    tpe match {
      case ast.ListType(ofType, _) if optional ⇒ OptionType(ListType(getOutputType(origin, ofType, true)))
      case ast.ListType(ofType, _) ⇒ ListType(getOutputType(origin, ofType, true))
      case ast.NotNullType(ofType, _) ⇒ getOutputType(origin, ofType, false)
      case ast.NamedType(name, _) ⇒
        getNamedType(origin, name) match {
          case input: OutputType[_] if optional ⇒ OptionType(input)
          case input: OutputType[_] ⇒ input
          case _ ⇒ throw new SchemaMaterializationException(s"Type '$name' is not an output type, but was used in output type position!")
        }
    }

  def getNamedType(origin: MatOrigin, typeName: String): Type with Named =
    typeDefCache.getOrElseUpdate(origin → typeName, Schema.getBuiltInType(typeName) getOrElse {
      val existing = existingDefsMat.get(typeName).toVector
      val sdl = typeDefsMat.filter(_.name == typeName)
      val additional = builder.additionalTypes.filter(_.name == typeName).toVector

      val allCandidates = existing ++ sdl ++ additional

      val builtType =
        if (allCandidates.size > 1) {
          val resolved = builder.resolveNameConflict(
            origin,
            allCandidates ++
              typeDefCache.find(_._2.name == typeName).map{case ((o, _), v) ⇒ BuiltMaterializedTypeInst(o, v)}.toVector)

          if (!resolved.isInstanceOf[BuiltMaterializedTypeInst] && typeDefCache.keySet.exists(_._2 == resolved.name))
            throw SchemaMaterializationException("Name conflict resolution produced already existing type name")
          else
            getNamedType(origin, resolved)
        } else if (allCandidates.nonEmpty) {
          getNamedType(origin, allCandidates.head)
        } else None

      builtType getOrElse (throw new SchemaMaterializationException(s"Invalid or incomplete schema, unknown type: $typeName."))
    })

  def getNamedType(origin: MatOrigin, tpe: MaterializedType): Option[Type with Named] =
    tpe match {
      case BuiltMaterializedTypeInst(o, t) ⇒ Some(t)
      case MaterializedTypeInst(o, t) ⇒ Some(extendType(o, t))
      case MaterializedTypeAst(o, t) ⇒ buildType(o, t)
    }

  def buildType(origin: MatOrigin, definition: TypeDefinition): Option[Type with Named] = definition match {
    case d: ast.ObjectTypeDefinition ⇒ buildObjectDef(origin, d)
    case d: ast.InterfaceTypeDefinition ⇒ buildInterfaceDef(origin, d)
    case d: ast.UnionTypeDefinition ⇒ buildUnionDef(origin, d)
    case d: ast.InputObjectTypeDefinition ⇒ buildInputObjectDef(origin, d)
    case d: ast.ScalarTypeDefinition ⇒ buildScalarDef(origin, d)
    case d: ast.EnumTypeDefinition ⇒ buildEnumDef(origin, d)
  }

  def extendType(origin: MatOrigin, existingType: Type with Named): Type with Named = existingType match {
    case tpe: ScalarType[_] ⇒ builder.transformScalarType(origin, tpe, this)
    case tpe: ScalarAlias[_, _] ⇒ extendScalarAlias(origin, tpe.asInstanceOf[ScalarAlias[Any, Any]])
    case tpe: EnumType[_] ⇒ builder.transformEnumType(origin, tpe, this)
    case tpe: InputObjectType[_] ⇒ builder.transformInputObjectType(origin, tpe, this)
    case tpe: UnionType[Ctx] ⇒ extendUnionType(origin, tpe)
    case tpe: ObjectType[Ctx, _] ⇒ extendObjectType(origin, tpe)
    case tpe: InterfaceType[Ctx, _] ⇒ extendInterfaceType(origin, tpe)
  }

  def buildField(origin: MatOrigin, typeDefinition: ast.TypeDefinition, extensions: Vector[ast.TypeExtensionDefinition], field: ast.FieldDefinition) = {
    val args = field.arguments flatMap (buildArgument(origin, typeDefinition, Some(field), _)) toList
    val fieldType = builder.buildFieldType(origin, typeDefinition, extensions, field, args, this)

    builder.buildField(origin, typeDefinition, extensions, field, fieldType, args, this)
  }


  def extendField(origin: MatOrigin, tpe: Option[ObjectLikeType[Ctx, _]], field: Field[Ctx, _]) = {
    val f = field.asInstanceOf[Field[Ctx, Any]]

    builder.extendField(origin, tpe, f, builder.extendFieldType(origin, tpe, f, this), this)
  }

  def buildObjectDef(origin: MatOrigin, tpe: ast.ObjectTypeDefinition) = {
    val extensions = findExtensions(tpe.name)

    builder.buildObjectType(
      origin,
      tpe,
      extensions.toList,
      () ⇒ buildFields(origin, tpe, tpe.fields, extensions).toList,
      buildInterfaces(origin, tpe, tpe.interfaces, extensions).toList,
      this)
  }

  def extendObjectType(origin: MatOrigin, tpe: ObjectType[Ctx, _]) = {
    val extensions = findExtensions(tpe.name)

    builder.extendObjectType(
      origin,
      tpe,
      extensions.toList,
      () ⇒ extendFields(origin, tpe, extensions),
      extendInterfaces(origin, tpe, extensions),
      this)
  }

  def buildInterfaceDef(origin: MatOrigin, tpe: ast.InterfaceTypeDefinition) = {
    val extensions = findExtensions(tpe.name)

    if (extensions.exists(_.definition.interfaces.nonEmpty))
      throw new SchemaMaterializationException(s"Extension of interface type '${tpe.name}' implements interfaces which is not allowed.")

    builder.buildInterfaceType(origin, tpe, extensions.toList, () ⇒ buildFields(origin, tpe, tpe.fields, extensions).toList, this)
  }

  def extendInterfaceType(origin: MatOrigin, tpe: InterfaceType[Ctx, _]) = {
    val extensions = findExtensions(tpe.name)

    if (extensions.exists(_.definition.interfaces.nonEmpty))
      throw new SchemaMaterializationException(s"Extension of interface type '${tpe.name}' implements interfaces which is not allowed.")

    builder.extendInterfaceType(origin, tpe, extensions.toList, () ⇒ extendFields(origin, tpe, extensions), this)
  }

  def buildInterfaces(origin: MatOrigin, tpe: ast.ObjectTypeDefinition, interfaces: Vector[ast.NamedType], extensions: Vector[ast.TypeExtensionDefinition]) = {
    val extraInts = extensions.flatMap(_.definition.interfaces)

    val allInts = extraInts.foldLeft(interfaces) {
      case (acc, interface) if acc.exists(_.name == interface.name) ⇒
        throw new SchemaMaterializationException(s"Type '${tpe.name}' already implements '${interface.name}'. It cannot also be implemented in this type extension.")
      case (acc, interface) ⇒ acc :+ interface
    }

    allInts map (getInterfaceType(origin, _))
  }

  def extendInterfaces(origin: MatOrigin, tpe: ObjectType[Ctx, _], extensions: Vector[ast.TypeExtensionDefinition]) = {
    val extraInts = extensions.flatMap(_.definition.interfaces)

    val allInts = extraInts.foldLeft(List.empty[ast.NamedType]) {
      case (acc, interface) if tpe.allInterfaces.exists(_.name == interface.name) || acc.exists(_.name == interface.name) ⇒
        throw new SchemaMaterializationException(s"Type '${tpe.name}' already implements '${interface.name}'. It cannot also be implemented in this type extension.")
      case (acc, interface) ⇒ acc :+ interface
    }

    val ei = allInts map (getInterfaceType(origin, _))
    val oi = tpe.interfaces map (getTypeFromDef(origin, _).asInstanceOf[InterfaceType[Ctx, Any]])

    ei ++ oi
  }

  def buildFields(origin: MatOrigin, tpe: TypeDefinition, fieldDefs: Vector[ast.FieldDefinition], extensions: Vector[ast.TypeExtensionDefinition]) = {
    val extraFields = extensions.flatMap(_.definition.fields)

    val withExtensions = extraFields.foldLeft(fieldDefs) {
      case (acc, field) if acc.exists(_.name == field.name) ⇒
        throw new SchemaMaterializationException(s"Field '${tpe.name}.${field.name}' already exists in the schema. It cannot also be defined in this type extension.")
      case (acc, field) ⇒ acc :+ field
    }

    val addFields = builder.buildAdditionalFields(origin, tpe, extensions, this).flatMap {
      case MaterializedFieldAst(o, ast) ⇒ buildField(o, tpe, extensions, ast)
      case MaterializedFieldInst(o, definition) ⇒ Some(extendField(o, None, definition))
    }

    withExtensions.flatMap(buildField(origin, tpe, extensions, _)) ++ addFields
  }

  def extendFields(origin: MatOrigin, tpe: ObjectLikeType[Ctx, _], extensions: Vector[ast.TypeExtensionDefinition]) = {
    val extraFields = extensions.flatMap(e ⇒ e.definition.fields map (e → _))

    val extensionFields = extraFields.foldLeft(List.empty[(ast.TypeExtensionDefinition, ast.FieldDefinition)]) {
      case (acc, field) if tpe.fieldsByName.contains(field._2.name) || acc.exists(_._2.name == field._2.name) ⇒
        throw new SchemaMaterializationException(s"Field '${tpe.name}.${field._2.name}' already exists in the schema. It cannot also be defined in this type extension.")
      case (acc, field) ⇒ acc :+ field
    }

    val ef = extensionFields flatMap (f ⇒ buildField(origin, f._1.definition, extensions, f._2))
    val of = tpe.uniqueFields.toList map (extendField(origin, Some(tpe), _))

    of ++ ef
  }

  def findExtensions(typeName: String) =
    typeExtensionDefs.filter(_.definition.name == typeName)

  def buildUnionDef(origin: MatOrigin, tpe: ast.UnionTypeDefinition) =
    builder.buildUnionType(origin, tpe, tpe.types map (getObjectType(origin, _)) toList, this)

  def extendUnionType(origin: MatOrigin, tpe: UnionType[Ctx]) =
    builder.extendUnionType(origin, tpe, tpe.types map (getTypeFromDef(origin, _)), this)

  def extendScalarAlias(origin: MatOrigin, alias: ScalarAlias[Any, Any]) =
    builder.extendScalarAlias(origin, alias, getTypeFromDef(origin, alias.aliasFor), this)

  def buildInputObjectDef(origin: MatOrigin, tpe: ast.InputObjectTypeDefinition) =
    builder.buildInputObjectType(origin, tpe, () ⇒ tpe.fields flatMap (buildInputField(origin, tpe, _)) toList, this)

  def buildScalarDef(origin: MatOrigin, tpe: ast.ScalarTypeDefinition) =
    builder.buildScalarType(origin, tpe, this)

  private def buildEnumDef(origin: MatOrigin, tpe: ast.EnumTypeDefinition) =
    builder.buildEnumType(origin, tpe, tpe.values flatMap (buildEnumValue(origin, tpe, _)) toList, this)

  private def buildEnumValue(origin: MatOrigin, typeDef: ast.EnumTypeDefinition, value: ast.EnumValueDefinition) =
    builder.buildEnumValue(origin, typeDef, value, this)

  def buildDefault(defaultValue: Option[ast.Value]) =
    defaultValue map (dv ⇒ dv → sangria.marshalling.queryAst.queryAstToInput)

  def buildArgument(origin: MatOrigin, typeDefinition: ast.TypeSystemDefinition, fieldDef: Option[ast.FieldDefinition], value: ast.InputValueDefinition) = {
    val default = buildDefault(value.defaultValue)
    val tpe = builder.buildArgumentType(origin, typeDefinition, fieldDef, value, default, this)

    builder.buildArgument(origin, typeDefinition, fieldDef, value, tpe, default, this)
  }

  def buildInputField(origin: MatOrigin, typeDef: ast.InputObjectTypeDefinition, value: ast.InputValueDefinition) = {
    val default = buildDefault(value.defaultValue)
    val tpe = builder.buildInputFieldType(origin, typeDef, value, default, this)

    builder.buildInputField(origin, typeDef, value, tpe, default, this)
  }

  def buildDirectiveLocation(loc: ast.DirectiveLocation) =
    try {
      DirectiveLocation.fromString(loc.name)
    } catch {
      case e: MatchError ⇒ throw new SchemaMaterializationException(s"Unknown directive location '${loc.name}'.")
    }
}

object AstSchemaMaterializer {
  case class SchemaInfo(query: ast.NamedType, mutation: Option[ast.NamedType], subscription: Option[ast.NamedType], definition: Option[ast.SchemaDefinition])

  def extractSchemaInfo(document: ast.Document, typeDefs: Vector[ast.TypeDefinition]): SchemaInfo = {
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

  def definitions(document: ast.Document): Vector[Named] =
    definitions[Any](document, AstSchemaBuilder.default)

  def definitions[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]): Vector[Named] =
    new AstSchemaMaterializer[Ctx](document, AstSchemaBuilder.default).definitions

  def extendSchema[Ctx, Val](schema: Schema[Ctx, Val], document: ast.Document, builder: AstSchemaBuilder[Ctx] = AstSchemaBuilder.default): Schema[Ctx, Val] =
    new AstSchemaMaterializer[Ctx](document, builder).extend(schema)
}