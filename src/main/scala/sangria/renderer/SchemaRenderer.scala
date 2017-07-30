package sangria.renderer

import sangria.execution.ValueCoercionHelper
import sangria.introspection._
import sangria.marshalling.{InputUnmarshaller, ToInput}
import sangria.schema._
import sangria.ast
import sangria.introspection.__DirectiveLocation
import sangria.parser.QueryParser

object SchemaRenderer {
  def renderTypeName(tpe: Type, topLevel: Boolean = false) = {
    def loop(t: Type, suffix: String): String = t match {
      case OptionType(ofType) ⇒ loop(ofType, "")
      case OptionInputType(ofType) ⇒ loop(ofType, "")
      case ListType(ofType) ⇒ s"[${loop(ofType, "!")}]" + suffix
      case ListInputType(ofType) ⇒ s"[${loop(ofType, "!")}]" + suffix
      case named: Named ⇒ named.name + suffix
    }

    loop(tpe, if (topLevel) "" else "!")
  }

  def renderTypeNameAst(tpe: Type, topLevel: Boolean = false) = {
    def nn(tpe: ast.Type, notNull: Boolean) =
      if (notNull) ast.NotNullType(tpe)
      else tpe

    def loop(t: Type, notNull: Boolean): ast.Type = t match {
      case OptionType(ofType) ⇒ loop(ofType, false)
      case OptionInputType(ofType) ⇒ loop(ofType, false)
      case ListType(ofType) ⇒ nn(ast.ListType(loop(ofType, true)), notNull)
      case ListInputType(ofType) ⇒ nn(ast.ListType(loop(ofType, true)), notNull)
      case named: Named ⇒ nn(ast.NamedType(named.name), notNull)
    }

    loop(tpe, !topLevel)
  }

  private def renderDescription(description: Option[String]) = description match {
    case Some(descr) if descr.trim.nonEmpty ⇒
      descr.split("\n").map(l ⇒ ast.Comment(l)).toVector
    case _ ⇒ Vector.empty
  }

  private def renderImplementedInterfaces(tpe: IntrospectionObjectType) =
    tpe.interfaces.map(t ⇒ ast.NamedType(t.name)).toVector

  private def renderImplementedInterfaces(tpe: ObjectLikeType[_, _]) =
    tpe.allInterfaces.map(t ⇒ ast.NamedType(t.name))

  def renderTypeName(tpe: IntrospectionTypeRef): ast.Type =
    tpe match {
      case IntrospectionListTypeRef(ofType) ⇒ ast.ListType(renderTypeName(ofType))
      case IntrospectionNonNullTypeRef(ofType) ⇒ ast.NotNullType(renderTypeName(ofType))
      case IntrospectionNamedTypeRef(_, name) ⇒ ast.NamedType(name)
    }

  private def renderDefault(defaultValue: Option[String]) =
    defaultValue.flatMap(d ⇒ QueryParser.parseInput(d).toOption)

  private def renderDefault(value: (Any, ToInput[_, _]), tpe: InputType[_]) = {
    val coercionHelper = new ValueCoercionHelper[Any]

    DefaultValueRenderer.renderInputValue(value, tpe, coercionHelper)
  }

  private def renderArg(arg: IntrospectionInputValue) =
    ast.InputValueDefinition(arg.name, renderTypeName(arg.tpe), renderDefault(arg.defaultValue), comments = renderDescription(arg.description))

  private def renderArg(arg: Argument[_]) =
    ast.InputValueDefinition(arg.name, renderTypeNameAst(arg.argumentType), arg.defaultValue.flatMap(renderDefault(_, arg.argumentType)), arg.astDirectives, renderDescription(arg.description))

  private def withoutDeprecated(dirs: Vector[ast.Directive]) = dirs.filterNot(_.name == "deprecated")

  private def renderDeprecation(isDeprecated: Boolean, reason: Option[String]) = (isDeprecated, reason) match {
    case (true, Some(r)) if r.trim == DefaultDeprecationReason ⇒ Vector(ast.Directive("deprecated", Vector.empty))
    case (true, Some(r)) if r.trim.nonEmpty ⇒ Vector(ast.Directive("deprecated", Vector(ast.Argument("reason", ast.StringValue(r.trim)))))
    case (true, _) ⇒ Vector(ast.Directive("deprecated", Vector.empty))
    case _ ⇒ Vector.empty
  }

  def renderArgsI(args: Seq[IntrospectionInputValue]) =
    args.map(renderArg).toVector

  private def renderArgs(args: Seq[Argument[_]]) =
    args.map(renderArg).toVector

  private def renderFieldsI(fields: Seq[IntrospectionField]) =
    fields.map(renderField).toVector

  private def renderFields(fields: Seq[Field[_, _]]) =
    fields.map(renderField).toVector

  private def renderInputFieldsI(fields: Seq[IntrospectionInputValue]) =
    fields.map(renderInputField).toVector

  private def renderInputFields(fields: Seq[InputField[_]]) =
    fields.map(renderInputField).toVector

  private def renderField(field: IntrospectionField) =
    ast.FieldDefinition(field.name, renderTypeName(field.tpe), renderArgsI(field.args), renderDeprecation(field.isDeprecated, field.deprecationReason), renderDescription(field.description))

  private def renderField(field: Field[_, _]) =
    ast.FieldDefinition(field.name, renderTypeNameAst(field.fieldType), renderArgs(field.arguments), withoutDeprecated(field.astDirectives) ++ renderDeprecation(field.deprecationReason.isDefined, field.deprecationReason), renderDescription(field.description))

  private def renderInputField(field: IntrospectionInputValue) =
    ast.InputValueDefinition(field.name, renderTypeName(field.tpe), renderDefault(field.defaultValue), comments = renderDescription(field.description))

  private def renderInputField(field: InputField[_]) =
    ast.InputValueDefinition(field.name, renderTypeNameAst(field.fieldType), field.defaultValue.flatMap(renderDefault(_, field.fieldType)), field.astDirectives, renderDescription(field.description))

  private def renderObject(tpe: IntrospectionObjectType) =
    ast.ObjectTypeDefinition(tpe.name, renderImplementedInterfaces(tpe), renderFieldsI(tpe.fields), comments = renderDescription(tpe.description))

  private def renderObject(tpe: ObjectType[_, _]) =
    ast.ObjectTypeDefinition(tpe.name, renderImplementedInterfaces(tpe), renderFields(tpe.uniqueFields), tpe.astDirectives, renderDescription(tpe.description))

  private def renderEnum(tpe: IntrospectionEnumType) =
    ast.EnumTypeDefinition(tpe.name, renderEnumValuesI(tpe.enumValues), comments = renderDescription(tpe.description))

  private def renderEnum(tpe: EnumType[_]) =
    ast.EnumTypeDefinition(tpe.name, renderEnumValues(tpe.values), tpe.astDirectives, renderDescription(tpe.description))

  private def renderEnumValuesI(values: Seq[IntrospectionEnumValue]) =
    values.map(v ⇒ ast.EnumValueDefinition(v.name, renderDeprecation(v.isDeprecated, v.deprecationReason), renderDescription(v.description))).toVector

  private def renderEnumValues(values: Seq[EnumValue[_]]) =
    values.map(v ⇒ ast.EnumValueDefinition(v.name, withoutDeprecated(v.astDirectives) ++ renderDeprecation(v.deprecationReason.isDefined, v.deprecationReason), renderDescription(v.description))).toVector

  private def renderScalar(tpe: IntrospectionScalarType) =
    ast.ScalarTypeDefinition(tpe.name, comments = renderDescription(tpe.description))

  private def renderScalar(tpe: ScalarType[_]) =
    ast.ScalarTypeDefinition(tpe.name, tpe.astDirectives, renderDescription(tpe.description))

  private def renderInputObject(tpe: IntrospectionInputObjectType) =
    ast.InputObjectTypeDefinition(tpe.name, renderInputFieldsI(tpe.inputFields), comments = renderDescription(tpe.description))

  private def renderInputObject(tpe: InputObjectType[_]) =
    ast.InputObjectTypeDefinition(tpe.name, renderInputFields(tpe.fields), tpe.astDirectives, renderDescription(tpe.description))

  private def renderInterface(tpe: IntrospectionInterfaceType) =
    ast.InterfaceTypeDefinition(tpe.name, renderFieldsI(tpe.fields), comments = renderDescription(tpe.description))

  private def renderInterface(tpe: InterfaceType[_, _]) =
    ast.InterfaceTypeDefinition(tpe.name, renderFields(tpe.uniqueFields), tpe.astDirectives, renderDescription(tpe.description))

  private def renderUnion(tpe: IntrospectionUnionType) =
    ast.UnionTypeDefinition(tpe.name, tpe.possibleTypes.map(t ⇒ ast.NamedType(t.name)).toVector, comments = renderDescription(tpe.description))

  private def renderUnion(tpe: UnionType[_]) =
    ast.UnionTypeDefinition(tpe.name, tpe.types.map(t ⇒ ast.NamedType(t.name)).toVector, tpe.astDirectives, renderDescription(tpe.description))

  private def renderSchemaDefinition(schema: IntrospectionSchema): Option[ast.SchemaDefinition] =
    if (isSchemaOfCommonNames(schema.queryType.name, schema.mutationType.map(_.name), schema.subscriptionType.map(_.name)))
      None
    else {
      val withQuery = Vector(ast.OperationTypeDefinition(ast.OperationType.Query, ast.NamedType(schema.queryType.name)))
      val withMutation = schema.mutationType.fold(withQuery)(t ⇒ withQuery :+ ast.OperationTypeDefinition(ast.OperationType.Mutation, ast.NamedType(t.name)))
      val withSubs = schema.subscriptionType.fold(withMutation)(t ⇒ withMutation :+ ast.OperationTypeDefinition(ast.OperationType.Subscription, ast.NamedType(t.name)))

      Some(ast.SchemaDefinition(withSubs))
    }

  private def renderSchemaDefinition(schema: Schema[_, _]): Option[ast.SchemaDefinition] =
    if (isSchemaOfCommonNames(schema.query.name, schema.mutation.map(_.name), schema.subscription.map(_.name)))
      None
    else {
      val withQuery = Vector(ast.OperationTypeDefinition(ast.OperationType.Query, ast.NamedType(schema.query.name)))
      val withMutation = schema.mutation.fold(withQuery)(t ⇒ withQuery :+ ast.OperationTypeDefinition(ast.OperationType.Mutation, ast.NamedType(t.name)))
      val withSubs = schema.subscription.fold(withMutation)(t ⇒ withMutation :+ ast.OperationTypeDefinition(ast.OperationType.Subscription, ast.NamedType(t.name)))

      Some(ast.SchemaDefinition(withSubs, schema.astDirectives))
    }

  private def isSchemaOfCommonNames(query: String, mutation: Option[String], subscription: Option[String]) =
    query == "Query" && mutation.fold(true)(_ == "Mutation") && subscription.fold(true)(_ == "Subscription")

  private def renderType(tpe: IntrospectionType): ast.TypeDefinition =
    tpe match {
      case o: IntrospectionObjectType ⇒ renderObject(o)
      case u: IntrospectionUnionType ⇒ renderUnion(u)
      case i: IntrospectionInterfaceType ⇒ renderInterface(i)
      case io: IntrospectionInputObjectType ⇒ renderInputObject(io)
      case s: IntrospectionScalarType ⇒ renderScalar(s)
      case e: IntrospectionEnumType ⇒ renderEnum(e)
      case kind ⇒ throw new IllegalArgumentException(s"Unsupported kind: $kind")
    }

  private def renderType(tpe: Type): ast.TypeDefinition =
    tpe match {
      case o: ObjectType[_, _] ⇒ renderObject(o)
      case u: UnionType[_] ⇒ renderUnion(u)
      case i: InterfaceType[_, _] ⇒ renderInterface(i)
      case io: InputObjectType[_] ⇒ renderInputObject(io)
      case s: ScalarType[_] ⇒ renderScalar(s)
      case s: ScalarAlias[_, _] ⇒ renderScalar(s.aliasFor)
      case e: EnumType[_] ⇒ renderEnum(e)
      case _ ⇒ throw new IllegalArgumentException(s"Unsupported type: $tpe")
    }

  private def renderDirectiveLocation(loc: DirectiveLocation.Value) =
    ast.DirectiveLocation(__DirectiveLocation.byValue(loc).name)

  private def renderDirective(dir: Directive) =
    ast.DirectiveDefinition(dir.name, renderArgs(dir.arguments), dir.locations.toVector.map(renderDirectiveLocation).sortBy(_.name), renderDescription(dir.description))

  private def renderDirective(dir: IntrospectionDirective) =
    ast.DirectiveDefinition(dir.name, renderArgsI(dir.args), dir.locations.toVector.map(renderDirectiveLocation).sortBy(_.name), renderDescription(dir.description))

  def schemaAstFromIntrospection(introspectionSchema: IntrospectionSchema, filter: SchemaFilter = SchemaFilter.withoutSangriaBuiltIn): ast.Document = {
    val schemaDef = renderSchemaDefinition(introspectionSchema)
    val types = introspectionSchema.types filter (t ⇒ filter.filterTypes(t.name)) sortBy (_.name) map renderType
    val directives = introspectionSchema.directives filter (d ⇒ filter.filterDirectives(d.name)) sortBy (_.name) map renderDirective

    ast.Document(schemaDef.toVector ++ types ++ directives)
  }

  def renderSchema(introspectionSchema: IntrospectionSchema): String =
    schemaAstFromIntrospection(introspectionSchema, SchemaFilter.withoutSangriaBuiltIn).renderPretty

  def renderSchema[T: InputUnmarshaller](introspectionResult: T): String = {
    import sangria.parser.DeliveryScheme.Throw

    schemaAstFromIntrospection(IntrospectionParser parse introspectionResult, SchemaFilter.withoutSangriaBuiltIn).renderPretty
  }

  def renderSchema(introspectionSchema: IntrospectionSchema, filter: SchemaFilter): String =
    schemaAstFromIntrospection(introspectionSchema, filter).renderPretty

  def renderSchema[T: InputUnmarshaller](introspectionResult: T, filter: SchemaFilter): String = {
    import sangria.parser.DeliveryScheme.Throw

    schemaAstFromIntrospection(IntrospectionParser parse introspectionResult, filter).renderPretty
  }

  def schemaAst(schema: Schema[_, _], filter: SchemaFilter = SchemaFilter.withoutSangriaBuiltIn): ast.Document = {
    val schemaDef = renderSchemaDefinition(schema)
    val types = schema.typeList filter (t ⇒ filter.filterTypes(t.name)) sortBy (_.name) map renderType
    val directives = schema.directives filter (d ⇒ filter.filterDirectives(d.name)) sortBy (_.name) map renderDirective

    ast.Document(schemaDef.toVector ++ types ++ directives)
  }

  def renderSchema(schema: Schema[_, _]): String =
    schemaAst(schema, SchemaFilter.withoutSangriaBuiltIn).renderPretty

  def renderSchema(schema: Schema[_, _], filter: SchemaFilter): String =
    schemaAst(schema, filter).renderPretty

  private def introspectionSchemaAst(introspectionSchema: IntrospectionSchema): ast.Document = {
    val types = introspectionSchema.types filter (tpe ⇒ Schema.isIntrospectionType(tpe.name)) sortBy (_.name) map (renderType(_))
    val directives = introspectionSchema.directives filter (d ⇒ Schema.isBuiltInDirective(d.name)) sortBy (_.name) map (renderDirective(_))

    ast.Document(types.toVector ++ directives)
  }

  @deprecated("use `renderSchema` with `SchemaFilter.introspection`", "1.2.1")
  def renderIntrospectionSchema(introspectionSchema: IntrospectionSchema): String =
    introspectionSchemaAst(introspectionSchema).renderPretty

  @deprecated("use `renderSchema` with `SchemaFilter.introspection`", "1.2.1")
  def renderIntrospectionSchema[T: InputUnmarshaller](introspectionResult: T): String = {
    import sangria.parser.DeliveryScheme.Throw
    
    renderIntrospectionSchema(IntrospectionParser parse introspectionResult)
  }
}

case class SchemaFilter(filterTypes: String ⇒ Boolean, filterDirectives: String ⇒ Boolean)

object SchemaFilter {
  val withoutSangriaBuiltIn = SchemaFilter(
    typeName ⇒ !Schema.isBuiltInType(typeName),
    dirName ⇒ !Schema.isBuiltInDirective(dirName))

  val withoutGraphQLBuiltIn = SchemaFilter(
    typeName ⇒ !Schema.isBuiltInGraphQLType(typeName),
    dirName ⇒ !Schema.isBuiltInDirective(dirName))

  val withoutIntrospection = SchemaFilter(
    typeName ⇒ !Schema.isIntrospectionType(typeName),
    Function.const(true))

  val builtIn = SchemaFilter(
    typeName ⇒ Schema.isBuiltInType(typeName),
    dirName ⇒ Schema.isBuiltInDirective(dirName))

  val introspection = SchemaFilter(
    typeName ⇒ Schema.isIntrospectionType(typeName),
    Function.const(false))

  val all = SchemaFilter(Function.const(true), Function.const(true))
}