package sangria.renderer

import sangria.ast
import sangria.execution.ValueCoercionHelper
import sangria.introspection._
import sangria.marshalling.{InputUnmarshaller, ToInput}
import sangria.parser.QueryParser
import sangria.schema._

object SchemaRenderer {
  def renderTypeName(tpe: Type, topLevel: Boolean = false) = {
    def loop(t: Type, suffix: String): String = t match {
      case OptionType(ofType) => loop(ofType, "")
      case OptionInputType(ofType) => loop(ofType, "")
      case ListType(ofType) => s"[${loop(ofType, "!")}]" + suffix
      case ListInputType(ofType) => s"[${loop(ofType, "!")}]" + suffix
      case named: Named => named.name + suffix
    }

    loop(tpe, if (topLevel) "" else "!")
  }

  def renderTypeNameAst(tpe: Type, topLevel: Boolean = false): ast.Type = {
    def nn(tpe: ast.Type, notNull: Boolean) =
      if (notNull) ast.NotNullType(tpe)
      else tpe

    def loop(t: Type, notNull: Boolean): ast.Type = t match {
      case OptionType(ofType) => loop(ofType, false)
      case OptionInputType(ofType) => loop(ofType, false)
      case ListType(ofType) => nn(ast.ListType(loop(ofType, true)), notNull)
      case ListInputType(ofType) => nn(ast.ListType(loop(ofType, true)), notNull)
      case named: Named => nn(ast.NamedType(named.name), notNull)
    }

    loop(tpe, !topLevel)
  }

  def renderDescription(description: Option[String]): Option[ast.StringValue] =
    description.flatMap { d =>
      if (d.trim.nonEmpty) Some(ast.StringValue(d, block = d.indexOf('\n') > 0))
      else None
    }

  def renderImplementedInterfaces(tpe: IntrospectionObjectType) =
    tpe.interfaces.map(t => ast.NamedType(t.name)).toVector

  def renderImplementedInterfaces(tpe: ObjectLikeType[_, _]) =
    tpe.allInterfaces.map(t => ast.NamedType(t.name))

  def renderTypeName(tpe: IntrospectionTypeRef): ast.Type =
    tpe match {
      case IntrospectionListTypeRef(ofType) => ast.ListType(renderTypeName(ofType))
      case IntrospectionNonNullTypeRef(ofType) => ast.NotNullType(renderTypeName(ofType))
      case IntrospectionNamedTypeRef(_, name) => ast.NamedType(name)
    }

  def renderDefault(defaultValue: Option[String]) =
    defaultValue.flatMap(d => QueryParser.parseInput(d).toOption)

  def renderDefault(value: (Any, ToInput[_, _]), tpe: InputType[_]) = {
    val coercionHelper = new ValueCoercionHelper[Any]

    DefaultValueRenderer.renderInputValue(value, tpe, coercionHelper)
  }

  def renderArg(arg: IntrospectionInputValue) =
    ast.InputValueDefinition(
      arg.name,
      renderTypeName(arg.tpe),
      renderDefault(arg.defaultValue),
      description = renderDescription(arg.description))

  def renderArg(arg: Argument[_]) =
    ast.InputValueDefinition(
      arg.name,
      renderTypeNameAst(arg.argumentType),
      arg.defaultValue.flatMap(renderDefault(_, arg.argumentType)),
      arg.astDirectives,
      renderDescription(arg.description)
    )

  def withoutDeprecated(dirs: Vector[ast.Directive]) = dirs.filterNot(_.name == "deprecated")

  def renderDeprecation(isDeprecated: Boolean, reason: Option[String]) =
    (isDeprecated, reason) match {
      case (true, Some(r)) if r.trim == DefaultDeprecationReason =>
        Vector(ast.Directive("deprecated", Vector.empty))
      case (true, Some(r)) if r.trim.nonEmpty =>
        Vector(ast.Directive("deprecated", Vector(ast.Argument("reason", ast.StringValue(r.trim)))))
      case (true, _) => Vector(ast.Directive("deprecated", Vector.empty))
      case _ => Vector.empty
    }

  def renderArgsI(args: Seq[IntrospectionInputValue]) =
    args.map(renderArg).toVector

  def renderArgs(args: Seq[Argument[_]]) =
    args.map(renderArg).toVector

  def renderFieldsI(fields: Seq[IntrospectionField]) =
    fields.map(renderField).toVector

  def renderFields(fields: Seq[Field[_, _]]) =
    fields.map(renderField).toVector

  def renderInputFieldsI(fields: Seq[IntrospectionInputValue]) =
    fields.map(renderInputField).toVector

  def renderInputFields(fields: Seq[InputField[_]]) =
    fields.map(renderInputField).toVector

  def renderField(field: IntrospectionField) =
    ast.FieldDefinition(
      field.name,
      renderTypeName(field.tpe),
      renderArgsI(field.args),
      renderDeprecation(field.isDeprecated, field.deprecationReason),
      renderDescription(field.description)
    )

  def renderField(field: Field[_, _]) =
    ast.FieldDefinition(
      field.name,
      renderTypeNameAst(field.fieldType),
      renderArgs(field.arguments),
      withoutDeprecated(field.astDirectives) ++ renderDeprecation(
        field.deprecationReason.isDefined,
        field.deprecationReason),
      renderDescription(field.description)
    )

  def renderInputField(field: IntrospectionInputValue) =
    ast.InputValueDefinition(
      field.name,
      renderTypeName(field.tpe),
      renderDefault(field.defaultValue),
      description = renderDescription(field.description))

  def renderInputField(field: InputField[_]) =
    ast.InputValueDefinition(
      field.name,
      renderTypeNameAst(field.fieldType),
      field.defaultValue.flatMap(renderDefault(_, field.fieldType)),
      field.astDirectives,
      renderDescription(field.description)
    )

  def renderObject(tpe: IntrospectionObjectType) =
    ast.ObjectTypeDefinition(
      tpe.name,
      renderImplementedInterfaces(tpe),
      renderFieldsI(tpe.fields),
      description = renderDescription(tpe.description))

  def renderObject(tpe: ObjectType[_, _]) =
    ast.ObjectTypeDefinition(
      tpe.name,
      renderImplementedInterfaces(tpe),
      renderFields(tpe.uniqueFields),
      tpe.astDirectives,
      renderDescription(tpe.description))

  def renderEnum(tpe: IntrospectionEnumType) =
    ast.EnumTypeDefinition(
      tpe.name,
      renderEnumValuesI(tpe.enumValues),
      description = renderDescription(tpe.description))

  def renderEnum(tpe: EnumType[_]) =
    ast.EnumTypeDefinition(
      tpe.name,
      renderEnumValues(tpe.values),
      tpe.astDirectives,
      renderDescription(tpe.description))

  def renderEnumValuesI(values: Seq[IntrospectionEnumValue]) =
    values
      .map(v =>
        ast.EnumValueDefinition(
          v.name,
          renderDeprecation(v.isDeprecated, v.deprecationReason),
          renderDescription(v.description)))
      .toVector

  def renderEnumValues(values: Seq[EnumValue[_]]) =
    values.map(renderEnumValue).toVector

  def renderEnumValue(v: EnumValue[_]) =
    ast.EnumValueDefinition(
      v.name,
      withoutDeprecated(v.astDirectives) ++ renderDeprecation(
        v.deprecationReason.isDefined,
        v.deprecationReason),
      renderDescription(v.description))

  def renderScalar(tpe: IntrospectionScalarType) =
    ast.ScalarTypeDefinition(tpe.name, description = renderDescription(tpe.description))

  def renderScalar(tpe: ScalarType[_]) =
    ast.ScalarTypeDefinition(tpe.name, tpe.astDirectives, renderDescription(tpe.description))

  def renderInputObject(tpe: IntrospectionInputObjectType) =
    ast.InputObjectTypeDefinition(
      tpe.name,
      renderInputFieldsI(tpe.inputFields),
      description = renderDescription(tpe.description))

  def renderInputObject(tpe: InputObjectType[_]) =
    ast.InputObjectTypeDefinition(
      tpe.name,
      renderInputFields(tpe.fields),
      tpe.astDirectives,
      renderDescription(tpe.description))

  def renderInterface(tpe: IntrospectionInterfaceType) =
    ast.InterfaceTypeDefinition(
      tpe.name,
      renderFieldsI(tpe.fields),
      description = renderDescription(tpe.description))

  def renderInterface(tpe: InterfaceType[_, _]) =
    ast.InterfaceTypeDefinition(
      tpe.name,
      renderFields(tpe.uniqueFields),
      tpe.astDirectives,
      renderDescription(tpe.description))

  def renderUnion(tpe: IntrospectionUnionType) =
    ast.UnionTypeDefinition(
      tpe.name,
      tpe.possibleTypes.map(t => ast.NamedType(t.name)).toVector,
      description = renderDescription(tpe.description))

  def renderUnion(tpe: UnionType[_]) =
    ast.UnionTypeDefinition(
      tpe.name,
      tpe.types.map(t => ast.NamedType(t.name)).toVector,
      tpe.astDirectives,
      renderDescription(tpe.description))

  private def renderSchemaDefinition(schema: IntrospectionSchema): Option[ast.SchemaDefinition] =
    if (isSchemaOfCommonNames(
        schema.queryType.name,
        schema.mutationType.map(_.name),
        schema.subscriptionType.map(_.name)))
      None
    else {
      val withQuery = Vector(
        ast.OperationTypeDefinition(ast.OperationType.Query, ast.NamedType(schema.queryType.name)))
      val withMutation = schema.mutationType.fold(withQuery)(t =>
        withQuery :+ ast.OperationTypeDefinition(ast.OperationType.Mutation, ast.NamedType(t.name)))
      val withSubs = schema.subscriptionType.fold(withMutation)(t =>
        withMutation :+ ast.OperationTypeDefinition(
          ast.OperationType.Subscription,
          ast.NamedType(t.name)))

      Some(ast.SchemaDefinition(withSubs, description = renderDescription(schema.description)))
    }

  private def renderSchemaDefinition(schema: Schema[_, _]): Option[ast.SchemaDefinition] =
    if (isSchemaOfCommonNames(
        schema.query.name,
        schema.mutation.map(_.name),
        schema.subscription.map(
          _.name)) && schema.description.isEmpty && schema.astDirectives.isEmpty)
      None
    else {
      val withQuery = Vector(
        ast.OperationTypeDefinition(ast.OperationType.Query, ast.NamedType(schema.query.name)))
      val withMutation = schema.mutation.fold(withQuery)(t =>
        withQuery :+ ast.OperationTypeDefinition(ast.OperationType.Mutation, ast.NamedType(t.name)))
      val withSubs = schema.subscription.fold(withMutation)(t =>
        withMutation :+ ast.OperationTypeDefinition(
          ast.OperationType.Subscription,
          ast.NamedType(t.name)))

      Some(
        ast.SchemaDefinition(withSubs, schema.astDirectives, renderDescription(schema.description)))
    }

  private def isSchemaOfCommonNames(
      query: String,
      mutation: Option[String],
      subscription: Option[String]) =
    query == "Query" && mutation.fold(true)(_ == "Mutation") && subscription.fold(true)(
      _ == "Subscription")

  def renderType(tpe: IntrospectionType): ast.TypeDefinition =
    tpe match {
      case o: IntrospectionObjectType => renderObject(o)
      case u: IntrospectionUnionType => renderUnion(u)
      case i: IntrospectionInterfaceType => renderInterface(i)
      case io: IntrospectionInputObjectType => renderInputObject(io)
      case s: IntrospectionScalarType => renderScalar(s)
      case e: IntrospectionEnumType => renderEnum(e)
      case kind => throw new IllegalArgumentException(s"Unsupported kind: $kind")
    }

  def renderType(tpe: Type with Named): ast.TypeDefinition =
    tpe match {
      case o: ObjectType[_, _] => renderObject(o)
      case u: UnionType[_] => renderUnion(u)
      case i: InterfaceType[_, _] => renderInterface(i)
      case io: InputObjectType[_] => renderInputObject(io)
      case s: ScalarType[_] => renderScalar(s)
      case s: ScalarAlias[_, _] => renderScalar(s.aliasFor)
      case e: EnumType[_] => renderEnum(e)
      case _ => throw new IllegalArgumentException(s"Unsupported type: $tpe")
    }

  def renderDirectiveLocation(loc: DirectiveLocation.Value) =
    ast.DirectiveLocation(__DirectiveLocation.byValue(loc).name)

  def renderDirective(dir: Directive) =
    ast.DirectiveDefinition(
      dir.name,
      renderArgs(dir.arguments),
      dir.locations.toVector.map(renderDirectiveLocation).sortBy(_.name),
      renderDescription(dir.description))

  def renderDirective(dir: IntrospectionDirective) =
    ast.DirectiveDefinition(
      dir.name,
      renderArgsI(dir.args),
      dir.locations.toVector.map(renderDirectiveLocation).sortBy(_.name),
      renderDescription(dir.description))

  def schemaAstFromIntrospection(
      introspectionSchema: IntrospectionSchema,
      filter: SchemaFilter = SchemaFilter.default): ast.Document = {
    val schemaDef = if (filter.renderSchema) renderSchemaDefinition(introspectionSchema) else None
    val types = introspectionSchema.types
      .filter(t => filter.filterTypes(t.name))
      .sortBy(_.name)
      .map(renderType)
    val directives = introspectionSchema.directives
      .filter(d => filter.filterDirectives(d.name))
      .sortBy(_.name)
      .map(renderDirective)

    ast.Document(schemaDef.toVector ++ types ++ directives)
  }

  def renderSchema(introspectionSchema: IntrospectionSchema): String =
    QueryRenderer.renderPretty(
      schemaAstFromIntrospection(introspectionSchema, SchemaFilter.default))

  def renderSchema[T: InputUnmarshaller](introspectionResult: T): String =
    QueryRenderer.renderPretty(
      schemaAstFromIntrospection(
        IntrospectionParser.parse(introspectionResult).get,
        SchemaFilter.default))

  def renderSchema(introspectionSchema: IntrospectionSchema, filter: SchemaFilter): String =
    QueryRenderer.renderPretty(schemaAstFromIntrospection(introspectionSchema, filter))

  def renderSchema[T: InputUnmarshaller](introspectionResult: T, filter: SchemaFilter): String =
    QueryRenderer.renderPretty(
      schemaAstFromIntrospection(IntrospectionParser.parse(introspectionResult).get, filter))

  def schemaAst(schema: Schema[_, _], filter: SchemaFilter = SchemaFilter.default): ast.Document = {
    val schemaDef = if (filter.renderSchema) renderSchemaDefinition(schema) else None
    val types =
      schema.typeList.filter(t => filter.filterTypes(t.name)).sortBy(_.name).map(renderType)
    val directives = schema.directives
      .filter(d => filter.filterDirectives(d.name))
      .sortBy(_.name)
      .map(renderDirective)

    ast.Document(schemaDef.toVector ++ types ++ directives)
  }

  def renderSchema(schema: Schema[_, _]): String =
    QueryRenderer.renderPretty(schemaAst(schema, SchemaFilter.default))

  def renderSchema(schema: Schema[_, _], filter: SchemaFilter): String =
    QueryRenderer.renderPretty(schemaAst(schema, filter))
}

case class SchemaFilter(
    filterTypes: String => Boolean,
    filterDirectives: String => Boolean,
    renderSchema: Boolean = true)

object SchemaFilter {
  val withoutSangriaBuiltIn: SchemaFilter = SchemaFilter(
    typeName => !Schema.isBuiltInType(typeName),
    dirName => !Schema.isBuiltInDirective(dirName))

  val default: SchemaFilter = withoutSangriaBuiltIn

  val withoutGraphQLBuiltIn = SchemaFilter(
    typeName => !Schema.isBuiltInGraphQLType(typeName),
    dirName => !Schema.isBuiltInDirective(dirName))

  val withoutIntrospection: SchemaFilter =
    SchemaFilter(typeName => !Schema.isIntrospectionType(typeName), Function.const(true))

  val builtIn: SchemaFilter = SchemaFilter(
    typeName => Schema.isBuiltInType(typeName),
    dirName => Schema.isBuiltInDirective(dirName))

  val introspection: SchemaFilter = SchemaFilter(
    typeName => Schema.isIntrospectionType(typeName),
    Function.const(false),
    renderSchema = false)

  val all: SchemaFilter = SchemaFilter(Function.const(true), Function.const(true))
}
