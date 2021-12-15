package sangria.renderer

import sangria.ast
import sangria.introspection.{
  IntrospectionDirective,
  IntrospectionEnumType,
  IntrospectionField,
  IntrospectionInputObjectType,
  IntrospectionInputValue,
  IntrospectionInterfaceType,
  IntrospectionObjectType,
  IntrospectionParser,
  IntrospectionScalarType,
  IntrospectionSchema,
  IntrospectionType,
  IntrospectionUnionType
}
import sangria.marshalling.InputUnmarshaller
import sangria.parser.QueryParser
import sangria.renderer.SchemaRenderer.{
  isSchemaOfCommonNames,
  renderDeprecation,
  renderDescription,
  renderDirectiveLocation,
  renderEnum,
  renderImplementedInterfaces,
  renderScalar,
  renderTypeName,
  renderUnion
}

/** Schema rendering methods used by tests. */
private[renderer] object SchemaRendererExtras {
  private[this] def renderDefault(defaultValue: Option[String]) =
    defaultValue.flatMap(d => QueryParser.parseInput(d).toOption)

  private[this] def renderArg(arg: IntrospectionInputValue) =
    ast.InputValueDefinition(
      arg.name,
      renderTypeName(arg.tpe),
      renderDefault(arg.defaultValue),
      description = renderDescription(arg.description))

  private[this] def renderArgsI(args: Seq[IntrospectionInputValue]) =
    args.map(renderArg).toVector

  private[this] def renderFieldsI(fields: Seq[IntrospectionField]) =
    fields.map(renderField).toVector

  private[this] def renderInputFieldsI(fields: Seq[IntrospectionInputValue]) =
    fields.map(renderInputField).toVector

  private[this] def renderField(field: IntrospectionField) =
    ast.FieldDefinition(
      field.name,
      renderTypeName(field.tpe),
      renderArgsI(field.args),
      renderDeprecation(field.isDeprecated, field.deprecationReason),
      renderDescription(field.description)
    )

  private[this] def renderInputField(field: IntrospectionInputValue) =
    ast.InputValueDefinition(
      field.name,
      renderTypeName(field.tpe),
      renderDefault(field.defaultValue),
      description = renderDescription(field.description))

  private[this] def renderObject(tpe: IntrospectionObjectType) =
    ast.ObjectTypeDefinition(
      tpe.name,
      renderImplementedInterfaces(tpe),
      renderFieldsI(tpe.fields),
      description = renderDescription(tpe.description))

  private[this] def renderInputObject(tpe: IntrospectionInputObjectType) =
    ast.InputObjectTypeDefinition(
      tpe.name,
      renderInputFieldsI(tpe.inputFields),
      description = renderDescription(tpe.description))

  private[this] def renderInterface(tpe: IntrospectionInterfaceType) =
    ast.InterfaceTypeDefinition(
      tpe.name,
      renderFieldsI(tpe.fields),
      description = renderDescription(tpe.description))

  private[this] def renderSchemaDefinition(
      schema: IntrospectionSchema): Option[ast.SchemaDefinition] =
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

  private[this] def renderType(tpe: IntrospectionType): ast.TypeDefinition =
    tpe match {
      case o: IntrospectionObjectType => renderObject(o)
      case u: IntrospectionUnionType => renderUnion(u)
      case i: IntrospectionInterfaceType => renderInterface(i)
      case io: IntrospectionInputObjectType => renderInputObject(io)
      case s: IntrospectionScalarType => renderScalar(s)
      case e: IntrospectionEnumType => renderEnum(e)
      case kind => throw new IllegalArgumentException(s"Unsupported kind: $kind")
    }

  private[this] def renderDirective(dir: IntrospectionDirective) =
    ast.DirectiveDefinition(
      dir.name,
      renderArgsI(dir.args),
      dir.locations.toVector.map(renderDirectiveLocation).sortBy(_.name),
      renderDescription(dir.description))

  private[this] def schemaAstFromIntrospection(
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

  // TODO This appears to not be used.
  private[this] def renderSchema(introspectionSchema: IntrospectionSchema): String =
    QueryRenderer.renderPretty(
      schemaAstFromIntrospection(introspectionSchema, SchemaFilter.default))

  def renderSchema[T: InputUnmarshaller](introspectionResult: T): String =
    QueryRenderer.renderPretty(
      schemaAstFromIntrospection(
        IntrospectionParser.parse(introspectionResult).get,
        SchemaFilter.default))

  // TODO This appears to not be used.
  private[this] def renderSchema(
      introspectionSchema: IntrospectionSchema,
      filter: SchemaFilter): String =
    QueryRenderer.renderPretty(schemaAstFromIntrospection(introspectionSchema, filter))

  def renderSchema[T: InputUnmarshaller](introspectionResult: T, filter: SchemaFilter): String =
    QueryRenderer.renderPretty(
      schemaAstFromIntrospection(IntrospectionParser.parse(introspectionResult).get, filter))
}
