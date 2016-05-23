package sangria.renderer

import sangria.execution.ValueCoercionHelper
import sangria.introspection._
import sangria.marshalling.{ToInput, ResultMarshaller, InputUnmarshaller}
import sangria.parser.DeliveryScheme.Throw
import sangria.schema._
import sangria.util.StringUtil.escapeString
import sangria.introspection.__DirectiveLocation

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

  val TypeSeparator = "\n\n"
  val Indention = "  "

  private def renderDescription(description: Option[String], prefix: String = "", lineBreak : Boolean = true) = description match {
    case Some(descr) if descr.trim.nonEmpty ⇒
      val lines = descr.lines.map(_.trim).toList

      lines map (l ⇒ prefix + "##" + (if (l.trim.nonEmpty) " " + l else "")) mkString ("", "\n", if (lineBreak) "\n" else "")
    case _ ⇒ ""
  }

  private def renderImplementedInterfaces(tpe: IntrospectionObjectType) =
    if (tpe.interfaces.nonEmpty)
      tpe.interfaces map (_.name) mkString (" implements ", ", ", "")
    else
      ""

  private def renderImplementedInterfaces(tpe: ObjectLikeType[_, _]) =
    if (tpe.allInterfaces.nonEmpty)
      tpe.allInterfaces map (_.name) mkString (" implements ", ", ", "")
    else
      ""

  def renderTypeName(tpe: IntrospectionTypeRef): String =
    tpe match {
      case IntrospectionListTypeRef(ofType) ⇒ s"[${renderTypeName(ofType)}]"
      case IntrospectionNonNullTypeRef(ofType) ⇒ s"${renderTypeName(ofType)}!"
      case IntrospectionNamedTypeRef(_, name) ⇒ name
    }

  private def renderDefault(value: (Any, ToInput[_, _]), tpe: InputType[_]) = {
    val coercionHelper = new ValueCoercionHelper[Any]

    import sangria.marshalling.queryAst.queryAstResultMarshaller

    s" = ${DefaultValueRenderer.renderInputValueCompact(value, tpe, coercionHelper)}"
  }

  private def renderArg(arg: IntrospectionInputValue, defParser: Option[DefaultValueParser[_]]) = {
    val argDef = s"${arg.name}: ${renderTypeName(arg.tpe)}"
    val default =
      for {
        default ← arg.defaultValue
        parser ← defParser
        tpe ← parser.schema.getInputType(arg.tpe)
        parsed ← parser.parser.parse(default).toOption
      } yield renderDefault(parsed → parser.toInput, tpe)

    argDef + (default getOrElse "")
  }

  private def renderArg(arg: Argument[_]) = {
    val argDef = s"${arg.name}: ${renderTypeName(arg.argumentType)}"
    val default = arg.defaultValue.fold("")(renderDefault(_, arg.argumentType))

    argDef + default
  }

  def spaceOpt(show: Boolean) = if (show) " " else ""

  private def renderDeprecation(isDeprecated: Boolean, reason: Option[String], frontSep: Boolean = true) = (isDeprecated, reason) match {
    case (true, Some(r)) if r.trim == DefaultDeprecationReason ⇒ spaceOpt(frontSep) + "@deprecated"
    case (true, Some(r)) if r.trim.nonEmpty ⇒ spaceOpt(frontSep) + "@deprecated(reason: \"" + escapeString(r.trim) + "\")"
    case (true, _) ⇒ spaceOpt(frontSep) + "@deprecated"
    case _ ⇒ ""
  }

  def renderArgs(args: Seq[IntrospectionInputValue], defParser: Option[DefaultValueParser[_]]) =
    if (args.nonEmpty)
      args map (renderArg(_, defParser)) mkString ("(", ", ", ")")
    else
      ""

  private def renderArgs(args: Seq[Argument[_]]) =
    if (args.nonEmpty)
      args map renderArg mkString ("(", ", ", ")")
    else
      ""

  private def renderFields(fields: Seq[IntrospectionField], defParser: Option[DefaultValueParser[_]]) =
    if (fields.nonEmpty)
      fields map (renderField(_, defParser)) mkString "\n"
    else
      ""

  private def renderFields(fields: Seq[Field[_, _]]) =
    if (fields.nonEmpty)
      fields map (renderField(_)) mkString "\n"
    else
      ""

  private def renderInputFieldsI(fields: Seq[IntrospectionInputValue], defParser: Option[DefaultValueParser[_]]) =
    if (fields.nonEmpty)
      fields map (renderInputField(_, defParser)) mkString "\n"
    else
      ""

  private def renderInputFields(fields: Seq[InputField[_]]) =
    if (fields.nonEmpty)
      fields map (renderInputField(_)) mkString "\n"
    else
      ""

  private def renderField(field: IntrospectionField, defParser: Option[DefaultValueParser[_]]) =
    s"${renderDescription(field.description, prefix = Indention)}$Indention${field.name}${renderArgs(field.args, defParser)}: ${renderTypeName(field.tpe)}${renderDeprecation(field.isDeprecated, field.deprecationReason)}"

  private def renderField(field: Field[_, _]) =
    s"${renderDescription(field.description, prefix = Indention)}$Indention${field.name}${renderArgs(field.arguments)}: ${renderTypeName(field.fieldType)}${renderDeprecation(field.deprecationReason.isDefined, field.deprecationReason)}"

  private def renderInputField(field: IntrospectionInputValue, defParser: Option[DefaultValueParser[_]]) = {
    val default =
      for {
        default ← field.defaultValue
        parser ← defParser
        tpe ← parser.schema.getInputType(field.tpe)
        parsed ← parser.parser.parse(default).toOption
      } yield renderDefault(parsed → parser.toInput, tpe)

    s"${renderDescription(field.description, prefix = Indention)}$Indention${field.name}: ${renderTypeName(field.tpe)}${default getOrElse ""}"
  }

  private def renderInputField(field: InputField[_]) = {
    val default = field.defaultValue.fold("")(renderDefault(_, field.fieldType))

    s"${renderDescription(field.description, prefix = Indention)}$Indention${field.name}: ${renderTypeName(field.fieldType)}$default"
  }

  private def renderObject(tpe: IntrospectionObjectType, defParser: Option[DefaultValueParser[_]]) =
    s"${renderDescription(tpe.description)}type ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe.fields, defParser)}\n}"

  private def renderObject(tpe: ObjectType[_, _]) =
    s"${renderDescription(tpe.description)}type ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe.uniqueFields)}\n}"

  private def renderEnum(tpe: IntrospectionEnumType) =
    s"${renderDescription(tpe.description)}enum ${tpe.name} {\n${renderEnumValuesI(tpe.enumValues)}\n}"

  private def renderEnum(tpe: EnumType[_]) =
    s"${renderDescription(tpe.description)}enum ${tpe.name} {\n${renderEnumValues(tpe.values)}\n}"

  private def renderEnumValuesI(values: Seq[IntrospectionEnumValue]) =
    if (values.nonEmpty)
      values map (v ⇒ renderDescription(v.description, prefix = Indention) + Indention + v.name + renderDeprecation(v.isDeprecated, v.deprecationReason)) mkString "\n"
    else
      ""

  private def renderEnumValues(values: Seq[EnumValue[_]]) =
    if (values.nonEmpty)
      values map (v ⇒ renderDescription(v.description, prefix = Indention) + Indention + v.name + renderDeprecation(v.deprecationReason.isDefined, v.deprecationReason)) mkString "\n"
    else
      ""

  private def renderScalar(tpe: IntrospectionScalarType) =
    s"${renderDescription(tpe.description)}scalar ${tpe.name}"

  private def renderScalar(tpe: ScalarType[_]) =
    s"${renderDescription(tpe.description)}scalar ${tpe.name}"

  private def renderInputObject(tpe: IntrospectionInputObjectType, defParser: Option[DefaultValueParser[_]]) =
    s"${renderDescription(tpe.description)}input ${tpe.name} {\n${renderInputFieldsI(tpe.inputFields, defParser)}\n}"

  private def renderInputObject(tpe: InputObjectType[_]) =
    s"${renderDescription(tpe.description)}input ${tpe.name} {\n${renderInputFields(tpe.fields)}\n}"

  private def renderInterface(tpe: IntrospectionInterfaceType, defParser: Option[DefaultValueParser[_]]) =
    s"${renderDescription(tpe.description)}interface ${tpe.name} {\n${renderFields(tpe.fields, defParser)}\n}"

  private def renderInterface(tpe: InterfaceType[_, _]) =
    s"${renderDescription(tpe.description)}interface ${tpe.name} {\n${renderFields(tpe.uniqueFields)}\n}"

  private def renderUnion(tpe: IntrospectionUnionType) =
    if (tpe.possibleTypes.nonEmpty)
      tpe.possibleTypes map (_.name) mkString (s"${renderDescription(tpe.description)}union ${tpe.name} = ", " | ", "")
    else
      ""

  private def renderUnion(tpe: UnionType[_]) =
    if (tpe.types.nonEmpty)
      tpe.types map (_.name) mkString (s"${renderDescription(tpe.description)}union ${tpe.name} = ", " | ", "")
    else
      ""

  private def renderType(tpe: IntrospectionType, defParser: Option[DefaultValueParser[_]]) =
    tpe match {
      case o: IntrospectionObjectType ⇒ renderObject(o, defParser)
      case u: IntrospectionUnionType ⇒ renderUnion(u)
      case i: IntrospectionInterfaceType ⇒ renderInterface(i, defParser)
      case io: IntrospectionInputObjectType ⇒ renderInputObject(io, defParser)
      case s: IntrospectionScalarType ⇒ renderScalar(s)
      case e: IntrospectionEnumType ⇒ renderEnum(e)
      case kind ⇒ throw new IllegalArgumentException(s"Unsupported kind: $kind")
    }

  private def renderType(tpe: Type) =
    tpe match {
      case o: ObjectType[_, _] ⇒ renderObject(o)
      case u: UnionType[_] ⇒ renderUnion(u)
      case i: InterfaceType[_, _] ⇒ renderInterface(i)
      case io: InputObjectType[_] ⇒ renderInputObject(io)
      case s: ScalarType[_] ⇒ renderScalar(s)
      case e: EnumType[_] ⇒ renderEnum(e)
      case _ ⇒ throw new IllegalArgumentException(s"Unsupported type: $tpe")
    }

  private def renderDirectiveLocation(loc: DirectiveLocation.Value) =
    __DirectiveLocation.byValue(loc).name

  private def renderDirective(dir: Directive) =
    s"${renderDescription(dir.description)}directive @${dir.name}${renderArgs(dir.arguments)} on ${dir.locations.toList.map(renderDirectiveLocation).sorted mkString " | "}"

  private def renderDirective(dir: IntrospectionDirective, defParser: Option[DefaultValueParser[_]]) =
    s"${renderDescription(dir.description)}directive @${dir.name}${renderArgs(dir.args, defParser)} on ${dir.locations.toList.map(renderDirectiveLocation).sorted mkString " | "}"

  def renderSchema(introspectionSchema: IntrospectionSchema, defParser: Option[DefaultValueParser[_]]): String = {
    val types = introspectionSchema.types filterNot isBuiltIn sortBy (_.name) map (renderType(_, defParser))
    val directives = introspectionSchema.directives filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) sortBy (_.name) map (renderDirective(_, defParser))

    (types ++ directives) mkString TypeSeparator
  }

  def renderSchema[T: InputUnmarshaller](introspectionResult: T, defParser: Option[DefaultValueParser[_]]): String =
    renderSchema(IntrospectionParser parse introspectionResult, defParser)

  def renderSchema(schema: Schema[_, _]): String = {
    val types = schema.typeList filterNot isBuiltInType sortBy (_.name) map renderType
    val directives = schema.directives filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) sortBy (_.name) map renderDirective

    (types ++ directives) mkString TypeSeparator
  }

  def renderIntrospectionSchema(introspectionSchema: IntrospectionSchema, defParser: Option[DefaultValueParser[_]]): String = {
    val types = introspectionSchema.types filter (tpe ⇒ Schema.isIntrospectionType(tpe.name)) sortBy (_.name) map (renderType(_, defParser))
    val directives = introspectionSchema.directives filter (d ⇒ Schema.isBuiltInDirective(d.name)) sortBy (_.name) map (renderDirective(_, defParser))

    (types ++ directives) mkString TypeSeparator
  }

  def renderIntrospectionSchema[T: InputUnmarshaller](introspectionResult: T, defParser: Option[DefaultValueParser[_]]): String =
    renderIntrospectionSchema(IntrospectionParser parse introspectionResult, defParser)

  private def isBuiltIn(tpe: IntrospectionType) =
    Schema.isBuiltInType(tpe.name)

  private def isBuiltInType(tpe: Type with Named) =
    Schema.isBuiltInType(tpe.name)
}