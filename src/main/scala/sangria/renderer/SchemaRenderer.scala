package sangria.renderer

import sangria.execution.ValueCoercionHelper
import sangria.introspection._
import sangria.marshalling.{ToInput, InputUnmarshaller}
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

      lines map (l ⇒ prefix + "#" + (if (l.trim.nonEmpty) " " + l else "")) mkString ("", "\n", if (lineBreak) "\n" else "")
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

  private def renderDefault(defaultValue: Option[String]) =
    defaultValue.fold("")(d ⇒ s" = $d")

  private def renderDefault(value: (Any, ToInput[_, _]), tpe: InputType[_]) = {
    val coercionHelper = new ValueCoercionHelper[Any]

    DefaultValueRenderer.renderInputValueCompact(value, tpe, coercionHelper).fold("")(d ⇒ s" = $d")
  }

  private def renderArg(arg: IntrospectionInputValue) = {
    val argDef = s"${arg.name}: ${renderTypeName(arg.tpe)}"

    argDef + renderDefault(arg.defaultValue)
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

  def renderArgsI(args: Seq[IntrospectionInputValue]) =
    if (args.nonEmpty)
      args map (renderArg(_)) mkString ("(", ", ", ")")
    else
      ""

  private def renderArgs(args: Seq[Argument[_]]) =
    if (args.nonEmpty)
      args map renderArg mkString ("(", ", ", ")")
    else
      ""

  private def renderFieldsI(fields: Seq[IntrospectionField]) =
    if (fields.nonEmpty)
      fields.zipWithIndex map { case (f, idx) ⇒
        (if (idx != 0 && f.description.isDefined) "\n" else "") +
          renderField(f)
      } mkString "\n"
    else
      ""

  private def renderFields(fields: Seq[Field[_, _]]) =
    if (fields.nonEmpty)
      fields.zipWithIndex map { case (f, idx) ⇒
        (if (idx != 0 && f.description.isDefined) "\n" else "") +
          renderField(f)
      } mkString "\n"
    else
      ""

  private def renderInputFieldsI(fields: Seq[IntrospectionInputValue]) =
    if (fields.nonEmpty)
      fields.zipWithIndex map { case (f, idx) ⇒
        (if (idx != 0 && f.description.isDefined) "\n" else "") +
          renderInputField(f)
      } mkString "\n"
    else
      ""

  private def renderInputFields(fields: Seq[InputField[_]]) =
    if (fields.nonEmpty)
      fields.zipWithIndex map { case (f, idx) ⇒
        (if (idx != 0 && f.description.isDefined) "\n" else "") +
          renderInputField(f)
      } mkString "\n"
    else
      ""

  private def renderField(field: IntrospectionField) =
    s"${renderDescription(field.description, prefix = Indention)}$Indention${field.name}${renderArgsI(field.args)}: ${renderTypeName(field.tpe)}${renderDeprecation(field.isDeprecated, field.deprecationReason)}"

  private def renderField(field: Field[_, _]) =
    s"${renderDescription(field.description, prefix = Indention)}$Indention${field.name}${renderArgs(field.arguments)}: ${renderTypeName(field.fieldType)}${renderDeprecation(field.deprecationReason.isDefined, field.deprecationReason)}"

  private def renderInputField(field: IntrospectionInputValue) =
    s"${renderDescription(field.description, prefix = Indention)}$Indention${field.name}: ${renderTypeName(field.tpe)}${renderDefault(field.defaultValue)}"

  private def renderInputField(field: InputField[_]) = {
    val default = field.defaultValue.fold("")(renderDefault(_, field.fieldType))

    s"${renderDescription(field.description, prefix = Indention)}$Indention${field.name}: ${renderTypeName(field.fieldType)}$default"
  }

  private def renderObject(tpe: IntrospectionObjectType) =
    s"${renderDescription(tpe.description)}type ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFieldsI(tpe.fields)}\n}"

  private def renderObject(tpe: ObjectType[_, _]) =
    s"${renderDescription(tpe.description)}type ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe.uniqueFields)}\n}"

  private def renderEnum(tpe: IntrospectionEnumType) =
    s"${renderDescription(tpe.description)}enum ${tpe.name} {\n${renderEnumValuesI(tpe.enumValues)}\n}"

  private def renderEnum(tpe: EnumType[_]) =
    s"${renderDescription(tpe.description)}enum ${tpe.name} {\n${renderEnumValues(tpe.values)}\n}"

  private def renderEnumValuesI(values: Seq[IntrospectionEnumValue]) =
    if (values.nonEmpty)
      values.zipWithIndex map { case (v, idx) ⇒
        (if (idx != 0 && v.description.isDefined) "\n" else "") +
          renderDescription(v.description, prefix = Indention) + Indention + v.name + renderDeprecation(v.isDeprecated, v.deprecationReason)
      } mkString "\n"
    else
      ""

  private def renderEnumValues(values: Seq[EnumValue[_]]) =
    if (values.nonEmpty)
      values.zipWithIndex map { case (v, idx) ⇒
        (if (idx != 0 && v.description.isDefined) "\n" else "") +
          renderDescription(v.description, prefix = Indention) + Indention + v.name + renderDeprecation(v.deprecationReason.isDefined, v.deprecationReason)
      } mkString "\n"
    else
      ""

  private def renderScalar(tpe: IntrospectionScalarType) =
    s"${renderDescription(tpe.description)}scalar ${tpe.name}"

  private def renderScalar(tpe: ScalarType[_]) =
    s"${renderDescription(tpe.description)}scalar ${tpe.name}"

  private def renderInputObject(tpe: IntrospectionInputObjectType) =
    s"${renderDescription(tpe.description)}input ${tpe.name} {\n${renderInputFieldsI(tpe.inputFields)}\n}"

  private def renderInputObject(tpe: InputObjectType[_]) =
    s"${renderDescription(tpe.description)}input ${tpe.name} {\n${renderInputFields(tpe.fields)}\n}"

  private def renderInterface(tpe: IntrospectionInterfaceType) =
    s"${renderDescription(tpe.description)}interface ${tpe.name} {\n${renderFieldsI(tpe.fields)}\n}"

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

  private def renderSchemaDefinition(schema: IntrospectionSchema): Option[String] =
    if (isSchemaOfCommonNames(schema.queryType.name, schema.mutationType.map(_.name), schema.subscriptionType.map(_.name)))
      None
    else {
      val withQuery = (Indention + "query: " + renderTypeName(schema.queryType)) :: Nil
      val withMutation = schema.mutationType.fold(withQuery)(t ⇒ withQuery :+ (Indention + "mutation: " + renderTypeName(t)))
      val withSubs = schema.subscriptionType.fold(withMutation)(t ⇒ withMutation :+ (Indention + "subscription: " + renderTypeName(t)))

      Some(s"schema {\n${withSubs mkString "\n"}\n}")
    }

  private def renderSchemaDefinition(schema: Schema[_, _]): Option[String] =
    if (isSchemaOfCommonNames(schema.query.name, schema.mutation.map(_.name), schema.subscription.map(_.name)))
      None
    else {
      val withQuery = (Indention + "query: " + renderTypeName(schema.query, true)) :: Nil
      val withMutation = schema.mutation.fold(withQuery)(t ⇒ withQuery :+ (Indention + "mutation: " + renderTypeName(t, true)))
      val withSubs = schema.subscription.fold(withMutation)(t ⇒ withMutation :+ (Indention + "subscription: " + renderTypeName(t, true)))

      Some(s"schema {\n${withSubs mkString "\n"}\n}")
    }

  private def isSchemaOfCommonNames(query: String, mutation: Option[String], subscription: Option[String]) =
    query == "Query" && mutation.fold(true)(_ == "Mutation") && subscription.fold(true)(_ == "Subscription")

  private def renderType(tpe: IntrospectionType) =
    tpe match {
      case o: IntrospectionObjectType ⇒ renderObject(o)
      case u: IntrospectionUnionType ⇒ renderUnion(u)
      case i: IntrospectionInterfaceType ⇒ renderInterface(i)
      case io: IntrospectionInputObjectType ⇒ renderInputObject(io)
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

  private def renderDirective(dir: IntrospectionDirective) =
    s"${renderDescription(dir.description)}directive @${dir.name}${renderArgsI(dir.args)} on ${dir.locations.toList.map(renderDirectiveLocation).sorted mkString " | "}"

  def renderSchema(introspectionSchema: IntrospectionSchema): String = {
    val schemaDef = renderSchemaDefinition(introspectionSchema)
    val types = introspectionSchema.types filterNot isBuiltIn sortBy (_.name) map (renderType(_))
    val directives = introspectionSchema.directives filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) sortBy (_.name) map (renderDirective(_))

    (schemaDef.toSeq ++ types ++ directives) mkString TypeSeparator
  }

  def renderSchema[T: InputUnmarshaller](introspectionResult: T): String =
    renderSchema(IntrospectionParser parse introspectionResult)

  def renderSchema(schema: Schema[_, _]): String = {
    val schemaDef = renderSchemaDefinition(schema)
    val types = schema.typeList filterNot isBuiltInType sortBy (_.name) map renderType
    val directives = schema.directives filterNot (d ⇒ Schema.isBuiltInDirective(d.name)) sortBy (_.name) map renderDirective

    (schemaDef.toSeq ++ types ++ directives) mkString TypeSeparator
  }

  def renderIntrospectionSchema(introspectionSchema: IntrospectionSchema): String = {
    val types = introspectionSchema.types filter (tpe ⇒ Schema.isIntrospectionType(tpe.name)) sortBy (_.name) map (renderType(_))
    val directives = introspectionSchema.directives filter (d ⇒ Schema.isBuiltInDirective(d.name)) sortBy (_.name) map (renderDirective(_))

    (types ++ directives) mkString TypeSeparator
  }

  def renderIntrospectionSchema[T: InputUnmarshaller](introspectionResult: T): String =
    renderIntrospectionSchema(IntrospectionParser parse introspectionResult)

  private def isBuiltIn(tpe: IntrospectionType) =
    Schema.isBuiltInType(tpe.name)

  private def isBuiltInType(tpe: Type with Named) =
    Schema.isBuiltInType(tpe.name)
}