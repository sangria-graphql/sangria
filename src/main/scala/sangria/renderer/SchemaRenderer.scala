package sangria.renderer

import sangria.execution.ValueCoercionHelper
import sangria.introspection._
import sangria.marshalling.{ResultMarshaller, InputUnmarshaller}
import sangria.parser.DeliveryScheme.Throw
import sangria.schema._

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

  private def renderImplementedInterfaces(tpe: IntrospectionObjectType) =
    if (tpe.interfaces.nonEmpty)
      tpe.interfaces map (_.name) mkString (" implements ", ", ", "")
    else
      ""

  private def renderImplementedInterfaces(tpe: ObjectLikeType[_, _])(implicit m: ResultMarshaller) =
    if (tpe.interfaces.nonEmpty)
      tpe.interfaces map (_.name) mkString (" implements ", ", ", "")
    else
      ""

  def renderTypeName(tpe: IntrospectionTypeRef): String =
    tpe match {
      case IntrospectionListTypeRef(ofType) ⇒ s"[${renderTypeName(ofType)}]"
      case IntrospectionNonNullTypeRef(ofType) ⇒ s"${renderTypeName(ofType)}!"
      case IntrospectionNamedTypeRef(_, name) ⇒ name
    }

  private def renderArg(arg: IntrospectionInputValue) = {
    val argDef = s"${arg.name}: ${renderTypeName(arg.tpe)}"
    val default = arg.defaultValue.fold("")(d ⇒ s" = $d")

    argDef + default
  }

  private def renderArg(arg: Argument[_])(implicit m: ResultMarshaller) = {
    val coercionHelper = new ValueCoercionHelper[Any]

    val argDef = s"${arg.name}: ${renderTypeName(arg.argumentType)}"
    val default = arg.defaultValue.fold("")(d ⇒ s" = ${DefaultValueRenderer.renderInputValueCompact(d, arg.argumentType, coercionHelper)}")

    argDef + default
  }

  def renderArgs(args: Seq[IntrospectionInputValue])=
    if (args.nonEmpty)
      args map renderArg mkString ("(", ", ", ")")
    else
      ""

  private def renderArgs(args: Seq[Argument[_]])(implicit m: ResultMarshaller) =
    if (args.nonEmpty)
      args map renderArg mkString ("(", ", ", ")")
    else
      ""

  private def renderFields(fields: Seq[IntrospectionField]) =
    if (fields.nonEmpty)
      fields map (Indention + renderField(_)) mkString "\n"
    else
      ""

  private def renderFields(fields: Seq[Field[_, _]])(implicit m: ResultMarshaller) =
    if (fields.nonEmpty)
      fields map (Indention + renderField(_)) mkString "\n"
    else
      ""

  private def renderInputFieldsI(fields: Seq[IntrospectionInputValue]) =
    if (fields.nonEmpty)
      fields map (Indention + renderInputField(_)) mkString "\n"
    else
      ""

  private def renderInputFields(fields: Seq[InputField[_]]) =
    if (fields.nonEmpty)
      fields map (Indention + renderInputField(_)) mkString "\n"
    else
      ""

  private def renderField(field: IntrospectionField) =
    s"${field.name}${renderArgs(field.args)}: ${renderTypeName(field.tpe)}"

  private def renderField(field: Field[_, _])(implicit m: ResultMarshaller) =
    s"${field.name}${renderArgs(field.arguments)}: ${renderTypeName(field.fieldType)}"

  private def renderInputField(field: IntrospectionInputValue) =
    s"${field.name}: ${renderTypeName(field.tpe)}"

  private def renderInputField(field: InputField[_])(implicit m: ResultMarshaller) =
    s"${field.name}: ${renderTypeName(field.fieldType)}"

  private def renderObject(tpe: IntrospectionObjectType) =
    s"type ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe.fields)}\n}"

  private def renderObject(tpe: ObjectType[_, _])(implicit m: ResultMarshaller) =
    s"type ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe.uniqueFields)}\n}"

  private def renderEnum(tpe: IntrospectionEnumType) =
    s"enum ${tpe.name} {\n${renderEnumValuesI(tpe.enumValues)}\n}"

  private def renderEnum(tpe: EnumType[_]) =
    s"enum ${tpe.name} {\n${renderEnumValues(tpe.values)}\n}"

  private def renderEnumValuesI(values: Seq[IntrospectionEnumValue]) =
    if (values.nonEmpty)
      values map (Indention + _.name) mkString "\n"
    else
      ""

  private def renderEnumValues(values: Seq[EnumValue[_]]) =
    if (values.nonEmpty)
      values map (Indention + _.name) mkString "\n"
    else
      ""

  private def renderScalar(tpe: IntrospectionScalarType) =
    s"scalar ${tpe.name}"

  private def renderScalar(tpe: ScalarType[_]) =
    s"scalar ${tpe.name}"

  private def renderInputObject(tpe: IntrospectionInputObjectType) =
    s"input ${tpe.name} {\n${renderInputFieldsI(tpe.inputFields)}\n}"

  private def renderInputObject(tpe: InputObjectType[_])(implicit m: ResultMarshaller) =
    s"input ${tpe.name} {\n${renderInputFields(tpe.fields)}\n}"

  private def renderInterface(tpe: IntrospectionInterfaceType) =
    s"interface ${tpe.name} {\n${renderFields(tpe.fields)}\n}"

  private def renderInterface(tpe: InterfaceType[_, _])(implicit m: ResultMarshaller) =
    s"interface ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe.fields)}\n}"

  private def renderUnion(tpe: IntrospectionUnionType) =
    if (tpe.possibleTypes.nonEmpty)
      tpe.possibleTypes map (_.name) mkString (s"union ${tpe.name} = ", " | ", "")
    else
      ""

  private def renderUnion(tpe: UnionType[_])(implicit m: ResultMarshaller) =
    if (tpe.types.nonEmpty)
      tpe.types map (_.name) mkString (s"union ${tpe.name} = ", " | ", "")
    else
      ""

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

  private def renderType(tpe: Type)(implicit m: ResultMarshaller) =
    tpe match {
      case o: ObjectType[_, _] ⇒ renderObject(o)
      case u: UnionType[_] ⇒ renderUnion(u)
      case i: InterfaceType[_, _] ⇒ renderInterface(i)
      case io: InputObjectType[_] ⇒ renderInputObject(io)
      case s: ScalarType[_] ⇒ renderScalar(s)
      case e: EnumType[_] ⇒ renderEnum(e)
      case _ ⇒ throw new IllegalArgumentException(s"Unsupported type: $tpe")
    }

  def renderSchema(introspectionSchema: IntrospectionSchema): String =
    introspectionSchema.types filterNot isBuiltIn sortBy (_.name) map renderType mkString TypeSeparator

  def renderSchema[T: InputUnmarshaller](introspectionResult: T): String =
    renderSchema(IntrospectionParser parse introspectionResult)

  def renderSchema(schema: Schema[_, _])(implicit m: ResultMarshaller): String =
    schema.typeList filterNot isBuiltInType sortBy (_.name) map renderType mkString TypeSeparator

  def renderIntrospectionSchema(introspectionSchema: IntrospectionSchema): String =
    introspectionSchema.types filter (tpe ⇒ Schema.isIntrospectionType(tpe.name)) sortBy (_.name) map renderType mkString TypeSeparator

  def renderIntrospectionSchema[T: InputUnmarshaller](introspectionResult: T): String =
    renderIntrospectionSchema(IntrospectionParser parse introspectionResult)

  private def isBuiltIn(tpe: IntrospectionType) =
    Schema.isBuiltInType(tpe.name)

  private def isBuiltInType(tpe: Type with Named) =
    Schema.isBuiltInType(tpe.name)
}
