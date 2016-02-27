package sangria.renderer

import sangria.execution.ValueCoercionHelper
import sangria.marshalling.{ResultMarshaller, InputUnmarshaller}
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

  private def renderImplementedInterfaces[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "interfaces").fold("")(interfaces ⇒
      interfaces map (stringField(_, "name")) mkString (" implements ", ", ", ""))

  private def renderImplementedInterfaces(tpe: ObjectLikeType[_, _])(implicit m: ResultMarshaller) =
    if (tpe.interfaces.nonEmpty)
      tpe.interfaces map (_.name) mkString (" implements ", ", ", "")
    else
      ""

  private def renderTypeDef[In : InputUnmarshaller](tpe: In): String =
    stringField(tpe, "kind") match {
      case "LIST" ⇒ s"[${renderTypeDef(mapField(tpe, "ofType"))}]"
      case "NON_NULL" ⇒ s"${renderTypeDef(mapField(tpe, "ofType"))}!"
      case _ ⇒ stringField(tpe, "name")
    }

  private def renderArg[In : InputUnmarshaller](arg: In) = {
    val argDef = s"${stringField(arg, "name")}: ${renderTypeDef(mapField(arg, "type"))}"
    val default = um.getMapValue(arg, "defaultValue").filter(um.isDefined).fold("")(d ⇒ s" = ${um.getScalarValue(d)}")

    argDef + default
  }

  private def renderArg(arg: Argument[_])(implicit m: ResultMarshaller) = {
    val coercionHelper = new ValueCoercionHelper[Any]

    val argDef = s"${arg.name}: ${renderTypeName(arg.argumentType)}"
    val default = arg.defaultValue.fold("")(d ⇒ s" = ${DefaultValueRenderer.renderInputValueCompact(d, arg.argumentType, coercionHelper)}")

    argDef + default
  }

  def renderArgs[In : InputUnmarshaller](field: In)=
    nonEmptyList(field, "args").fold("")(args ⇒
      args map (renderArg(_)) mkString ("(", ", ", ")"))

  private def renderArgs(args: Seq[Argument[_]])(implicit m: ResultMarshaller) =
    if (args.nonEmpty)
      args map renderArg mkString ("(", ", ", ")")
    else
      ""

  private def renderFields[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "fields").fold("")(fields ⇒
      fields map (Indention + renderField(_)) mkString "\n")

  private def renderFields(fields: Seq[Field[_, _]])(implicit m: ResultMarshaller) =
    if (fields.nonEmpty)
      fields map (Indention + renderField(_)) mkString "\n"
  else
      ""

  private def renderInputFields[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "inputFields").fold("")(fields ⇒
      fields map (Indention + renderInputField(_)) mkString "\n")

  private def renderInputFields(fields: Seq[InputField[_]]) =
    if (fields.nonEmpty)
      fields map (Indention + renderInputField(_)) mkString "\n"
    else
      ""

  private def renderField[In : InputUnmarshaller](field: In) =
    s"${stringField(field, "name")}${renderArgs(field)}: ${renderTypeDef(mapField(field, "type"))}"

  private def renderField(field: Field[_, _])(implicit m: ResultMarshaller) =
    s"${field.name}${renderArgs(field.arguments)}: ${renderTypeName(field.fieldType)}"

  private def renderInputField[In : InputUnmarshaller](field: In) =
    s"${stringField(field, "name")}: ${renderTypeDef(mapField(field, "type"))}"

  private def renderInputField(field: InputField[_])(implicit m: ResultMarshaller) =
    s"${field.name}: ${renderTypeName(field.fieldType)}"

  private def renderObject[In : InputUnmarshaller](tpe: In) =
    s"type ${stringField(tpe, "name")}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe)}\n}"

  private def renderObject(tpe: ObjectType[_, _])(implicit m: ResultMarshaller) =
    s"type ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe.fields)}\n}"

  private def renderEnum[In : InputUnmarshaller](tpe: In) =
    s"enum ${stringField(tpe, "name")} {\n${renderEnumValues(tpe)}\n}"

  private def renderEnum(tpe: EnumType[_]) =
    s"enum ${tpe.name} {\n${renderEnumValues(tpe.values)}\n}"

  private def renderEnumValues[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "enumValues").fold("")(values ⇒
      values map (Indention + stringField(_, "name")) mkString "\n")

  private def renderEnumValues(values: Seq[EnumValue[_]]) =
    if (values.nonEmpty)
      values map (Indention + _.name) mkString "\n"
    else
      ""

  private def renderScalar[In : InputUnmarshaller](tpe: In) =
    s"scalar ${stringField(tpe, "name")}"

  private def renderScalar(tpe: ScalarType[_]) =
    s"scalar ${tpe.name}"

  private def renderInputObject[In : InputUnmarshaller](tpe: In) =
    s"input ${stringField(tpe, "name")} {\n${renderInputFields(tpe)}\n}"

  private def renderInputObject(tpe: InputObjectType[_])(implicit m: ResultMarshaller) =
    s"input ${tpe.name} {\n${renderInputFields(tpe.fields)}\n}"

  private def renderInterface[In : InputUnmarshaller](tpe: In) =
    s"interface ${stringField(tpe, "name")}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe)}\n}"

  private def renderInterface(tpe: InterfaceType[_, _])(implicit m: ResultMarshaller) =
    s"interface ${tpe.name}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe.fields)}\n}"

  private def renderUnion[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "possibleTypes").fold("")(types ⇒
      types map (stringField(_, "name")) mkString (s"union ${stringField(tpe, "name")} = ", " | ", ""))

  private def renderUnion(tpe: UnionType[_])(implicit m: ResultMarshaller) =
    if (tpe.types.nonEmpty)
      tpe.types map (_.name) mkString (s"union ${tpe.name} = ", " | ", "")
    else
      ""

  private def renderType[In : InputUnmarshaller](tpe: In) =
    stringField(tpe, "kind") match {
      case "OBJECT" ⇒ renderObject(tpe)
      case "UNION" ⇒ renderUnion(tpe)
      case "INTERFACE" ⇒ renderInterface(tpe)
      case "INPUT_OBJECT" ⇒ renderInputObject(tpe)
      case "SCALAR" ⇒ renderScalar(tpe)
      case "ENUM" ⇒ renderEnum(tpe)
      case kind ⇒ error(s"Unsupported kind: $kind")
    }

  private def renderType(schema: Schema[_, _], tpe: Type)(implicit m: ResultMarshaller) =
    tpe match {
      case o: ObjectType[_, _] ⇒ renderObject(o)
      case u: UnionType[_] ⇒ renderUnion(u)
      case i: InterfaceType[_, _] ⇒ renderInterface(i)
      case io: InputObjectType[_] ⇒ renderInputObject(io)
      case s: ScalarType[_] ⇒ renderScalar(s)
      case e: EnumType[_] ⇒ renderEnum(e)
      case _ ⇒ error(s"Unsupported type: $tpe")
    }

  def renderSchema[T: InputUnmarshaller](introspectionResult: T) = {
    val u = um[T]

    checkErrors(introspectionResult)

    for {
      data ← um.getRootMapValue(introspectionResult, "data")
      schema ← um.getMapValue(data, "__schema")
      types ← um.getMapValue(schema, "types")
      typeList = um.getListValue(types) filterNot isBuiltIn sortBy (stringField(_, "name"))
    } yield typeList map (renderType(_)) mkString TypeSeparator
  }

  def renderSchema(schema: Schema[_, _])(implicit m: ResultMarshaller) =
    schema.typeList filterNot isBuiltInType map (renderType(schema, _)) mkString TypeSeparator

  def renderIntrospectionSchema[T: InputUnmarshaller](introspectionResult: T) = {
    val u = um[T]

    checkErrors(introspectionResult)

    for {
      data ← um.getRootMapValue(introspectionResult, "data")
      schema ← um.getMapValue(data, "__schema")
      types ← um.getMapValue(schema, "types")
      typeList = um.getListValue(types) filter (tpe ⇒ isIntrospectionType(stringField(tpe, "name"))) sortBy (stringField(_, "name"))
    } yield typeList map (renderType(_)) mkString TypeSeparator
  }

  private def checkErrors[T : InputUnmarshaller](introspectionResult: T): Unit = {
    um.getRootMapValue(introspectionResult, "errors") match {
      case Some(errors) ⇒
        throw new IllegalArgumentException(
          s"Can't render introspection results because it contains errors: ${um.render(errors)}")
      case None ⇒ // everything is fine
    }
  }

  private def isBuiltIn[In : InputUnmarshaller](tpe: In) = {
    val tpeName = stringField(tpe, "name")

    isIntrospectionType(tpeName) || isBuiltInScalar(tpeName)
  }

  private def isBuiltInType(tpe: Type with Named) =
    isIntrospectionType(tpe.name) || isBuiltInScalar(tpe.name)

  def isIntrospectionType(tpeName: String) = tpeName startsWith "__"

  def isBuiltInScalar(tpeName: String) =
    tpeName match {
      case "String" | "Boolean" | "Int" | "Long" | "BigInt" | "BigDecimal" | "Float" | "ID" ⇒ true
      case _ ⇒ false
    }

  private def stringField[In : InputUnmarshaller](map: In, name: String) =
    um.getMapValue(map, name).map(um.getScalarValue(_).asInstanceOf[String]) getOrElse error(s"Field '$name' is undefined!")

  private def mapField[In : InputUnmarshaller](map: In, name: String) =
    um.getMapValue(map, name) getOrElse error(s"Field '$name' is undefined!")

  private def nonEmptyList[In : InputUnmarshaller](map: In, name: String) =
    um.getMapValue(map, name) filter um.isDefined map um.getListValue filter (_.nonEmpty)

  private def um[T: InputUnmarshaller] = implicitly[InputUnmarshaller[T]]

  private def error(message: String) = throw new IllegalStateException(message)
}
