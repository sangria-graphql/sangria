package sangria.renderer

import sangria.integration.InputUnmarshaller
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

  def renderImplementedInterfaces[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "interfaces").fold("")(interfaces ⇒
      interfaces map (stringField(_, "name")) mkString (" implements ", ", ", ""))

  def renderTypeDef[In : InputUnmarshaller](tpe: In): String =
    stringField(tpe, "kind") match {
      case "LIST" ⇒ s"[${renderTypeDef(mapField(tpe, "ofType"))}]"
      case "NON_NULL" ⇒ s"${renderTypeDef(mapField(tpe, "ofType"))}!"
      case _ ⇒ stringField(tpe, "name")
    }

  def renderArg[In : InputUnmarshaller](arg: In) = {
    val argDef = s"${stringField(arg, "name")}: ${renderTypeDef(mapField(arg, "type"))}"
    val default = um.getMapValue(arg, "defaultValue").filter(um.isDefined).fold("")(d ⇒ s" = ${um.getScalarValue(d)}")

    argDef + default
  }

  def renderArgs[In : InputUnmarshaller](field: In)=
    nonEmptyList(field, "args").fold("")(args ⇒
      args map (renderArg(_)) mkString ("(", ", ", ")"))

  def renderFields[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "fields").fold("")(fields ⇒
      fields map (Indention + renderField(_)) mkString ("\n"))

  def renderInputFields[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "inputFields").fold("")(fields ⇒
      fields map (Indention + renderInputField(_)) mkString ("\n"))

  def renderField[In : InputUnmarshaller](field: In) =
    s"${stringField(field, "name")}${renderArgs(field)}: ${renderTypeDef(mapField(field, "type"))}"

  def renderInputField[In : InputUnmarshaller](field: In) =
    s"${stringField(field, "name")}: ${renderTypeDef(mapField(field, "type"))}"

  def renderObject[In : InputUnmarshaller](tpe: In) =
    s"type ${stringField(tpe, "name")}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe)}\n}"

  def renderEnum[In : InputUnmarshaller](tpe: In) =
    s"enum ${stringField(tpe, "name")} {\n${renderEnumValues(tpe)}\n}"

  def renderEnumValues[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "enumValues").fold("")(values ⇒
      values map (Indention + stringField(_, "name")) mkString ("\n"))

  def renderScalar[In : InputUnmarshaller](tpe: In) =
    s"scalar ${stringField(tpe, "name")}"

  def renderInputObject[In : InputUnmarshaller](tpe: In) =
    s"input ${stringField(tpe, "name")} {\n${renderInputFields(tpe)}\n}"

  def renderInterface[In : InputUnmarshaller](tpe: In) =
    s"interface ${stringField(tpe, "name")}${renderImplementedInterfaces(tpe)} {\n${renderFields(tpe)}\n}"

  def renderUnion[In : InputUnmarshaller](tpe: In) =
    nonEmptyList(tpe, "possibleTypes").fold("")(types ⇒
      types map (stringField(_, "name")) mkString (s"union ${stringField(tpe, "name")} = ", " | ", ""))

  def renderType[In : InputUnmarshaller](tpe: In) =
    stringField(tpe, "kind") match {
      case "OBJECT" ⇒ renderObject(tpe)
      case "UNION" ⇒ renderUnion(tpe)
      case "INTERFACE" ⇒ renderInterface(tpe)
      case "INPUT_OBJECT" ⇒ renderInputObject(tpe)
      case "SCALAR" ⇒ renderScalar(tpe)
      case "ENUM" ⇒ renderEnum(tpe)
      case kind ⇒ error(s"Unsupported kind: $kind")
    }

  def renderSchema[T: InputUnmarshaller](introspectionResult: T) = {
    val u = um[T]

    for {
      data ← um.getRootMapValue(introspectionResult, "data")
      schema ← um.getMapValue(data, "__schema")
      types ← um.getMapValue(schema, "types")
      typeList = um.getListValue(types) filter (!isBuiltIn(_)) sortBy (stringField(_, "name"))
    } yield typeList map (renderType(_)) mkString TypeSeparator
  }

  def renderIntrospectionSchema[T: InputUnmarshaller](introspectionResult: T) = {
    val u = um[T]

    for {
      data ← um.getRootMapValue(introspectionResult, "data")
      schema ← um.getMapValue(data, "__schema")
      types ← um.getMapValue(schema, "types")
      typeList = um.getListValue(types) filter (isIntrospectionType(_)) sortBy (stringField(_, "name"))
    } yield typeList map (renderType(_)) mkString TypeSeparator
  }

  def isBuiltIn[In : InputUnmarshaller](tpe: In) =
    isIntrospectionType(tpe) || isBuiltInScalar(tpe)

  def isIntrospectionType[In : InputUnmarshaller](tpe: In) =
    stringField(tpe, "name") startsWith "__"

  def isBuiltInScalar[In : InputUnmarshaller](tpe: In) =
    stringField(tpe, "name") match {
      case "String" | "Boolean" | "Int" | "Long" | "BigInt" | "BigDecimal" | "Float" | "ID" ⇒ true
      case _ ⇒ false
    }

  def stringField[In : InputUnmarshaller](map: In, name: String) =
    um.getMapValue(map, name).map(um.getScalarValue(_).asInstanceOf[String]) getOrElse error(s"Field '$name' is undefined!")

  def mapField[In : InputUnmarshaller](map: In, name: String) =
    um.getMapValue(map, name) getOrElse error(s"Field '$name' is undefined!")

  def nonEmptyList[In : InputUnmarshaller](map: In, name: String) =
    um.getMapValue(map, name) filter um.isDefined map um.getListValue filter (_.nonEmpty)

  private def um[T: InputUnmarshaller] = implicitly[InputUnmarshaller[T]]

  private def error(message: String) = throw new IllegalStateException(message)
}
