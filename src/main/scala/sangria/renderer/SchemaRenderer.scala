package sangria.renderer

import sangria.execution.InputUnmarshaller
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

  val TypeSeparator = "\n\n"
  val Indention = "  "

  def renderImplementedInterfaces(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    nonEmptyList(u)(tpe, "interfaces").fold("")(interfaces =>
      interfaces map (stringField(u)(_, "name")) mkString (" implements ", ", ", ""))

  def renderTypeDef(u: InputUnmarshaller[_])(tpe: u.LeafNode): String =
    stringField(u)(tpe, "kind") match {
      case "LIST" => s"[${renderTypeDef(u)(mapField(u)(tpe, "ofType"))}]"
      case "NON_NULL" => s"${renderTypeDef(u)(mapField(u)(tpe, "ofType"))}!"
      case _ => stringField(u)(tpe, "name")
    }

  def renderArg(u: InputUnmarshaller[_])(arg: u.LeafNode) = {
    val argDef = s"${stringField(u)(arg, "name")}: ${renderTypeDef(u)(mapField(u)(arg, "type"))}"
    val default = u.getMapValue(arg, "defaultValue").filter(u.isDefined).fold("")(d => s" = ${u.getScalarValue(d)}")

    argDef + default
  }

  def renderArgs(u: InputUnmarshaller[_])(field: u.LeafNode) =
    nonEmptyList(u)(field, "args").fold("")(args =>
      args map renderArg(u) mkString ("(", ", ", ")"))

  def renderFields(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    nonEmptyList(u)(tpe, "fields").fold("")(fields =>
      fields map (Indention + renderField(u)(_)) mkString ("\n"))

  def renderInputFields(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    nonEmptyList(u)(tpe, "inputFields").fold("")(fields =>
      fields map (Indention + renderInputField(u)(_)) mkString ("\n"))

  def renderField(u: InputUnmarshaller[_])(field: u.LeafNode) =
    s"${stringField(u)(field, "name")}${renderArgs(u)(field)}: ${renderTypeDef(u)(mapField(u)(field, "type"))}"

  def renderInputField(u: InputUnmarshaller[_])(field: u.LeafNode) =
    s"${stringField(u)(field, "name")}: ${renderTypeDef(u)(mapField(u)(field, "type"))}"

  def renderObject(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    s"type ${stringField(u)(tpe, "name")}${renderImplementedInterfaces(u)(tpe)} {\n${renderFields(u)(tpe)}\n}"

  def renderEnum(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    s"enum ${stringField(u)(tpe, "name")} {\n${renderEnumValues(u)(tpe)}\n}"

  def renderEnumValues(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    nonEmptyList(u)(tpe, "enumValues").fold("")(values =>
      values map (Indention + stringField(u)(_, "name")) mkString ("\n"))

  def renderScalar(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    s"scalar ${stringField(u)(tpe, "name")}"

  def renderInputObject(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    s"input ${stringField(u)(tpe, "name")} {\n${renderInputFields(u)(tpe)}\n}"

  def renderInterface(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    s"interface ${stringField(u)(tpe, "name")}${renderImplementedInterfaces(u)(tpe)} {\n${renderFields(u)(tpe)}\n}"

  def renderUnion(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    nonEmptyList(u)(tpe, "possibleTypes").fold("")(types =>
      types map (stringField(u)(_, "name")) mkString (s"union ${stringField(u)(tpe, "name")} = ", " | ", ""))

  def renderType(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    stringField(u)(tpe, "kind") match {
      case "OBJECT" => renderObject(u)(tpe)
      case "UNION" => renderUnion(u)(tpe)
      case "INTERFACE" => renderInterface(u)(tpe)
      case "INPUT_OBJECT" => renderInputObject(u)(tpe)
      case "SCALAR" => renderScalar(u)(tpe)
      case "ENUM" => renderEnum(u)(tpe)
      case kind => error(s"Unsupported kind: $kind")
    }

  def renderSchema[T: InputUnmarshaller](introspectionResult: T) = {
    val u = um[T]

    for {
      data <- u.getRootMapValue(introspectionResult, "data")
      schema <- u.getMapValue(data, "__schema")
      types <- u.getMapValue(schema, "types")
      typeList = u.getListValue(types) filter (!isBuiltIn(u)(_)) sortBy (stringField(u)(_, "name"))
    } yield typeList map renderType(u) mkString TypeSeparator
  }

  def renderIntrospectionSchema[T: InputUnmarshaller](introspectionResult: T) = {
    val u = um[T]

    for {
      data <- u.getRootMapValue(introspectionResult, "data")
      schema <- u.getMapValue(data, "__schema")
      types <- u.getMapValue(schema, "types")
      typeList = u.getListValue(types) filter (isIntrospectionType(u)(_)) sortBy (stringField(u)(_, "name"))
    } yield typeList map renderType(u) mkString TypeSeparator
  }

  def isBuiltIn(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    isIntrospectionType(u)(tpe) || isBuiltInScalar(u)(tpe)

  def isIntrospectionType(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    stringField(u)(tpe, "name") startsWith "__"

  def isBuiltInScalar(u: InputUnmarshaller[_])(tpe: u.LeafNode) =
    stringField(u)(tpe, "name") match {
      case "String" | "Boolean" | "Int" | "Long" | "BigInt" | "BigDecimal" | "Float" | "ID" => true
      case _ => false
    }

  def stringField(u: InputUnmarshaller[_])(map: u.LeafNode, name: String) =
    u.getMapValue(map, name).map(u.getScalarValue(_).asInstanceOf[String]) getOrElse error(s"Field '$name' is undefined!")

  def mapField(u: InputUnmarshaller[_])(map: u.LeafNode, name: String) =
    u.getMapValue(map, name) getOrElse error(s"Field '$name' is undefined!")

  def nonEmptyList(u: InputUnmarshaller[_])(map: u.LeafNode, name: String) =
    u.getMapValue(map, name) filter u.isDefined map (u.getListValue(_)) filter (_.nonEmpty)

  private def um[T: InputUnmarshaller] = implicitly[InputUnmarshaller[T]]

  private def error(message: String) = throw new IllegalStateException(message)
}
