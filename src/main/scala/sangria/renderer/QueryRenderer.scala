package sangria.renderer

import java.util.Locale

import sangria.ast._

object QueryRenderer {
  val Pretty = QueryRendererConfig(
    indentLevel = "  ",
    lineBreak = "\n",
    separator = " ",
    mandatorySeparator = " ",
    mandatoryLineBreak = "\n",
    definitionSeparator = "\n\n",
    inputFieldSeparator = ", ",
    inputListSeparator = ",",
    formatInputValues = false,
    renderComments = true)

  val PrettyInput = Pretty.copy(
    inputFieldSeparator = "\n",
    formatInputValues = true,
    renderComments = true)

  val Compact = QueryRendererConfig(
    indentLevel = "",
    lineBreak = "",
    separator = "",
    mandatorySeparator = " ",
    mandatoryLineBreak = " ",
    definitionSeparator = "\n",
    inputFieldSeparator = ",",
    inputListSeparator = ",",
    formatInputValues = false,
    renderComments = false)

  def renderSelections(sels: List[Selection], indent: String, indentLevel: Int, config: QueryRendererConfig) =
    if (sels.nonEmpty) {
      val rendered = sels.zipWithIndex map {
        case (sel, idx) ⇒
          (if (idx != 0 && shouldRenderComment(sel.comment, config)) config.lineBreak else "") + render(sel, config, indentLevel + 1)
      } mkString config.mandatoryLineBreak

      "{" + config.lineBreak + rendered + config.lineBreak + indent + "}"
    } else ""

  def renderFieldDefinitions(fields: List[FieldDefinition], indent: String, indentLevel: Int, config: QueryRendererConfig) =
    if (fields.nonEmpty) {
      val rendered = fields.zipWithIndex map {
        case (field, idx) ⇒
          (if (idx != 0 && shouldRenderComment(field.comment, config)) config.lineBreak else "") + render(field, config, indentLevel + 1)
      } mkString config.mandatoryLineBreak

      "{" + config.lineBreak + rendered + config.lineBreak + indent + "}"
    } else "{}"

  def renderDirs(dirs: List[Directive], config: QueryRendererConfig, frontSep: Boolean = false, withSep: Boolean = true) =
    (if (dirs.nonEmpty && frontSep && withSep) config.separator else "") +
      (dirs map (render(_, config)) mkString config.separator) +
      (if (dirs.nonEmpty && !frontSep && withSep) config.separator else "")

  def renderArgs(args: List[Argument], config: QueryRendererConfig, withSep: Boolean = true) =
    if (args.nonEmpty) "(" + (args map (render(_, config)) mkString ("," + config.separator)) + ")" + (if (withSep) config.separator else "") else ""

  def renderInputValueDefs(args: List[InputValueDefinition], indentLevel: Int, config: QueryRendererConfig, withSep: Boolean = true) =
    if (args.nonEmpty) {
      val argsRendered = args.zipWithIndex map { case (a, idx) ⇒
        (if (idx != 0 && shouldRenderComment(a.comment, config)) config.lineBreak else "") +
          (if (shouldRenderComment(a.comment, config)) config.mandatoryLineBreak else if (idx != 0) config.separator else "") +
          render(a, config, if (shouldRenderComment(a.comment, config)) indentLevel + 1 else 0)
      }

      "(" + (argsRendered mkString (",")) + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderInputObjectFieldDefs(fields: List[InputValueDefinition], indentLevel: Int, config: QueryRendererConfig) = {
    val fieldsRendered = fields.zipWithIndex map { case (f, idx) ⇒
      (if (idx != 0 && shouldRenderComment(f.comment, config)) config.lineBreak else "") +
          render(f, config, indentLevel + 1)
    }

    fieldsRendered mkString config.mandatoryLineBreak
  }

  def renderInterfaces(interfaces: List[NamedType], config: QueryRendererConfig, frontSep: Boolean = false, withSep: Boolean = true) =
    if (interfaces.nonEmpty)
      (if (frontSep) config.mandatorySeparator else "") +
        "implements" + config.mandatorySeparator +
        (interfaces map (render(_, config)) mkString ("," + config.separator)) +
        (if (withSep) config.separator else "")
    else ""

  def renderOpType(operationType: OperationType) = operationType match {
    case OperationType.Query ⇒ "query"
    case OperationType.Mutation ⇒ "mutation"
    case OperationType.Subscription ⇒ "subscription"
  }

  def shouldRenderComment(comment: Option[Comment], config: QueryRendererConfig) =
    config.renderComments && comment.isDefined

  def renderComment(comment: Option[Comment], indent: String, config: QueryRendererConfig): String =
    if (shouldRenderComment(comment, config))
      comment.get.lines map (l ⇒ indent + "#" + (if (l.trim.nonEmpty && !l.trim.startsWith("#")) " " else "") + l.trim) mkString (
        "",
        config.mandatoryLineBreak,
        config.mandatoryLineBreak)
    else
      ""

  def renderInputComment(comment: Option[Comment], indent: String, config: QueryRendererConfig) =
    if (config.formatInputValues && shouldRenderComment(comment, config)) renderComment(comment, indent, config) + indent else ""

  def render(node: AstNode, config: QueryRendererConfig = Pretty, indentLevel: Int = 0, prefix: Option[String] = None): String = {
    lazy val indent = config.indentLevel * indentLevel

    node match {
      case Document(defs, _, _) ⇒ defs map (render(_, config, indentLevel)) mkString config.definitionSeparator

      case OperationDefinition(OperationType.Query, None, Nil, Nil, sels, comment, _) ⇒
        renderComment(comment, indent, config) + indent + renderSelections(sels, indent, indentLevel, config)

      case OperationDefinition(opType, name, vars, dirs, sels, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + renderOpType(opType) + config.mandatorySeparator +
          (name getOrElse "") +
          (if (vars.nonEmpty) "(" + (vars map (render(_, config)) mkString ("," + config.separator)) + ")" else "") +
          config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, indent, indentLevel, config)

      case FragmentDefinition(name, typeCondition, dirs, sels, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + "fragment" + config.mandatorySeparator + name + config.mandatorySeparator + "on" +
          config.mandatorySeparator + typeCondition.name + config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, indent, indentLevel, config)

      case VariableDefinition(name, tpe, defaultValue, _, _) ⇒
        indent + "$" + name + ":" + config.separator +
          render(tpe, config) +
          (defaultValue map (v ⇒ config.separator + "=" + config.separator + render(v, config)) getOrElse "")

      case NotNullType(ofType, _) ⇒
        render(ofType) + "!"

      case ListType(ofType, _) ⇒
        "[" + render(ofType) + "]"

      case NamedType(name, _) ⇒
        name

      case Field(alias, name, args, dirs, sels, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + (alias map (_ + ":" + config.separator) getOrElse "") + name +
          renderArgs(args, config, withSep = false) +
          (if (dirs.nonEmpty || sels.nonEmpty) config.separator else "") +
          renderDirs(dirs, config, withSep = sels.nonEmpty) +
          renderSelections(sels, indent, indentLevel, config)

      case FragmentSpread(name, dirs, _, _) ⇒
        indent + "..." + name + renderDirs(dirs, config, frontSep = true)

      case InlineFragment(typeCondition, dirs, sels, _, _) ⇒
        indent + "..." + config.mandatorySeparator + typeCondition.fold("")("on" + config.mandatorySeparator + _.name) + config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, indent, indentLevel, config)

      case Directive(name, args, _, _) ⇒
        indent + "@" + name + renderArgs(args, config, withSep = false)

      case Argument(name, value, _, _) ⇒
        indent + name + ":" + config.separator + render(value, config)

      case IntValue(value, comment, _) ⇒ renderInputComment(comment, indent, config) + value
      case BigIntValue(value, comment, _) ⇒ renderInputComment(comment, indent, config) + value
      case FloatValue(value, comment, _) ⇒ renderInputComment(comment, indent, config) + value
      case BigDecimalValue(value, comment, _) ⇒ renderInputComment(comment, indent, config) + value
      case StringValue(value, comment, _) ⇒ renderInputComment(comment, indent, config) + '"' + escapeString(value) + '"'
      case BooleanValue(value, comment, _) ⇒ renderInputComment(comment, indent, config) + value
      case NullValue(comment, _) ⇒ renderInputComment(comment, indent, config) + "null"
      case EnumValue(value, comment, _) ⇒ renderInputComment(comment, indent, config) + value
      case ListValue(value, comment, _) ⇒
        def addIdent(v: Value) = v match {
          case o: ObjectValue ⇒ false
          case _ ⇒ true
        }

        def renderValue(v: Value, idx: Int) =
          if (config.formatInputValues && shouldRenderComment(v.comment, config))
            (if (idx != 0) config.lineBreak else "") +
              config.lineBreak +
              render(v, config, indentLevel + (if (addIdent(v)) 1 else 0))
          else
            (if (idx != 0) config.separator else "") + render(v, config, indentLevel)


        renderInputComment(comment, indent, config) +
          "[" + (value.zipWithIndex map {case (v, idx) ⇒ renderValue(v, idx)} mkString config.inputListSeparator) + "]"
      case ObjectValue(value, comment, _) ⇒
        renderInputComment(comment, indent, config) +
          "{" + inputLineBreak(config) +
          (value.zipWithIndex map {case (v, idx) ⇒ (if (idx != 0 && config.formatInputValues && shouldRenderComment(v.comment, config)) config.lineBreak else "") + render(v, config, inputFieldIndent(config, indentLevel))} mkString config.inputFieldSeparator) +
          inputLineBreak(config) + inputIndent(config, indent) + "}"
      case VariableValue(name, _, _) ⇒ indent + "$" + name
      case ObjectField(name, value, comment, _) ⇒
        val rendered =
            if (config.formatInputValues && shouldRenderComment(value.comment, config))
              config.lineBreak + render(value, config, indentLevel + 1)
            else
              config.separator + render(value, config, indentLevel)

        (if (config.formatInputValues) renderComment(comment, indent, config) else "") +
          indent + name + ":" + rendered
      case c @ Comment(_, _) ⇒ renderComment(Some(c), indent, config)

      case ScalarTypeDefinition(name, dirs, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + "scalar" + config.mandatorySeparator + name +
          renderDirs(dirs, config, frontSep = true)

      case ObjectTypeDefinition(name, interfaces, fields, dirs, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + prefix.getOrElse("") + "type" + config.mandatorySeparator + name +
          config.mandatorySeparator +
          renderInterfaces(interfaces, config) +
          renderDirs(dirs, config) +
          renderFieldDefinitions(fields, indent, indentLevel, config)

      case InputObjectTypeDefinition(name, fields, dirs, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + "input" + config.mandatorySeparator + name +
          config.mandatorySeparator +
          renderDirs(dirs, config) +
          "{" + config.lineBreak +
          renderInputObjectFieldDefs(fields, indentLevel, config) +
          config.lineBreak +
          indent + "}"

      case InterfaceTypeDefinition(name, fields, dirs, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + "interface" + config.mandatorySeparator + name +
          config.separator +
          renderDirs(dirs, config) +
          renderFieldDefinitions(fields, indent, indentLevel, config)

      case UnionTypeDefinition(name, types, dirs, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + "union" + config.mandatorySeparator + name +
            renderDirs(dirs, config, frontSep = true) +
            config.separator + "=" + config.separator +
            (types map(render(_, config)) mkString (config.separator + "|" + config.separator))

      case EnumTypeDefinition(name, values, dirs, comment, _) ⇒
        val renderedValues = values.zipWithIndex map { case (value, idx) ⇒
            (if (idx != 0 && shouldRenderComment(value.comment, config)) config.lineBreak else "") +
                render(value, config, indentLevel + 1)
        } mkString config.mandatoryLineBreak

        renderComment(comment, indent, config) +
            indent + "enum" + config.mandatorySeparator + name +
            config.separator +
            renderDirs(dirs, config) +
            "{" + config.lineBreak + renderedValues + config.lineBreak + indent + "}"

      case EnumValueDefinition(name, dirs, comment, _) ⇒
        renderComment(comment, indent, config) +
            indent + name +
            renderDirs(dirs, config, frontSep = true)

      case FieldDefinition(name, fieldType, args, dirs, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + name +
          renderInputValueDefs(args, indentLevel, config, withSep = false) +
          ":" + config.separator + render(fieldType) +
          renderDirs(dirs, config, frontSep = true)

      case InputValueDefinition(name, valueType, default, dirs, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + name + ":" + config.separator + render(valueType, config) +
          default.fold("")(d ⇒ config.separator + "=" + config.separator + render(d, config)) +
          renderDirs(dirs, config, frontSep = true)

      case TypeExtensionDefinition(definition, comment, _) ⇒
        renderComment(comment, indent, config) +
          render(definition.copy(comment = None), config, indentLevel, Some("extend" + config.mandatorySeparator))

      case DirectiveDefinition(name, args, locations, comment, _) ⇒
        val locsRendered = locations.zipWithIndex map { case (l, idx) ⇒
          (if (idx != 0 && shouldRenderComment(l.comment, config)) config.lineBreak else "") +
            (if (shouldRenderComment(l.comment, config)) config.lineBreak else if (idx != 0) config.separator else "") +
            render(l, config, if (shouldRenderComment(l.comment, config)) indentLevel + 1 else 0)
        }

        renderComment(comment, indent, config) +
          indent + "directive" + config.separator + "@" + name +
          renderInputValueDefs(args, indentLevel, config) +
          "on" + (if (shouldRenderComment(locations.head.comment, config)) "" else config.mandatorySeparator) +
          locsRendered.mkString(config.separator + "|")

      case DirectiveLocation(name, comment, _) ⇒
        renderComment(comment, indent, config) + indent + name

      case SchemaDefinition(ops, dirs, comment, _) ⇒
        val renderedOps = ops.zipWithIndex map { case (op, idx) ⇒
          (if (idx != 0 && shouldRenderComment(op.comment, config)) config.lineBreak else "") +
            render(op, config, indentLevel + 1)
        } mkString config.mandatoryLineBreak

        renderComment(comment, indent, config) +
          indent + "schema"  + config.separator +
          renderDirs(dirs, config) +
          "{" + config.lineBreak + renderedOps + config.lineBreak + indent + "}"

      case OperationTypeDefinition(op, tpe, comment, _) ⇒
        renderComment(comment, indent, config) +
          indent + renderOpType(op) + ":" + config.separator + render(tpe, config)
    }
  }

  def inputLineBreak(config: QueryRendererConfig) =
    if (config.formatInputValues) config.lineBreak
    else ""

  def inputFieldIndent(config: QueryRendererConfig, indent: Int) =
    if (config.formatInputValues) indent + 1
    else 0

  def inputIndent(config: QueryRendererConfig, indent: String) =
    if (config.formatInputValues) indent
    else ""

  def escapeString(str: String) =
    str flatMap {
      case ch if ch > 0xfff ⇒ "\\u" + charHex(ch)
      case ch if ch > 0xff ⇒ "\\u0" + charHex(ch)
      case ch if ch > 0x7f ⇒ "\\u00" + charHex(ch)
      case ch if ch < 32 ⇒
        ch match {
          case '\b' ⇒ "\\b"
          case '\n' ⇒ "\\n"
          case '\t' ⇒ "\\t"
          case '\f' ⇒ "\\f"
          case '\r' ⇒ "\\r"
          case ch if ch > 0xf ⇒ "\\u00" + charHex(ch)
          case ch ⇒ "\\u000" + charHex(ch)
        }
      case '"' ⇒ "\\\""
      case '\\' ⇒ "\\\\"
      case ch ⇒ ch.toString
    }

  def charHex(ch: Char): String =
    Integer.toHexString(ch).toUpperCase(Locale.ENGLISH)
}

case class QueryRendererConfig(
  indentLevel: String,
  lineBreak: String,
  mandatorySeparator: String,
  mandatoryLineBreak: String,
  separator: String,
  definitionSeparator: String,
  inputFieldSeparator: String,
  inputListSeparator: String,
  formatInputValues: Boolean,
  renderComments: Boolean)