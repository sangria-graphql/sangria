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
    definitionSeparator = "",
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

  def renderDirs(dirs: List[Directive], config: QueryRendererConfig, frontSep: Boolean = false, withSep: Boolean = true) =
    (if (dirs.nonEmpty && frontSep && withSep) config.separator else "") +
      (dirs map (render(_, config)) mkString config.separator) +
      (if (dirs.nonEmpty && !frontSep && withSep) config.separator else "")

  def renderArgs(args: List[Argument], config: QueryRendererConfig, withSep: Boolean = true) =
    if (args.nonEmpty) "(" + (args map (render(_, config)) mkString ("," + config.separator)) + ")" + (if (withSep) config.separator else "") else ""

  def renderOpType(operationType: OperationType) = operationType match {
    case OperationType.Query ⇒ "query"
    case OperationType.Mutation ⇒ "mutation"
    case OperationType.Subscription ⇒ "subscription"
  }

  def shouldRenderComment(comment: Option[Comment], config: QueryRendererConfig) =
    config.renderComments && comment.isDefined

  def renderComment(comment: Option[Comment], indent: String, config: QueryRendererConfig): String =
    if (shouldRenderComment(comment, config))
      comment.get.lines map (l ⇒ indent + "#" + (if (l.trim.nonEmpty) " " else "") + l.trim) mkString (
        "",
        config.mandatoryLineBreak,
        config.mandatoryLineBreak)
    else
      ""

  def renderInputComment(comment: Option[Comment], indent: String, config: QueryRendererConfig) =
    if (config.formatInputValues && shouldRenderComment(comment, config)) renderComment(comment, indent, config) + indent else ""

  def render(node: AstNode, config: QueryRendererConfig = Pretty, indentLevel: Int = 0): String = {
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
          indent + "fragment" + config.mandatorySeparator + name + config.mandatorySeparator + "on" + config.mandatorySeparator + typeCondition.name + config.separator +
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