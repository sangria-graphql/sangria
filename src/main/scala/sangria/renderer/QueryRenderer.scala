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
    inputListSeparator = ", ",
    formatInputValues = false)

  val PrettyInput = Pretty.copy(
    inputFieldSeparator = "\n",
    formatInputValues = true)

  val Compact = QueryRendererConfig(
    indentLevel = "",
    lineBreak = "",
    separator = "",
    mandatorySeparator = " ",
    mandatoryLineBreak = " ",
    definitionSeparator = "",
    inputFieldSeparator = ",",
    inputListSeparator = ",",
    formatInputValues = false)

  def renderSelections(sels: List[Selection], indent: String, indentLevel: Int, config: QueryRendererConfig) =
    if (sels.nonEmpty)
      "{" + config.lineBreak +
        (sels map (render(_, config, indentLevel + 1)) mkString config.mandatoryLineBreak) +
        config.lineBreak + indent + "}"
    else ""

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

  def render(node: AstNode, config: QueryRendererConfig = Pretty, indentLevel: Int = 0): String = {
    lazy val indent = config.indentLevel * indentLevel

    node match {
      case Document(defs, _, _) ⇒ defs map (render(_, config, indentLevel)) mkString config.definitionSeparator

      case OperationDefinition(OperationType.Query, None, Nil, Nil, sels, _) ⇒
        indent + renderSelections(sels, indent, indentLevel, config)

      case OperationDefinition(opType, name, vars, dirs, sels, _) ⇒
        indent + renderOpType(opType) + config.mandatorySeparator +
          (name getOrElse "") +
          (if (vars.nonEmpty) "(" + (vars map (render(_, config)) mkString ("," + config.separator)) + ")" else "") +
          config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, indent, indentLevel, config)

      case FragmentDefinition(name, typeCondition, dirs, sels, _) ⇒
        indent + "fragment" + config.mandatorySeparator + name + config.mandatorySeparator + "on" + config.mandatorySeparator + typeCondition.name + config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, indent, indentLevel, config)

      case VariableDefinition(name, tpe, defaultValue, _) ⇒
        indent + "$" + name + ":" + config.separator +
          render(tpe, config) +
          (defaultValue map (v ⇒ config.separator + "=" + config.separator + render(v, config)) getOrElse "")

      case NotNullType(ofType, _) ⇒
        render(ofType) + "!"

      case ListType(ofType, _) ⇒
        "[" + render(ofType) + "]"

      case NamedType(name, _) ⇒
        name

      case Field(alias, name, args, dirs, sels, _) ⇒
        indent + (alias map (_ + ":" + config.separator) getOrElse "") + name +
          renderArgs(args, config, withSep = false) +
          (if (dirs.nonEmpty || sels.nonEmpty) config.separator else "") +
          renderDirs(dirs, config, withSep = sels.nonEmpty) +
          renderSelections(sels, indent, indentLevel, config)

      case FragmentSpread(name, dirs, _) ⇒
        indent + "..." + name + renderDirs(dirs, config, frontSep = true)

      case InlineFragment(typeCondition, dirs, sels, _) ⇒
        indent + "..." + config.mandatorySeparator + typeCondition.fold("")("on" + config.mandatorySeparator + _.name) + config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, indent, indentLevel, config)

      case Directive(name, args, _) ⇒
        indent + "@" + name + renderArgs(args, config, withSep = false)

      case Argument(name, value, _) ⇒
        indent + name + ":" + config.separator + render(value, config)

      case IntValue(value, _) ⇒ "" + value
      case BigIntValue(value, _) ⇒ value.toString
      case FloatValue(value, _) ⇒ value.toString
      case BigDecimalValue(value, _) ⇒ value.toString
      case StringValue(value, _) ⇒ '"' + escapeString(value) + '"'
      case BooleanValue(value, _) ⇒ value.toString
      case NullValue(_) ⇒ "null"
      case EnumValue(value, _) ⇒ indent + value
      case ListValue(value, _) ⇒
        "[" + (value map (render(_, config, indentLevel)) mkString (config.inputListSeparator)) + "]"
      case ObjectValue(value, _) ⇒
        "{" + inputLineBreak(config) +
            (value map (render(_, config, inputFieldIndent(config, indentLevel))) mkString config.inputFieldSeparator) +
            inputLineBreak(config) + inputIndent(config, indent) + "}"
      case VariableValue(name, _) ⇒ indent + "$" + name
      case ObjectField(name, value, _) ⇒ indent + name + ":" + config.separator + render(value, config, indentLevel)
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
  formatInputValues: Boolean)