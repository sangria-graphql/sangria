package sangria.renderer

import org.parboiled2.Position
import sangria.util.StringUtil.escapeString
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

  def renderSelections(sels: List[Selection], tc: WithTrailingComments, indent: String, indentLevel: Int, config: QueryRendererConfig) =
    if (sels.nonEmpty) {
      val rendered = sels.zipWithIndex map {
        case (sel, idx) ⇒
          (if (idx != 0 && shouldRenderComment(sel, config)) config.lineBreak else "") + render(sel, config, indentLevel + 1)
      } mkString config.mandatoryLineBreak

      "{" +
        config.lineBreak +
        rendered +
        renderTrailingComment(tc, config.indentLevel * (indentLevel + 1), config) +
        trailingLineBreak(tc, config) +
        indent +
        "}"
    } else ""

  def renderFieldDefinitions(fields: List[FieldDefinition], tc: WithTrailingComments, indent: String, indentLevel: Int, config: QueryRendererConfig) =
    if (fields.nonEmpty) {
      val rendered = fields.zipWithIndex map {
        case (field, idx) ⇒
          (if (idx != 0 && shouldRenderComment(field, config)) config.lineBreak else "") + render(field, config, indentLevel + 1)
      } mkString config.mandatoryLineBreak

      "{" +
        config.lineBreak +
        rendered +
        renderTrailingComment(tc, config.indentLevel * (indentLevel + 1), config) +
        trailingLineBreak(tc, config) +
        indent +
        "}"
    } else "{}"

  def renderDirs(dirs: List[Directive], config: QueryRendererConfig, frontSep: Boolean = false, withSep: Boolean = true) =
    (if (dirs.nonEmpty && frontSep && withSep) config.separator else "") +
      (dirs map (render(_, config)) mkString config.separator) +
      (if (dirs.nonEmpty && !frontSep && withSep) config.separator else "")

  def renderArgs(args: List[Argument], indentLevel: Int, config: QueryRendererConfig, withSep: Boolean = true) =
    if (args.nonEmpty) {
      val argsRendered = args.zipWithIndex map { case (a, idx) ⇒
        (if (idx != 0 && shouldRenderComment(a, config)) config.lineBreak else "") +
            (if (shouldRenderComment(a, config)) config.mandatoryLineBreak else if (idx != 0) config.separator else "") +
            render(a, config, if (shouldRenderComment(a, config)) indentLevel + 1 else 0)
      }

      "(" + (argsRendered mkString ",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderInputValueDefs(args: List[InputValueDefinition], indentLevel: Int, config: QueryRendererConfig, withSep: Boolean = true) =
    if (args.nonEmpty) {
      val argsRendered = args.zipWithIndex map { case (a, idx) ⇒
        (if (idx != 0 && shouldRenderComment(a, config)) config.lineBreak else "") +
          (if (shouldRenderComment(a, config)) config.mandatoryLineBreak else if (idx != 0) config.separator else "") +
          render(a, config, if (shouldRenderComment(a, config)) indentLevel + 1 else 0)
      }

      "(" + (argsRendered mkString ",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderVarDefs(vars: List[VariableDefinition], indentLevel: Int, config: QueryRendererConfig, withSep: Boolean = true) =
    if (vars.nonEmpty) {
      val varsRendered = vars.zipWithIndex map { case (v, idx) ⇒
        (if (idx != 0 && shouldRenderComment(v, config)) config.lineBreak else "") +
          (if (shouldRenderComment(v, config)) config.mandatoryLineBreak else if (idx != 0) config.separator else "") +
          render(v, config, if (shouldRenderComment(v, config)) indentLevel + 2 else 0)
      }

      "(" + (varsRendered mkString ",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderInputObjectFieldDefs(fields: List[InputValueDefinition], indentLevel: Int, config: QueryRendererConfig) = {
    val fieldsRendered = fields.zipWithIndex map { case (f, idx) ⇒
      (if (idx != 0 && shouldRenderComment(f, config)) config.lineBreak else "") +
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

  def shouldRenderComment(node: WithComments, config: QueryRendererConfig) =
    config.renderComments && node.comments.nonEmpty

  def shouldRenderComment(node: WithTrailingComments, config: QueryRendererConfig) =
    config.renderComments && node.trailingComments.nonEmpty

  private def startsWithWhitespace(text: String) =
    text.charAt(0).isWhitespace

  def renderIndividualComment(node: Comment, indent: String, config: QueryRendererConfig): String =
    indent + "#" + (if (node.text.trim.nonEmpty && !startsWithWhitespace(node.text)) " " else "") + node.text

  def renderComment(node: WithComments, indent: String, config: QueryRendererConfig): String =
    if (shouldRenderComment(node, config)) {
      val lines = renderCommentLines(node.comments, node.position, indent, config)

      lines mkString ("", config.mandatoryLineBreak, config.mandatoryLineBreak)
    } else ""

  def renderCommentLines(comments: List[Comment], nodePos: Option[Position], indent: String, config: QueryRendererConfig) = {
    val nodeLine = nodePos.map(_.line).orElse(comments.last.position.map(_.line + 1)).fold(1)(identity)

    comments.foldRight((nodeLine, Vector.empty[String])) {
      case (c, (lastLine, acc)) ⇒
        val currLine = c.position.fold(lastLine - 1)(_.line)
        val diffLines = lastLine - currLine
        val fill = if (diffLines  > 1) config.lineBreak else ""

        currLine → ((renderIndividualComment(c, indent, config) + fill) +: acc)
    }._2
  }

  def renderTrailingComment(node: WithTrailingComments, indent: String, config: QueryRendererConfig): String =
    if (shouldRenderComment(node, config)) {
      val lines = renderCommentLines(node.trailingComments, None, indent, config)

      lines mkString (config.lineBreak + config.mandatoryLineBreak, config.mandatoryLineBreak, "")
    } else ""

  def renderInputComment(node: WithComments, indent: String, config: QueryRendererConfig) =
    if (config.formatInputValues && shouldRenderComment(node, config)) renderComment(node, indent, config) + indent else ""

  def render(node: AstNode, config: QueryRendererConfig = Pretty, indentLevel: Int = 0, prefix: Option[String] = None): String = {
    lazy val indent = config.indentLevel * indentLevel

    node match {
      case d @ Document(defs, _, _, _) ⇒
        (defs map (render(_, config, indentLevel)) mkString config.definitionSeparator) +
          renderTrailingComment(d, indent, config)

      case op @ OperationDefinition(OperationType.Query, None, Nil, Nil, sels, _, _, _) ⇒
        renderComment(op, indent, config) + indent + renderSelections(sels, op, indent, indentLevel, config)

      case op @ OperationDefinition(opType, name, vars, dirs, sels, _, _, _) ⇒
        renderComment(op, indent, config) +
          indent + renderOpType(opType) + config.mandatorySeparator +
          (name getOrElse "") +
          renderVarDefs(vars, indentLevel, config, withSep = false) +
          config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, op, indent, indentLevel, config)

      case fd @ FragmentDefinition(name, typeCondition, dirs, sels, _, _, _) ⇒
        renderComment(fd, indent, config) +
          indent + "fragment" + config.mandatorySeparator + name + config.mandatorySeparator + "on" +
          config.mandatorySeparator + typeCondition.name + config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, fd, indent, indentLevel, config)

      case vd @ VariableDefinition(name, tpe, defaultValue, _, _) ⇒
        renderComment(vd, indent, config) +
          indent + "$" + name + ":" + config.separator +
          render(tpe, config) +
          (defaultValue map (v ⇒ config.separator + "=" + config.separator + render(v, config)) getOrElse "")

      case NotNullType(ofType, _) ⇒
        render(ofType) + "!"

      case ListType(ofType, _) ⇒
        "[" + render(ofType) + "]"

      case NamedType(name, _) ⇒
        name

      case f @ Field(alias, name, args, dirs, sels, _, _, _) ⇒
        renderComment(f, indent, config) +
          indent + (alias map (_ + ":" + config.separator) getOrElse "") + name +
          renderArgs(args, indentLevel, config, withSep = false) +
          (if (dirs.nonEmpty || sels.nonEmpty) config.separator else "") +
          renderDirs(dirs, config, withSep = sels.nonEmpty) +
          renderSelections(sels, f, indent, indentLevel, config)

      case fs @ FragmentSpread(name, dirs, _, _) ⇒
        renderComment(fs, indent, config) +
          indent + "..." + name + renderDirs(dirs, config, frontSep = true)

      case ifr @ InlineFragment(typeCondition, dirs, sels, _, _, _) ⇒
        renderComment(ifr, indent, config) +
          indent + "..." + config.mandatorySeparator + typeCondition.fold("")("on" + config.mandatorySeparator + _.name) + config.separator +
          renderDirs(dirs, config) +
          renderSelections(sels, ifr, indent, indentLevel, config)

      case Directive(name, args, _, _) ⇒
        indent + "@" + name + renderArgs(args, indentLevel, config.copy(renderComments = false), withSep = false)

      case a @ Argument(name, value, _, _) ⇒
        renderComment(a, indent, config) +
          indent + name + ":" + config.separator + render(value, config)

      case v @ IntValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ BigIntValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ FloatValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ BigDecimalValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ StringValue(value, _, _) ⇒ renderInputComment(v, indent, config) + '"' + escapeString(value) + '"'
      case v @ BooleanValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ NullValue(_, _) ⇒ renderInputComment(v, indent, config) + "null"
      case v @ EnumValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ ListValue(value, _, _) ⇒
        def addIdent(v: Value) = v match {
          case o: ObjectValue ⇒ false
          case _ ⇒ true
        }

        def renderValue(v: Value, idx: Int) =
          if (config.formatInputValues && shouldRenderComment(v, config))
            (if (idx != 0) config.lineBreak else "") +
              config.lineBreak +
              render(v, config, indentLevel + (if (addIdent(v)) 1 else 0))
          else
            (if (idx != 0) config.separator else "") + render(v, config, indentLevel)

        renderInputComment(v, indent, config) +
          "[" + (value.zipWithIndex map {case (v, idx) ⇒ renderValue(v, idx)} mkString config.inputListSeparator) + "]"
      case v @ ObjectValue(value, _, _) ⇒
        renderInputComment(v, indent, config) +
          "{" + inputLineBreak(config) +
          (value.zipWithIndex map {case (v, idx) ⇒ (if (idx != 0 && config.formatInputValues && shouldRenderComment(v, config)) config.lineBreak else "") + render(v, config, inputFieldIndent(config, indentLevel))} mkString config.inputFieldSeparator) +
          inputLineBreak(config) + inputIndent(config, indent) + "}"
      case VariableValue(name, _, _) ⇒ indent + "$" + name
      case v @ ObjectField(name, value, _, _) ⇒
        val rendered =
            if (config.formatInputValues && shouldRenderComment(value, config))
              config.lineBreak + render(value, config, indentLevel + 1)
            else
              config.separator + render(value, config, indentLevel)

        (if (config.formatInputValues) renderComment(v, indent, config) else "") +
          indent + name + ":" + rendered
      case c @ Comment(_, _) ⇒ renderIndividualComment(c, indent, config)

      case std @ ScalarTypeDefinition(name, dirs, _, _) ⇒
        renderComment(std, indent, config) +
          indent + "scalar" + config.mandatorySeparator + name +
          renderDirs(dirs, config, frontSep = true)

      case otd @ ObjectTypeDefinition(name, interfaces, fields, dirs, _, _, _) ⇒
        renderComment(otd, indent, config) +
          indent + prefix.getOrElse("") + "type" + config.mandatorySeparator + name +
          config.mandatorySeparator +
          renderInterfaces(interfaces, config) +
          renderDirs(dirs, config) +
          renderFieldDefinitions(fields, otd, indent, indentLevel, config)

      case itd @ InputObjectTypeDefinition(name, fields, dirs, _, _, _) ⇒
        renderComment(itd, indent, config) +
          indent + "input" + config.mandatorySeparator + name +
          config.mandatorySeparator +
          renderDirs(dirs, config) +
          "{" +
          config.lineBreak +
          renderInputObjectFieldDefs(fields, indentLevel, config) +
          renderTrailingComment(itd, config.indentLevel * (indentLevel + 1), config) +
          trailingLineBreak(itd, config) +
          indent +
          "}"

      case itd @ InterfaceTypeDefinition(name, fields, dirs, _, _, _) ⇒
        renderComment(itd, indent, config) +
          indent + "interface" + config.mandatorySeparator + name +
          config.separator +
          renderDirs(dirs, config) +
          renderFieldDefinitions(fields, itd, indent, indentLevel, config)

      case utd @ UnionTypeDefinition(name, types, dirs, _, _) ⇒
        renderComment(utd, indent, config) +
          indent + "union" + config.mandatorySeparator + name +
            renderDirs(dirs, config, frontSep = true) +
            config.separator + "=" + config.separator +
            (types map(render(_, config)) mkString (config.separator + "|" + config.separator))

      case etd @ EnumTypeDefinition(name, values, dirs, _, _, _) ⇒
        val renderedValues = values.zipWithIndex map {
          case (value, idx) ⇒
            (if (idx != 0 && shouldRenderComment(value, config)) config.lineBreak else "") +
                render(value, config, indentLevel + 1)
        } mkString config.mandatoryLineBreak

        renderComment(etd, indent, config) +
            indent + "enum" + config.mandatorySeparator + name +
            config.separator +
            renderDirs(dirs, config) +
            "{" +
            config.lineBreak +
            renderedValues +
            renderTrailingComment(etd, config.indentLevel * (indentLevel + 1), config) +
            trailingLineBreak(etd, config) +
            indent +
            "}"

      case evd @ EnumValueDefinition(name, dirs, _, _) ⇒
        renderComment(evd, indent, config) +
            indent + name +
            renderDirs(dirs, config, frontSep = true)

      case fd @ FieldDefinition(name, fieldType, args, dirs, _, _) ⇒
        renderComment(fd, indent, config) +
          indent + name +
          renderInputValueDefs(args, indentLevel, config, withSep = false) +
          ":" + config.separator + render(fieldType) +
          renderDirs(dirs, config, frontSep = true)

      case ivd @ InputValueDefinition(name, valueType, default, dirs, _, _) ⇒
        renderComment(ivd, indent, config) +
          indent + name + ":" + config.separator + render(valueType, config) +
          default.fold("")(d ⇒ config.separator + "=" + config.separator + render(d, config)) +
          renderDirs(dirs, config, frontSep = true)

      case ted @ TypeExtensionDefinition(definition, _, _) ⇒
        renderComment(ted, indent, config) +
          render(definition.copy(comments = Nil), config, indentLevel, Some("extend" + config.mandatorySeparator))

      case dd @ DirectiveDefinition(name, args, locations, _, _) ⇒
        val locsRendered = locations.zipWithIndex map { case (l, idx) ⇒
          (if (idx != 0 && shouldRenderComment(l, config)) config.lineBreak else "") +
            (if (shouldRenderComment(l, config)) config.lineBreak else if (idx != 0) config.separator else "") +
            render(l, config, if (shouldRenderComment(l, config)) indentLevel + 1 else 0)
        }

        renderComment(dd, indent, config) +
          indent + "directive" + config.separator + "@" + name +
          renderInputValueDefs(args, indentLevel, config) +
          "on" + (if (shouldRenderComment(locations.head, config)) "" else config.mandatorySeparator) +
          locsRendered.mkString(config.separator + "|")

      case dl @ DirectiveLocation(name, _, _) ⇒
        renderComment(dl, indent, config) + indent + name

      case sd @ SchemaDefinition(ops, dirs, _, _, _) ⇒
        val renderedOps = ops.zipWithIndex map { case (op, idx) ⇒
          (if (idx != 0 && shouldRenderComment(op, config)) config.lineBreak else "") +
            render(op, config, indentLevel + 1)
        } mkString config.mandatoryLineBreak

        renderComment(sd, indent, config) +
          indent + "schema"  + config.separator +
          renderDirs(dirs, config) +
          "{" +
          config.lineBreak +
          renderedOps +
          renderTrailingComment(sd, config.indentLevel * (indentLevel + 1), config) +
          trailingLineBreak(sd, config) +
          indent + "}"

      case otd @ OperationTypeDefinition(op, tpe, _, _) ⇒
        renderComment(otd, indent, config) +
          indent + renderOpType(op) + ":" + config.separator + render(tpe, config)
    }
  }

  private def trailingLineBreak(tc: WithTrailingComments, config: QueryRendererConfig) =
    if (shouldRenderComment(tc, config)) config.mandatoryLineBreak else config.lineBreak

  def inputLineBreak(config: QueryRendererConfig) =
    if (config.formatInputValues) config.lineBreak
    else ""

  def inputFieldIndent(config: QueryRendererConfig, indent: Int) =
    if (config.formatInputValues) indent + 1
    else 0

  def inputIndent(config: QueryRendererConfig, indent: String) =
    if (config.formatInputValues) indent
    else ""
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