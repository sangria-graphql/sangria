package sangria.renderer

import org.parboiled2.Position
import sangria.util.StringUtil.{escapeString, escapeBlockString}
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
    renderComments = true,
    formatBlockStrings = true)

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
    renderComments = false,
    formatBlockStrings = false)

  def renderSelections(sels: Vector[Selection], tc: WithTrailingComments, indent: Indent, config: QueryRendererConfig) =
    if (sels.nonEmpty) {
      val rendered = sels.zipWithIndex map {
        case (sel, idx) ⇒
          val prev = if (idx == 0) None else Some(sels(idx - 1))
          val next = if (idx == sels.size - 1) None else Some(sels(idx + 1))

          val trailingNext =
            for (n ← next; c ← n.comments.headOption; cp ← c.position; sp ← sel.position; if cp.line == sp.line) yield c

          val trailing =
            trailingNext orElse (for (c ← tc.trailingComments.headOption; cp ← c.position; sp ← sel.position; if cp.line == sp.line) yield c)

          (if (idx != 0 && shouldRenderComment(sel, prev, config)) config.lineBreak else "") + renderNode(sel, config, indent.inc, prev = prev) +
            trailing.fold("")(c ⇒ renderIndividualComment(c, " ", config))
      } mkString config.mandatoryLineBreak

      "{" +
        config.lineBreak +
        rendered +
        renderTrailingComment(tc, sels.lastOption, indent.inc, config) +
        trailingLineBreak(tc, config) +
        indent.str +
        "}"
    } else ""

  def renderFieldDefinitions(fields: Vector[FieldDefinition], tc: WithTrailingComments, indent: Indent, config: QueryRendererConfig) =
    if (fields.nonEmpty) {
      val rendered = fields.zipWithIndex map {
        case (field, idx) ⇒
          val prev = if (idx == 0) None else Some(fields(idx - 1))
          val next = if (idx == fields.size - 1) None else Some(fields(idx + 1))

          val trailingNext =
            for (n ← next; c ← n.comments.headOption; cp ← c.position; sp ← field.position; if cp.line == sp.line) yield c

          val trailing =
            trailingNext orElse (for (c ← tc.trailingComments.headOption; cp ← c.position; sp ← field.position; if cp.line == sp.line) yield c)

          (if (idx != 0 && shouldRenderComment(field, prev, config)) config.lineBreak else "") +
            renderNode(field, config, indent.inc, prev = prev) +
            trailing.fold("")(c ⇒ renderIndividualComment(c, " ", config))
      } mkString config.mandatoryLineBreak

      "{" +
        config.lineBreak +
        rendered +
        renderTrailingComment(tc, fields.lastOption, indent.inc, config) +
        trailingLineBreak(tc, config) +
        indent.str +
        "}"
    } else "{}"

  def renderDirs(dirs: Vector[Directive], config: QueryRendererConfig, indent: Indent, frontSep: Boolean = false, withSep: Boolean = true) =
    (if (dirs.nonEmpty && frontSep && withSep) config.separator else "") +
      (dirs map (renderNode(_, config, indent.zero)) mkString config.separator) +
      (if (dirs.nonEmpty && !frontSep && withSep) config.separator else "")

  def renderArgs(args: Vector[Argument], indent: Indent, config: QueryRendererConfig, withSep: Boolean = true) =
    if (args.nonEmpty) {
      val argsRendered = args.zipWithIndex map { case (a, idx) ⇒
        (if (idx != 0 && shouldRenderComment(a, None, config)) config.lineBreak else "") +
            (if (shouldRenderComment(a, None, config)) config.mandatoryLineBreak else if (idx != 0) config.separator else "") +
            renderNode(a, config, if (shouldRenderComment(a, None, config)) indent.inc else indent.zero)
      }

      "(" + (argsRendered mkString ",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderInputValueDefs(args: Vector[InputValueDefinition], indent: Indent, config: QueryRendererConfig, withSep: Boolean = true) =
    if (args.nonEmpty) {
      val argsRendered = args.zipWithIndex map { case (a, idx) ⇒
        (if (idx != 0 && shouldRenderComment(a, None, config)) config.lineBreak else "") +
          (if (shouldRenderComment(a, None, config)) config.mandatoryLineBreak else if (idx != 0) config.separator else "") +
          renderNode(a, config, if (shouldRenderComment(a, None, config)) indent.inc else indent.zero)
      }

      "(" + (argsRendered mkString ",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderVarDefs(vars: Vector[VariableDefinition], indent: Indent, config: QueryRendererConfig, withSep: Boolean = true) =
    if (vars.nonEmpty) {
      val varsRendered = vars.zipWithIndex map { case (v, idx) ⇒
        (if (idx != 0 && shouldRenderComment(v, None, config)) config.lineBreak else "") +
          (if (shouldRenderComment(v, None, config)) config.mandatoryLineBreak else if (idx != 0) config.separator else "") +
          renderNode(v, config, if (shouldRenderComment(v, None, config)) indent + 2 else indent.zero)
      }

      "(" + (varsRendered mkString ",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderInputObjectFieldDefs(fields: Vector[InputValueDefinition], tc: WithTrailingComments, indent: Indent, config: QueryRendererConfig) = {
    val fieldsRendered = fields.zipWithIndex map { case (f, idx) ⇒
      val prev = if (idx == 0) None else Some(fields(idx - 1))
      val next = if (idx == fields.size - 1) None else Some(fields(idx + 1))

      val trailingNext =
        for (n ← next; c ← n.comments.headOption; cp ← c.position; sp ← f.position; if cp.line == sp.line) yield c

      val trailing =
        trailingNext orElse (for (c ← tc.trailingComments.headOption; cp ← c.position; sp ← f.position; if cp.line == sp.line) yield c)

      (if (idx != 0 && shouldRenderComment(f, prev, config)) config.lineBreak else "") +
        renderNode(f, config, indent.inc, prev = prev) +
        trailing.fold("")(c ⇒ renderIndividualComment(c, " ", config))
    }

    fieldsRendered mkString config.mandatoryLineBreak
  }

  def renderInterfaces(interfaces: Vector[NamedType], config: QueryRendererConfig, indent: Indent, frontSep: Boolean = false, withSep: Boolean = true) =
    if (interfaces.nonEmpty)
      (if (frontSep) config.mandatorySeparator else "") +
        "implements" + config.mandatorySeparator +
        (interfaces map (renderNode(_, config, indent.zero)) mkString ("," + config.separator)) +
        (if (withSep) config.separator else "")
    else ""

  def renderOpType(operationType: OperationType) = operationType match {
    case OperationType.Query ⇒ "query"
    case OperationType.Mutation ⇒ "mutation"
    case OperationType.Subscription ⇒ "subscription"
  }

  def actualComments(node: WithComments, prev: Option[AstNode]) = {
    val ignoreFirst = for (ls ← prev; p ← ls.position; c ← node.comments.headOption; cp ← c.position) yield cp.line == p.line

    ignoreFirst match {
      case Some(true) ⇒ node.comments.tail
      case _ ⇒ node.comments
    }
  }

  def shouldRenderComment(node: WithComments, prev: Option[AstNode], config: QueryRendererConfig) = {
    val comments = actualComments(node, prev)

    config.renderComments && comments.nonEmpty
  }

  def shouldRenderComment(node: WithTrailingComments, config: QueryRendererConfig) =
    config.renderComments && node.trailingComments.nonEmpty

  def shouldRenderComment(comments: Vector[Comment], config: QueryRendererConfig) =
    config.renderComments && comments.nonEmpty

  private def startsWithWhitespace(text: String) =
    text.charAt(0).isWhitespace

  def renderIndividualComment(node: Comment, indent: String, config: QueryRendererConfig): String =
    indent + "#" + (if (node.text.trim.nonEmpty && !startsWithWhitespace(node.text)) " " else "") + node.text

  def renderComment(node: WithComments, prev: Option[AstNode], indent: Indent, config: QueryRendererConfig): String = {
    val comments = actualComments(node, prev)

    if (shouldRenderComment(comments, config)) {
      val lines = renderCommentLines(comments, node.position, indent, config)

      lines mkString ("", config.mandatoryLineBreak, config.mandatoryLineBreak)
    } else ""
  }

  def renderCommentLines(comments: Vector[Comment], nodePos: Option[Position], indent: Indent, config: QueryRendererConfig) = {
    val nodeLine = nodePos.map(_.line).orElse(comments.last.position.map(_.line + 1)).fold(1)(identity)

    comments.foldRight((nodeLine, Vector.empty[String])) {
      case (c, (lastLine, acc)) ⇒
        val currLine = c.position.fold(lastLine - 1)(_.line)
        val diffLines = lastLine - currLine
        val fill = if (diffLines  > 1) config.lineBreak else ""

        currLine → ((renderIndividualComment(c, indent.str, config) + fill) +: acc)
    }._2
  }

  def renderTrailingComment(node: WithTrailingComments, lastSelection: Option[AstNode], indent: Indent, config: QueryRendererConfig): String = {
    val ignoreFirst = for (ls ← lastSelection; p ← ls.position; c ← node.trailingComments.headOption; cp ← c.position) yield cp.line == p.line
    val comments = ignoreFirst match {
      case Some(true) ⇒ node.trailingComments.tail
      case _ ⇒ node.trailingComments
    }

    if (shouldRenderComment(comments, config)) {
      val lines = renderCommentLines(comments, None, indent, config)

      lines mkString (config.lineBreak + config.mandatoryLineBreak, config.mandatoryLineBreak, "")
    } else ""
  }

  def renderInputComment(node: WithComments, indent: Indent, config: QueryRendererConfig) =
    if (config.formatInputValues && shouldRenderComment(node, None, config)) renderComment(node, None, indent, config) + indent.str else ""

  def renderBlockString(node: AstNode, str: String, indent: Indent, config: QueryRendererConfig) =
    if (str.trim.nonEmpty) {
      val ind = indent.incForce.str
      val lines = escapeBlockString(str).split("\n").map(ind + _)

      lines.mkString("\"\"\"\n", "\n", "\n" + ind + "\"\"\"")
    } else "\"\""

  def render(node: AstNode, config: QueryRendererConfig = Pretty, indentLevel: Int = 0): String =
    renderNode(node, config, Indent(config, indentLevel, indentLevel))

  def renderNode(node: AstNode, config: QueryRendererConfig, indent: Indent, prefix: Option[String] = None, prev: Option[AstNode] = None): String =
    node match {
      case d @ Document(defs, _, _, _) ⇒
        (defs map (renderNode(_, config, indent)) mkString config.definitionSeparator) +
          renderTrailingComment(d, None, indent, config)

      case d @ InputDocument(defs, _, _, _) ⇒
        (defs map (renderNode(_, config, indent)) mkString config.definitionSeparator) +
          renderTrailingComment(d, None, indent, config)

      case op @ OperationDefinition(OperationType.Query, None, vars, dirs, sels, _, _, _) if vars.isEmpty && dirs.isEmpty ⇒
        renderComment(op, prev, indent, config) + indent.str + renderSelections(sels, op, indent, config)

      case op @ OperationDefinition(opType, name, vars, dirs, sels, _, _, _) ⇒
        renderComment(op, prev, indent, config) +
          indent.str + renderOpType(opType) + config.mandatorySeparator +
          (name getOrElse "") +
          renderVarDefs(vars, indent, config, withSep = false) +
          config.separator +
          renderDirs(dirs, config, indent) +
          renderSelections(sels, op, indent, config)

      case fd @ FragmentDefinition(name, typeCondition, dirs, sels, _, _, _) ⇒
        renderComment(fd, prev, indent, config) +
          indent.str + "fragment" + config.mandatorySeparator + name + config.mandatorySeparator + "on" +
          config.mandatorySeparator + typeCondition.name + config.separator +
          renderDirs(dirs, config, indent) +
          renderSelections(sels, fd, indent, config)

      case vd @ VariableDefinition(name, tpe, defaultValue, _, _) ⇒
        renderComment(vd, prev, indent, config) +
          indent.str + "$" + name + ":" + config.separator +
          renderNode(tpe, config, indent.zero) +
          (defaultValue map (v ⇒ config.separator + "=" + config.separator + renderNode(v, config, indent.zero)) getOrElse "")

      case NotNullType(ofType, _) ⇒
        renderNode(ofType, config, indent.zero) + "!"

      case ListType(ofType, _) ⇒
        "[" + renderNode(ofType, config, indent.zero) + "]"

      case NamedType(name, _) ⇒
        name

      case f @ Field(alias, name, args, dirs, sels, _, _, _) ⇒
        renderComment(f, prev, indent, config) +
          indent.str + (alias map (_ + ":" + config.separator) getOrElse "") + name +
          renderArgs(args, indent, config, withSep = false) +
          (if (dirs.nonEmpty || sels.nonEmpty) config.separator else "") +
          renderDirs(dirs, config, indent, withSep = sels.nonEmpty) +
          renderSelections(sels, f, indent, config)

      case fs @ FragmentSpread(name, dirs, _, _) ⇒
        renderComment(fs, prev, indent, config) +
          indent.str + "..." + name + renderDirs(dirs, config, indent, frontSep = true)

      case ifr @ InlineFragment(typeCondition, dirs, sels, _, _, _) ⇒
        renderComment(ifr, prev, indent, config) +
          indent.str + "..." + config.mandatorySeparator + typeCondition.fold("")("on" + config.mandatorySeparator + _.name) + config.separator +
          renderDirs(dirs, config, indent) +
          renderSelections(sels, ifr, indent, config)

      case Directive(name, args, _, _) ⇒
        indent.str + "@" + name + renderArgs(args, indent, config.copy(renderComments = false), withSep = false)

      case a @ Argument(name, value, _, _) ⇒
        renderComment(a, prev, indent, config) +
          indent.str + name + ":" + config.separator + renderNode(value, config, indent.zero)

      case v @ IntValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ BigIntValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ FloatValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ BigDecimalValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ BooleanValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ NullValue(_, _) ⇒ renderInputComment(v, indent, config) + "null"
      case v @ EnumValue(value, _, _) ⇒ renderInputComment(v, indent, config) + value
      case v @ StringValue(value, block, _, _, _) if !block || !config.formatBlockStrings ⇒
        renderInputComment(v, indent, config) + '"' + escapeString(value) + '"'
      case v @ StringValue(value, _, _, _, _) ⇒
        renderInputComment(v, indent, config) + renderBlockString(v, value, indent, config)

      case v @ ListValue(value, _, _) ⇒
        def addIdent(v: Value) = v match {
          case o: ObjectValue ⇒ false
          case _ ⇒ true
        }

        def renderValue(v: Value, idx: Int) =
          if (config.formatInputValues && shouldRenderComment(v, None, config))
            (if (idx != 0) config.lineBreak else "") +
              config.lineBreak +
              renderNode(v, config, indent + (if (addIdent(v)) 1 else 0))
          else
            (if (idx != 0) config.separator else "") + renderNode(v, config, indent)

        renderInputComment(v, indent, config) +
          "[" + (value.zipWithIndex map {case (v, idx) ⇒ renderValue(v, idx)} mkString config.inputListSeparator) + "]"
      case v @ ObjectValue(value, _, _) ⇒
        renderInputComment(v, indent, config) +
          "{" + inputLineBreak(config) +
          (value.zipWithIndex map {case (v, idx) ⇒ (if (idx != 0 && config.formatInputValues && shouldRenderComment(v, None, config)) config.lineBreak else "") + renderNode(v, config, inputFieldIndent(config, indent))} mkString config.inputFieldSeparator) +
          inputLineBreak(config) + inputIndent(config, indent) + "}"
      case VariableValue(name, _, _) ⇒ indent.str + "$" + name
      case v @ ObjectField(name, value, _, _) ⇒
        val rendered =
            if (config.formatInputValues && shouldRenderComment(value, None, config))
              config.lineBreak + renderNode(value, config, indent.inc)
            else
              config.separator + renderNode(value, config, indent)

        (if (config.formatInputValues) renderComment(v, prev, indent, config) else "") +
          indent.str + name + ":" + rendered

      case c @ Comment(_, _) ⇒ renderIndividualComment(c, indent.str, config)

      case std @ ScalarTypeDefinition(name, dirs, _, _) ⇒
        renderComment(std, prev, indent, config) +
          indent.str + "scalar" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true)

      case otd @ ObjectTypeDefinition(name, interfaces, fields, dirs, _, _, _) ⇒
        renderComment(otd, prev, indent, config) +
          indent.str + prefix.getOrElse("") + "type" + config.mandatorySeparator + name +
          config.mandatorySeparator +
          renderInterfaces(interfaces, config, indent) +
          renderDirs(dirs, config, indent) +
          renderFieldDefinitions(fields, otd, indent, config)

      case itd @ InputObjectTypeDefinition(name, fields, dirs, _, _, _) ⇒
        renderComment(itd, prev, indent, config) +
          indent.str + "input" + config.mandatorySeparator + name +
          config.mandatorySeparator +
          renderDirs(dirs, config, indent) +
          "{" +
          config.lineBreak +
          renderInputObjectFieldDefs(fields, itd, indent, config) +
          renderTrailingComment(itd, fields.lastOption, indent.inc, config) +
          trailingLineBreak(itd, config) +
          indent.str +
          "}"

      case itd @ InterfaceTypeDefinition(name, fields, dirs, _, _, _) ⇒
        renderComment(itd, prev, indent, config) +
          indent.str + "interface" + config.mandatorySeparator + name +
          config.separator +
          renderDirs(dirs, config, indent) +
          renderFieldDefinitions(fields, itd, indent, config)

      case utd @ UnionTypeDefinition(name, types, dirs, _, _) ⇒
        renderComment(utd, prev, indent, config) +
          indent.str + "union" + config.mandatorySeparator + name +
            renderDirs(dirs, config, indent, frontSep = true) +
            config.separator + "=" + config.separator +
            (types map(renderNode(_, config, indent.zero)) mkString (config.separator + "|" + config.separator))

      case etd @ EnumTypeDefinition(name, values, dirs, _, _, _) ⇒
        val renderedValues = values.zipWithIndex map {
          case (value, idx) ⇒
            val prev = if (idx == 0) None else Some(values(idx - 1))
            val next = if (idx == values.size - 1) None else Some(values(idx + 1))

            val trailingNext =
              for (n ← next; c ← n.comments.headOption; cp ← c.position; sp ← value.position; if cp.line == sp.line) yield c

            val trailing =
              trailingNext orElse (for (c ← etd.trailingComments.headOption; cp ← c.position; sp ← value.position; if cp.line == sp.line) yield c)

            (if (idx != 0 && shouldRenderComment(value, prev, config)) config.lineBreak else "") +
              renderNode(value, config, indent.inc, prev = prev) +
              trailing.fold("")(c ⇒ renderIndividualComment(c, " ", config))
        } mkString config.mandatoryLineBreak

        renderComment(etd, prev, indent, config) +
            indent.str + "enum" + config.mandatorySeparator + name +
            config.separator +
            renderDirs(dirs, config, indent) +
            "{" +
            config.lineBreak +
            renderedValues +
            renderTrailingComment(etd, values.lastOption, indent.inc, config) +
            trailingLineBreak(etd, config) +
            indent.str +
            "}"

      case evd @ EnumValueDefinition(name, dirs, _, _) ⇒
        renderComment(evd, prev, indent, config) +
            indent.str + name +
            renderDirs(dirs, config, indent, frontSep = true)

      case fd @ FieldDefinition(name, fieldType, args, dirs, _, _) ⇒
        renderComment(fd, prev, indent, config) +
          indent.str + name +
          renderInputValueDefs(args, indent, config, withSep = false) +
          ":" + config.separator + renderNode(fieldType, config, indent.zero) +
          renderDirs(dirs, config, indent, frontSep = true)

      case ivd @ InputValueDefinition(name, valueType, default, dirs, _, _) ⇒
        renderComment(ivd, prev, indent, config) +
          indent.str + name + ":" + config.separator + renderNode(valueType, config, indent.zero) +
          default.fold("")(d ⇒ config.separator + "=" + config.separator + renderNode(d, config, indent.zero)) +
          renderDirs(dirs, config, indent, frontSep = true)

      case ted @ TypeExtensionDefinition(definition, _, _) ⇒
        renderComment(ted, prev, indent, config) +
          renderNode(definition.copy(comments = Vector.empty), config, indent, Some("extend" + config.mandatorySeparator))

      case dd @ DirectiveDefinition(name, args, locations, _, _) ⇒
        val locsRendered = locations.zipWithIndex map { case (l, idx) ⇒
          (if (idx != 0 && shouldRenderComment(l, None, config)) config.lineBreak else "") +
            (if (shouldRenderComment(l, None, config)) config.lineBreak else if (idx != 0) config.separator else "") +
            renderNode(l, config, if (shouldRenderComment(l, None, config)) indent.inc else indent.zero)
        }

        renderComment(dd, prev, indent, config) +
          indent.str + "directive" + config.separator + "@" + name +
          renderInputValueDefs(args, indent, config) + (if (args.isEmpty) config.mandatorySeparator else "") +
          "on" + (if (shouldRenderComment(locations.head, None, config)) "" else config.mandatorySeparator) +
          locsRendered.mkString(config.separator + "|")

      case dl @ DirectiveLocation(name, _, _) ⇒
        renderComment(dl, prev, indent, config) + indent.str + name

      case sd @ SchemaDefinition(ops, dirs, _, _, _) ⇒
        val renderedOps = ops.zipWithIndex map { case (op, idx) ⇒
          (if (idx != 0 && shouldRenderComment(op, None, config)) config.lineBreak else "") +
            renderNode(op, config, indent.inc)
        } mkString config.mandatoryLineBreak

        renderComment(sd, prev, indent, config) +
          indent.str + "schema"  + config.separator +
          renderDirs(dirs, config, indent) +
          "{" +
          config.lineBreak +
          renderedOps +
          renderTrailingComment(sd, None, indent.inc, config) +
          trailingLineBreak(sd, config) +
          indent.str + "}"

      case otd @ OperationTypeDefinition(op, tpe, _, _) ⇒
        renderComment(otd, prev, indent, config) +
          indent.str + renderOpType(op) + ":" + config.separator + renderNode(tpe, config, indent.zero)
    }

  private def trailingLineBreak(tc: WithTrailingComments, config: QueryRendererConfig) =
    if (shouldRenderComment(tc, config)) config.mandatoryLineBreak else config.lineBreak

  def inputLineBreak(config: QueryRendererConfig) =
    if (config.formatInputValues) config.lineBreak
    else ""

  def inputFieldIndent(config: QueryRendererConfig, indent: Indent) =
    if (config.formatInputValues) indent.inc
    else indent.zero

  def inputIndent(config: QueryRendererConfig, indent: Indent) =
    if (config.formatInputValues) indent.str
    else ""
}

case class Indent(config: QueryRendererConfig, level: Int, prevLevel: Int) {
  lazy val str = config.indentLevel * level
  lazy val strPrev = config.indentLevel * prevLevel

  def strForce = if (level > 0) str else strPrev

  lazy val zero = copy(level = 0, prevLevel = if (level == 0) prevLevel else level)

  def +(l: Int) = copy(level = level + l, prevLevel = level)
  def -(l: Int) = {
    val newLevel = level - l

    if (newLevel >= 0) this
    else copy(level = level + l, prevLevel = level)
  }

  def inc = this + 1
  def dec = this - 1

  def incForce =
    if (level > 0) this + 1 else copy(level = prevLevel + 1, prevLevel = prevLevel)
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
  formatBlockStrings: Boolean,
  renderComments: Boolean)