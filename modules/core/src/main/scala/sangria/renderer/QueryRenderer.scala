package sangria.renderer

import sangria.ast.AstLocation
import sangria.util.StringUtil.{escapeBlockString, escapeString}
import sangria.ast._

object QueryRenderer {
  val Pretty: QueryRendererConfig = QueryRendererConfig(
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
    formatBlockStrings = true
  )

  val PrettyInput: QueryRendererConfig =
    Pretty.copy(inputFieldSeparator = "\n", formatInputValues = true, renderComments = true)

  val Compact: QueryRendererConfig = QueryRendererConfig(
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
    formatBlockStrings = false
  )

  def renderSelections(
      sels: Vector[Selection],
      tc: WithTrailingComments,
      indent: Indent,
      config: QueryRendererConfig): String =
    if (sels.nonEmpty) {
      val rendered = sels.iterator.zipWithIndex
        .map { case (sel, idx) =>
          val prev = if (idx == 0) None else Some(sels(idx - 1))
          val next = if (idx == sels.size - 1) None else Some(sels(idx + 1))

          val trailingNext =
            for {
              n <- next
              c <- n.comments.headOption
              cp <- c.location
              sp <- sel.location
              if cp.line == sp.line
            } yield c

          val trailing =
            trailingNext.orElse(for {
              c <- tc.trailingComments.headOption
              cp <- c.location
              sp <- sel.location
              if cp.line == sp.line
            } yield c)

          (if (idx != 0 && shouldRenderComment(sel, prev, config)) config.lineBreak
           else "") + renderNode(sel, config, indent.inc, prev = prev) +
            trailing.fold("")(c => renderIndividualComment(c, " ", config))
        }
        .mkString(config.mandatoryLineBreak)

      "{" +
        config.lineBreak +
        rendered +
        renderTrailingComment(tc, sels.lastOption, indent.inc, config) +
        trailingLineBreak(tc, config) +
        indent.str +
        "}"
    } else ""

  def renderFieldDefinitions(
      fields: Vector[FieldDefinition],
      tc: WithTrailingComments,
      indent: Indent,
      config: QueryRendererConfig,
      frontSep: Boolean = false): String =
    if (fields.nonEmpty) {
      val rendered = fields.iterator.zipWithIndex
        .map { case (field, idx) =>
          val prev = if (idx == 0) None else Some(fields(idx - 1))
          val next = if (idx == fields.size - 1) None else Some(fields(idx + 1))

          val trailingNext =
            for {
              n <- next
              c <- n.description.fold(n.comments)(_.comments).headOption
              cp <- c.location
              sp <- field.location
              if cp.line == sp.line
            } yield c

          val trailing =
            trailingNext.orElse(for {
              c <- tc.trailingComments.headOption
              cp <- c.location
              sp <- field.location
              if cp.line == sp.line
            } yield c)

          (if (idx != 0 && (shouldRenderComment(field, prev, config) || shouldRenderDescription(
               field))) config.lineBreak
           else "") +
            renderNode(field, config, indent.inc, prev = prev) +
            trailing.fold("")(c => renderIndividualComment(c, " ", config))
        }
        .mkString(config.mandatoryLineBreak)

      (if (frontSep) config.separator else "") +
        "{" +
        config.lineBreak +
        rendered +
        renderTrailingComment(tc, fields.lastOption, indent.inc, config) +
        trailingLineBreak(tc, config) +
        indent.str +
        "}"
    } else ""

  def renderInputFieldDefinitions(
      fields: Vector[InputValueDefinition],
      tc: WithTrailingComments,
      indent: Indent,
      config: QueryRendererConfig,
      frontSep: Boolean = false): String =
    if (fields.nonEmpty) {
      (if (frontSep) config.separator else "") +
        "{" +
        config.lineBreak +
        renderInputObjectFieldDefs(fields, tc, indent, config) +
        renderTrailingComment(tc, fields.lastOption, indent.inc, config) +
        trailingLineBreak(tc, config) +
        indent.str +
        "}"
    } else ""

  def renderEnumValues(
      values: Vector[EnumValueDefinition],
      tc: WithTrailingComments,
      indent: Indent,
      config: QueryRendererConfig,
      frontSep: Boolean = false): String =
    if (values.nonEmpty) {
      val renderedValues = values.iterator.zipWithIndex
        .map { case (value, idx) =>
          val prev = if (idx == 0) None else Some(values(idx - 1))
          val next = if (idx == values.size - 1) None else Some(values(idx + 1))

          val trailingNext =
            for {
              n <- next
              c <- n.description.fold(n.comments)(_.comments).headOption
              cp <- c.location
              sp <- value.location
              if cp.line == sp.line
            } yield c

          val trailing =
            trailingNext.orElse(for {
              c <- tc.trailingComments.headOption
              cp <- c.location
              sp <- value.location
              if cp.line == sp.line
            } yield c)

          (if (idx != 0 && (shouldRenderComment(value, prev, config) || shouldRenderDescription(
               value))) config.lineBreak
           else "") +
            renderNode(value, config, indent.inc, prev = prev) +
            trailing.fold("")(c => renderIndividualComment(c, " ", config))
        }
        .mkString(config.mandatoryLineBreak)

      (if (frontSep) config.separator else "") +
        "{" +
        config.lineBreak +
        renderedValues +
        renderTrailingComment(tc, values.lastOption, indent.inc, config) +
        trailingLineBreak(tc, config) +
        indent.str +
        "}"
    } else ""

  def renderOperationTypeDefinitions(
      ops: Vector[OperationTypeDefinition],
      tc: WithTrailingComments,
      indent: Indent,
      config: QueryRendererConfig,
      frontSep: Boolean = false): String =
    if (ops.nonEmpty) {
      val renderedOps = ops.iterator.zipWithIndex
        .map { case (op, idx) =>
          (if (idx != 0 && shouldRenderComment(op, None, config)) config.lineBreak else "") +
            renderNode(op, config, indent.inc)
        }
        .mkString(config.mandatoryLineBreak)

      (if (frontSep) config.separator else "") +
        "{" +
        config.lineBreak +
        renderedOps +
        renderTrailingComment(tc, None, indent.inc, config) +
        trailingLineBreak(tc, config) +
        indent.str + "}"
    } else ""

  def renderDirs(
      dirs: Vector[Directive],
      config: QueryRendererConfig,
      indent: Indent,
      frontSep: Boolean = false,
      withSep: Boolean = true): String =
    (if (dirs.nonEmpty && frontSep && withSep) config.separator else "") +
      dirs.iterator.map(renderNode(_, config, indent.zero)).mkString(config.separator) +
      (if (dirs.nonEmpty && !frontSep && withSep) config.separator else "")

  def renderArgs(
      args: Vector[Argument],
      indent: Indent,
      config: QueryRendererConfig,
      withSep: Boolean = true): String =
    if (args.nonEmpty) {
      val argsRendered = args.iterator.zipWithIndex
        .map { case (a, idx) =>
          (if (idx != 0 && shouldRenderComment(a, None, config)) config.lineBreak else "") +
            (if (shouldRenderComment(a, None, config)) config.mandatoryLineBreak
             else if (idx != 0) config.separator
             else "") +
            renderNode(
              a,
              config,
              if (shouldRenderComment(a, None, config)) indent.inc else indent.zero)
        }

      "(" + argsRendered.mkString(",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderInputValueDefs(
      args: Vector[InputValueDefinition],
      indent: Indent,
      config: QueryRendererConfig,
      withSep: Boolean = true): String =
    if (args.nonEmpty) {
      val argsRendered = args.iterator.zipWithIndex
        .map { case (a, idx) =>
          (if (idx != 0 && (shouldRenderComment(a, None, config) || shouldRenderDescription(a)))
             config.lineBreak
           else "") +
            (if (shouldRenderComment(a, None, config) || shouldRenderDescription(a))
               config.mandatoryLineBreak
             else if (idx != 0) config.separator
             else "") +
            renderNode(
              a,
              config,
              if (shouldRenderComment(a, None, config) || shouldRenderDescription(a)) indent.inc
              else indent.zero)
        }

      "(" + argsRendered.mkString(",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderVarDefs(
      vars: Vector[VariableDefinition],
      indent: Indent,
      config: QueryRendererConfig,
      withSep: Boolean = true): String =
    if (vars.nonEmpty) {
      val varsRendered = vars.iterator.zipWithIndex
        .map { case (v, idx) =>
          (if (idx != 0 && shouldRenderComment(v, None, config)) config.lineBreak else "") +
            (if (shouldRenderComment(v, None, config)) config.mandatoryLineBreak
             else if (idx != 0) config.separator
             else "") +
            renderNode(
              v,
              config,
              if (shouldRenderComment(v, None, config)) indent + 2 else indent.zero)
        }

      "(" + varsRendered.mkString(",") + ")" + (if (withSep) config.separator else "")
    } else ""

  def renderInputObjectFieldDefs(
      fields: Vector[InputValueDefinition],
      tc: WithTrailingComments,
      indent: Indent,
      config: QueryRendererConfig): String = {
    val fieldsRendered = fields.iterator.zipWithIndex
      .map { case (f, idx) =>
        val prev = if (idx == 0) None else Some(fields(idx - 1))
        val next = if (idx == fields.size - 1) None else Some(fields(idx + 1))

        val trailingNext =
          for {
            n <- next
            c <- n.description.fold(n.comments)(_.comments).headOption
            cp <- c.location
            sp <- f.location
            if cp.line == sp.line
          } yield c

        val trailing =
          trailingNext.orElse(for {
            c <- tc.trailingComments.headOption
            cp <- c.location
            sp <- f.location
            if cp.line == sp.line
          } yield c)

        (if (idx != 0 && (shouldRenderComment(f, prev, config) || shouldRenderDescription(f)))
           config.lineBreak
         else "") +
          renderNode(f, config, indent.inc, prev = prev) +
          trailing.fold("")(c => renderIndividualComment(c, " ", config))
      }

    fieldsRendered.mkString(config.mandatoryLineBreak)
  }

  def renderInterfaces(
      interfaces: Vector[NamedType],
      config: QueryRendererConfig,
      indent: Indent,
      frontSep: Boolean = false,
      withSep: Boolean = true): String =
    if (interfaces.nonEmpty)
      (if (frontSep) config.mandatorySeparator else "") +
        "implements" + config.mandatorySeparator +
        interfaces.iterator
          .map(renderNode(_, config, indent.zero))
          .mkString(config.separator + "&" + config.separator) +
        (if (withSep) config.separator else "")
    else ""

  def renderOpType(operationType: OperationType): String = operationType match {
    case OperationType.Query => "query"
    case OperationType.Mutation => "mutation"
    case OperationType.Subscription => "subscription"
  }

  def actualComments(node: WithComments, prev: Option[AstNode]): Vector[Comment] = {
    val ignoreFirst = for {
      ls <- prev
      p <- ls.location
      c <- node.comments.headOption
      cp <- c.location
    } yield cp.line == p.line

    ignoreFirst match {
      case Some(true) => node.comments.tail
      case _ => node.comments
    }
  }

  def shouldRenderComment(
      node: WithComments,
      prev: Option[AstNode],
      config: QueryRendererConfig): Boolean =
    config.renderComments && actualComments(node, prev).nonEmpty

  def shouldRenderDescription(node: WithDescription): Boolean =
    node.description.fold(false)(_.value.trim.nonEmpty)

  def shouldRenderComment(node: WithTrailingComments, config: QueryRendererConfig): Boolean =
    config.renderComments && node.trailingComments.nonEmpty

  def shouldRenderComment(comments: Vector[Comment], config: QueryRendererConfig): Boolean =
    config.renderComments && comments.nonEmpty

  private def startsWithWhitespace(text: String) =
    text.charAt(0).isWhitespace

  def renderIndividualComment(node: Comment, indent: String, config: QueryRendererConfig): String =
    indent + "#" + (if (node.text.trim.nonEmpty && !startsWithWhitespace(node.text)) " "
                    else "") + node.text

  def renderDescription(
      node: WithDescription,
      prev: Option[AstNode],
      indent: Indent,
      config: QueryRendererConfig): String =
    node.description match {
      case Some(description) =>
        renderComment(description, prev, indent, config) +
          indent.str + renderStringValue(description, indent, config, extraIndent = false) +
          config.mandatoryLineBreak
      case None => ""
    }

  def renderComment(
      node: WithComments,
      prev: Option[AstNode],
      indent: Indent,
      config: QueryRendererConfig): String = {
    val comments = actualComments(node, prev)

    if (shouldRenderComment(comments, config)) {
      val lines = renderCommentLines(comments, node.location, indent, config)

      lines.mkString("", config.mandatoryLineBreak, config.mandatoryLineBreak)
    } else ""
  }

  def renderCommentLines(
      comments: Vector[Comment],
      nodePos: Option[AstLocation],
      indent: Indent,
      config: QueryRendererConfig): Seq[String] = {
    val nodeLine =
      nodePos.map(_.line).orElse(comments.last.location.map(_.line + 1)).fold(1)(identity)

    comments
      .foldRight((nodeLine, Vector.empty[String])) { case (c, (lastLine, acc)) =>
        val currLine = c.location.fold(lastLine - 1)(_.line)
        val diffLines = lastLine - currLine
        val fill = if (diffLines > 1) config.lineBreak else ""

        currLine -> ((renderIndividualComment(c, indent.str, config) + fill) +: acc)
      }
      ._2
  }

  def renderTrailingComment(
      node: WithTrailingComments,
      lastSelection: Option[AstNode],
      indent: Indent,
      config: QueryRendererConfig): String = {
    val ignoreFirst = for {
      ls <- lastSelection
      p <- ls.location
      c <- node.trailingComments.headOption
      cp <- c.location
    } yield cp.line == p.line
    val comments = ignoreFirst match {
      case Some(true) => node.trailingComments.tail
      case _ => node.trailingComments
    }

    if (shouldRenderComment(comments, config)) {
      val lines = renderCommentLines(comments, None, indent, config)

      lines.mkString(config.lineBreak + config.mandatoryLineBreak, config.mandatoryLineBreak, "")
    } else ""
  }

  def renderInputComment(node: WithComments, indent: Indent, config: QueryRendererConfig): String =
    if (config.formatInputValues && shouldRenderComment(node, None, config))
      renderComment(node, None, indent, config) + indent.str
    else ""

  def renderStringValue(
      node: StringValue,
      indent: Indent,
      config: QueryRendererConfig,
      extraIndent: Boolean = true): String =
    if (node.block && config.formatBlockStrings)
      renderBlockString(node, indent, config, extraIndent)
    else renderNonBlockString(node, indent, config)

  def renderNonBlockString(node: StringValue, indent: Indent, config: QueryRendererConfig) =
    s""""${escapeString(node.value)}""""

  def renderBlockString(
      node: StringValue,
      indent: Indent,
      config: QueryRendererConfig,
      extraIndent: Boolean = true): String =
    if (node.value.trim.nonEmpty) {
      val ind = if (extraIndent) indent.incForce.str else indent.strForce
      val lines = escapeBlockString(node.value).linesIterator.map { line =>
        if (line.isEmpty) line // do not output lines with only whitespaces inside
        else ind + line
      }

      lines.mkString(
        "\"\"\"" + config.mandatoryLineBreak,
        config.mandatoryLineBreak,
        config.mandatoryLineBreak + ind + "\"\"\"")
    } else "\"\""

  def render(node: AstNode, config: QueryRendererConfig = Pretty, indentLevel: Int = 0): String =
    renderNode(node, config, Indent(config, indentLevel, indentLevel))

  def renderPretty(node: AstNode): String = node match {
    case _: Value => render(node, PrettyInput)
    case _ => render(node, Pretty)
  }

  def renderCompact(node: AstNode): String = render(node, Compact)

  def renderNode(
      node: AstNode,
      config: QueryRendererConfig,
      indent: Indent,
      prefix: Option[String] = None,
      prev: Option[AstNode] = None): String =
    node match {
      case d @ Document(defs, _, _, _) =>
        defs.iterator
          .map(renderNode(_, config, indent))
          .mkString(config.definitionSeparator) +
          renderTrailingComment(d, None, indent, config)

      case d @ InputDocument(defs, _, _, _) =>
        defs.iterator
          .map(renderNode(_, config, indent))
          .mkString(config.definitionSeparator) +
          renderTrailingComment(d, None, indent, config)

      case op @ OperationDefinition(OperationType.Query, None, vars, dirs, sels, _, _, _)
          if vars.isEmpty && dirs.isEmpty =>
        renderComment(op, prev, indent, config) + indent.str + renderSelections(
          sels,
          op,
          indent,
          config)

      case op @ OperationDefinition(opType, name, vars, dirs, sels, _, _, _) =>
        renderComment(op, prev, indent, config) +
          indent.str + renderOpType(opType) + config.mandatorySeparator +
          name.getOrElse("") +
          renderVarDefs(vars, indent, config, withSep = false) +
          config.separator +
          renderDirs(dirs, config, indent) +
          renderSelections(sels, op, indent, config)

      case fd @ FragmentDefinition(name, typeCondition, dirs, sels, vars, _, _, _) =>
        renderComment(fd, prev, indent, config) +
          indent.str + "fragment" + config.mandatorySeparator + name + renderVarDefs(
            vars,
            indent,
            config,
            withSep = false) + config.mandatorySeparator + "on" +
          config.mandatorySeparator + typeCondition.name + config.separator +
          renderDirs(dirs, config, indent) +
          renderSelections(sels, fd, indent, config)

      case vd @ VariableDefinition(name, tpe, defaultValue, dirs, _, _) =>
        renderComment(vd, prev, indent, config) +
          indent.str + "$" + name + ":" + config.separator +
          renderNode(tpe, config, indent.zero) +
          defaultValue
            .map(v =>
              config.separator + "=" + config.separator + renderNode(v, config, indent.zero))
            .getOrElse("") +
          renderDirs(dirs, config, indent, frontSep = true)

      case NotNullType(ofType, _) =>
        renderNode(ofType, config, indent.zero) + "!"

      case ListType(ofType, _) =>
        "[" + renderNode(ofType, config, indent.zero) + "]"

      case NamedType(name, _) =>
        name

      case f @ Field(alias, name, args, dirs, sels, _, _, _) =>
        renderComment(f, prev, indent, config) +
          indent.str + alias.map(_ + ":" + config.separator).getOrElse("") + name +
          renderArgs(args, indent, config, withSep = false) +
          (if (dirs.nonEmpty || sels.nonEmpty) config.separator else "") +
          renderDirs(dirs, config, indent, withSep = sels.nonEmpty) +
          renderSelections(sels, f, indent, config)

      case fs @ FragmentSpread(name, dirs, _, _) =>
        renderComment(fs, prev, indent, config) +
          indent.str + "..." + name + renderDirs(dirs, config, indent, frontSep = true)

      case ifr @ InlineFragment(typeCondition, dirs, sels, _, _, _) =>
        renderComment(ifr, prev, indent, config) +
          indent.str + "..." + config.mandatorySeparator + typeCondition.fold("")(
            "on" + config.mandatorySeparator + _.name) + config.separator +
          renderDirs(dirs, config, indent) +
          renderSelections(sels, ifr, indent, config)

      case Directive(name, args, _, _) =>
        indent.str + "@" + name + renderArgs(
          args,
          indent,
          config.copy(renderComments = false),
          withSep = false)

      case a @ Argument(name, value, _, _) =>
        renderComment(a, prev, indent, config) +
          indent.str + name + ":" + config.separator + renderNode(value, config, indent.zero)

      case v @ IntValue(value, _, _) => renderInputComment(v, indent, config) + value
      case v @ BigIntValue(value, _, _) => renderInputComment(v, indent, config) + value
      case v @ FloatValue(value, _, _) => renderInputComment(v, indent, config) + value
      case v @ BigDecimalValue(value, _, _) => renderInputComment(v, indent, config) + value
      case v @ BooleanValue(value, _, _) => renderInputComment(v, indent, config) + value
      case v @ NullValue(_, _) => renderInputComment(v, indent, config) + "null"
      case v @ EnumValue(value, _, _) => renderInputComment(v, indent, config) + value
      case v @ StringValue(_, _, _, _, _) =>
        renderInputComment(v, indent, config) + renderStringValue(v, indent, config)

      case v @ ListValue(value, _, _) =>
        def addIdent(v: Value) = v match {
          case _: ObjectValue => false
          case _ => true
        }

        def renderValue(v: Value, idx: Int) =
          if (config.formatInputValues && shouldRenderComment(v, None, config))
            (if (idx != 0) config.lineBreak else "") +
              config.lineBreak +
              renderNode(v, config, indent + (if (addIdent(v)) 1 else 0))
          else
            (if (idx != 0) config.separator else "") + renderNode(v, config, indent)

        renderInputComment(v, indent, config) +
          "[" + value.iterator.zipWithIndex
            .map { case (v, idx) => renderValue(v, idx) }
            .mkString(config.inputListSeparator) + "]"
      case v @ ObjectValue(value, _, _) =>
        renderInputComment(v, indent, config) +
          "{" + inputLineBreak(config) +
          value.iterator.zipWithIndex
            .map { case (v, idx) =>
              (if (idx != 0 && config.formatInputValues && shouldRenderComment(v, None, config))
                 config.lineBreak
               else "") + renderNode(v, config, inputFieldIndent(config, indent))
            }
            .mkString(config.inputFieldSeparator) +
          inputLineBreak(config) + inputIndent(config, indent) + "}"
      case VariableValue(name, _, _) => indent.str + "$" + name
      case v @ ObjectField(name, value, _, _) =>
        val rendered =
          if (config.formatInputValues && shouldRenderComment(value, None, config))
            config.lineBreak + renderNode(value, config, indent.inc)
          else
            config.separator + renderNode(value, config, indent)

        (if (config.formatInputValues) renderComment(v, prev, indent, config) else "") +
          indent.str + name + ":" + rendered

      case c @ Comment(_, _) => renderIndividualComment(c, indent.str, config)

      case std @ ScalarTypeDefinition(name, dirs, description, _, _) =>
        renderDescription(std, prev, indent, config) +
          renderComment(std, description.orElse(prev), indent, config) +
          indent.str + "scalar" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true)

      case otd @ ObjectTypeDefinition(name, interfaces, fields, dirs, description, _, _, _) =>
        renderDescription(otd, prev, indent, config) +
          renderComment(otd, description.orElse(prev), indent, config) +
          indent.str + prefix.getOrElse("") + "type" + config.mandatorySeparator + name +
          config.mandatorySeparator +
          renderInterfaces(interfaces, config, indent) +
          renderDirs(dirs, config, indent, withSep = fields.nonEmpty) +
          renderFieldDefinitions(fields, otd, indent, config)

      case itd @ InputObjectTypeDefinition(name, fields, dirs, description, _, _, _) =>
        renderDescription(itd, prev, indent, config) +
          renderComment(itd, description.orElse(prev), indent, config) +
          indent.str + "input" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true) +
          renderInputFieldDefinitions(fields, itd, indent, config, frontSep = true)

      case itd @ InterfaceTypeDefinition(name, fields, interfaces, dirs, description, _, _, _) =>
        renderDescription(itd, prev, indent, config) +
          renderComment(itd, description.orElse(prev), indent, config) +
          indent.str + "interface" + config.mandatorySeparator + name +
          (if (interfaces.nonEmpty) config.mandatorySeparator else "") +
          renderInterfaces(interfaces, config, indent, withSep = false) +
          (if (dirs.nonEmpty) config.separator else "") +
          renderDirs(dirs, config, indent, withSep = false) +
          (if (fields.nonEmpty) config.separator else "") +
          renderFieldDefinitions(fields, itd, indent, config)

      case utd @ UnionTypeDefinition(name, types, dirs, description, _, _) =>
        val typesString =
          if (types.nonEmpty)
            config.separator + "=" + config.separator +
              types.iterator
                .map(renderNode(_, config, indent.zero))
                .mkString(config.separator + "|" + config.separator)
          else
            ""
        renderDescription(utd, prev, indent, config) +
          renderComment(utd, description.orElse(prev), indent, config) +
          indent.str + "union" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true) +
          typesString

      case etd @ EnumTypeDefinition(name, values, dirs, description, _, _, _) =>
        renderDescription(etd, prev, indent, config) +
          renderComment(etd, description.orElse(prev), indent, config) +
          indent.str + "enum" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true) +
          renderEnumValues(values, etd, indent, config, frontSep = true)

      case evd @ EnumValueDefinition(name, dirs, description, _, _) =>
        renderDescription(evd, prev, indent, config) +
          renderComment(evd, description.orElse(prev), indent, config) +
          indent.str + name +
          renderDirs(dirs, config, indent, frontSep = true)

      case fd @ FieldDefinition(name, fieldType, args, dirs, description, _, _) =>
        renderDescription(fd, prev, indent, config) +
          renderComment(fd, description.orElse(prev), indent, config) +
          indent.str + name +
          renderInputValueDefs(args, indent, config, withSep = false) +
          ":" + config.separator + renderNode(fieldType, config, indent.zero) +
          renderDirs(dirs, config, indent, frontSep = true)

      case ivd @ InputValueDefinition(name, valueType, default, dirs, description, _, _) =>
        renderDescription(ivd, prev, indent, config) +
          renderComment(ivd, description.orElse(prev), indent, config) +
          indent.str + name + ":" + config.separator + renderNode(valueType, config, indent.zero) +
          default.fold("")(d =>
            config.separator + "=" + config.separator + renderNode(d, config, indent.zero)) +
          renderDirs(dirs, config, indent, frontSep = true)

      case ted @ ObjectTypeExtensionDefinition(name, interfaces, fields, dirs, _, _, _) =>
        renderComment(ted, prev, indent, config) +
          indent.str + prefix.getOrElse(
            "") + "extend" + config.mandatorySeparator + "type" + config.mandatorySeparator + name +
          config.mandatorySeparator +
          renderInterfaces(interfaces, config, indent) +
          renderDirs(dirs, config, indent, withSep = fields.nonEmpty) +
          renderFieldDefinitions(fields, ted, indent, config)

      case ext @ InterfaceTypeExtensionDefinition(name, interfaces, fields, dirs, _, _, _) =>
        renderComment(ext, prev, indent, config) +
          indent.str + "extend" + config.mandatorySeparator + "interface" + config.mandatorySeparator + name +
          (if (interfaces.nonEmpty) config.mandatorySeparator else "") +
          renderInterfaces(interfaces, config, indent, withSep = false) +
          (if (dirs.nonEmpty) config.separator else "") +
          renderDirs(dirs, config, indent, withSep = false) +
          (if (fields.nonEmpty) config.separator else "") +
          renderFieldDefinitions(fields, ext, indent, config)

      case ext @ UnionTypeExtensionDefinition(name, types, dirs, _, _) =>
        val typesString =
          if (types.nonEmpty)
            config.separator + "=" + config.separator +
              types.iterator
                .map(renderNode(_, config, indent.zero))
                .mkString(config.separator + "|" + config.separator)
          else
            ""

        renderComment(ext, prev, indent, config) +
          indent.str + "extend" + config.mandatorySeparator + "union" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true) +
          typesString

      case ext @ InputObjectTypeExtensionDefinition(name, fields, dirs, _, _, _) =>
        renderComment(ext, prev, indent, config) +
          indent.str + "extend" + config.mandatorySeparator + "input" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true) +
          renderInputFieldDefinitions(fields, ext, indent, config, frontSep = true)

      case ext @ EnumTypeExtensionDefinition(name, values, dirs, _, _, _) =>
        renderComment(ext, prev, indent, config) +
          indent.str + "extend" + config.mandatorySeparator + "enum" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true) +
          renderEnumValues(values, ext, indent, config, frontSep = true)

      case ext @ ScalarTypeExtensionDefinition(name, dirs, _, _) =>
        renderComment(ext, prev, indent, config) +
          indent.str + "extend" + config.mandatorySeparator + "scalar" + config.mandatorySeparator + name +
          renderDirs(dirs, config, indent, frontSep = true)

      case ext @ SchemaExtensionDefinition(ops, dirs, _, _, _) =>
        renderComment(ext, prev, indent, config) +
          indent.str + "extend" + config.mandatorySeparator + "schema" +
          renderDirs(dirs, config, indent, frontSep = true) +
          renderOperationTypeDefinitions(ops, ext, indent, config, frontSep = true)

      case dd @ DirectiveDefinition(name, args, locations, description, rep, _, _) =>
        val locsRendered = locations.iterator.zipWithIndex
          .map { case (l, idx) =>
            (if (idx != 0 && shouldRenderComment(l, None, config)) config.lineBreak else "") +
              (if (shouldRenderComment(l, None, config)) config.lineBreak
               else if (idx != 0) config.separator
               else "") +
              renderNode(
                l,
                config,
                if (shouldRenderComment(l, None, config)) indent.inc else indent.zero)
          }

        renderDescription(dd, prev, indent, config) +
          renderComment(dd, description.orElse(prev), indent, config) +
          indent.str + "directive" + config.separator + "@" + name +
          renderInputValueDefs(args, indent, config) + (if (args.isEmpty) config.mandatorySeparator
                                                        else "") +
          (if (rep) "repeatable" + config.mandatorySeparator else "") +
          "on" + (if (shouldRenderComment(locations.head, None, config)) ""
                  else config.mandatorySeparator) +
          locsRendered.mkString(config.separator + "|")

      case dl @ DirectiveLocation(name, _, _) =>
        renderComment(dl, prev, indent, config) + indent.str + name

      case sd @ SchemaDefinition(ops, dirs, description, _, _, _) =>
        renderDescription(sd, prev, indent, config) +
          renderComment(sd, description.orElse(prev), indent, config) +
          indent.str + "schema" + config.separator +
          renderDirs(dirs, config, indent) +
          renderOperationTypeDefinitions(ops, sd, indent, config)

      case otd @ OperationTypeDefinition(op, tpe, _, _) =>
        renderComment(otd, prev, indent, config) +
          indent.str + renderOpType(op) + ":" + config.separator + renderNode(
            tpe,
            config,
            indent.zero)
    }

  private def trailingLineBreak(tc: WithTrailingComments, config: QueryRendererConfig) =
    if (shouldRenderComment(tc, config)) config.mandatoryLineBreak else config.lineBreak

  def inputLineBreak(config: QueryRendererConfig): String =
    if (config.formatInputValues) config.lineBreak
    else ""

  def inputFieldIndent(config: QueryRendererConfig, indent: Indent): Indent =
    if (config.formatInputValues) indent.inc
    else indent.zero

  def inputIndent(config: QueryRendererConfig, indent: Indent): String =
    if (config.formatInputValues) indent.str
    else ""
}

case class Indent(config: QueryRendererConfig, level: Int, prevLevel: Int) {
  lazy val str: String = config.indentLevel * level
  lazy val strPrev: String = config.indentLevel * prevLevel

  def strForce: String = if (level > 0) str else strPrev

  lazy val zero: Indent = copy(level = 0, prevLevel = if (level == 0) prevLevel else level)

  def +(l: Int): Indent = copy(level = level + l, prevLevel = level)
  def -(l: Int): Indent = {
    val newLevel = level - l

    if (newLevel >= 0) this
    else copy(level = level + l, prevLevel = level)
  }

  def inc: Indent = this + 1
  def dec: Indent = this - 1

  def incForce: Indent =
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
