package sangria.parser

import org.parboiled2.CharPredicate.{Digit19, HexDigit}
import org.parboiled2._
import sangria.ast
import shapeless.{::, HNil}

import scala.util.{Failure, Success, Try}

sealed trait Tokens extends StringBuilding with PositionTracking { this: Parser with Ignored =>

  private[this] def leadingWhitespace(str: String) = {
    var i = 0
    while (i < str.length && (str.charAt(i) == ' ' || str.charAt(i) == '\t')) i += 1
    i
  }

  private[this] def isBlank(str: String) = leadingWhitespace(str) == str.length

  /** Produces the value of a block string from its parsed raw value, similar to Coffeescript's
    * block string, Python's docstring trim or Ruby's strip_heredoc.
    *
    * This implements the GraphQL spec's BlockStringValue() static algorithm.
    */
  private[this] def blockStringValue(rawString: String): String = {
    val lines = rawString.split("""\r\n|[\n\r]""")
    val lineSizes = lines.map(l => l -> leadingWhitespace(l))
    val commonIndentLines =
      lineSizes.drop(1).collect { case (line, size) if size != line.length => size }
    val strippedLines =
      if (commonIndentLines.nonEmpty) {
        val commonIndent = commonIndentLines.min

        lines.take(1) ++ lines.drop(1).map(_.drop(commonIndent))
      } else lines
    val trimmedLines = strippedLines.reverse.dropWhile(isBlank).reverse.dropWhile(isBlank)

    trimmedLines.mkString("\n")
  }

  protected[this] def Ellipsis: Rule[HNil, HNil] = rule(quiet(str("...") ~ IgnoredNoComment.*))

  private[this] val NameFirstChar = CharPredicate.Alpha ++ '_'

  private[this] val NameChar = NameFirstChar ++ CharPredicate.Digit

  protected[this] def NameStrict: Rule[HNil, String :: HNil] = rule(
    capture(NameFirstChar ~ NameChar.*) ~ IgnoredNoComment.*)

  protected[this] def Name: Rule[HNil, String :: HNil] = rule(Ignored.* ~ NameStrict)

  protected[this] def NumberValue = rule {
    atomic(Comments ~ trackPos ~ IntegerValuePart ~ FloatValuePart.? ~ IgnoredNoComment.*) ~>
      ((comment, location, intPart, floatPart) =>
        floatPart
          .map(f => ast.BigDecimalValue(BigDecimal(intPart + f), comment, location))
          .getOrElse(ast.BigIntValue(BigInt(intPart), comment, location)))
  }

  private[this] def FloatValuePart = rule(
    atomic(capture(FractionalPart ~ ExponentPart.? | ExponentPart)))

  private[this] def FractionalPart = rule('.' ~ Digit.+)

  private[this] def IntegerValuePart = rule(capture(NegativeSign.? ~ IntegerPart))

  private[this] def IntegerPart = rule(ch('0') | NonZeroDigit ~ Digit.*)

  private[this] def ExponentPart = rule(ExponentIndicator ~ Sign.? ~ Digit.+)

  private[this] def ExponentIndicator = rule(ch('e') | ch('E'))

  private[this] def Sign = rule(ch('-') | '+')

  private[this] val NegativeSign = '-'

  private[this] val NonZeroDigit = Digit19

  private[this] def Digit = rule(ch('0') | NonZeroDigit)

  protected[this] def StringValue: Rule[HNil, ast.StringValue :: HNil] = rule(
    BlockStringValue | NonBlockStringValue)

  private[this] def BlockStringValue = rule {
    Comments ~ trackPos ~ BlockString ~ clearSB() ~ BlockStringCharacters ~ BlockString ~ push(
      sb.toString) ~ IgnoredNoComment.* ~>
      ((comment, location, s) =>
        ast.StringValue(blockStringValue(s), true, Some(s), comment, location))
  }

  private[this] def BlockStringCharacters = rule((BlockStringCharacter | BlockStringEscapedChar).*)

  private[this] def BlockString = rule(str("\"\"\""))

  private[this] def QuotedBlockString = rule(str("\\\"\"\""))

  private[this] def BlockStringCharacter = rule {
    !(QuotedBlockString | BlockString) ~ ((CRLF | LineTerminator) ~ trackNewLine | ANY) ~ appendSB()
  }

  private[this] def BlockStringEscapedChar = rule {
    QuotedBlockString ~ appendSB("\"\"\"")
  }

  private[this] def NormalCharacter = rule(!(QuoteBackslash | LineTerminator) ~ ANY ~ appendSB())

  private[this] def NonBlockStringValue = rule {
    Comments ~ trackPos ~ '"' ~ clearSB() ~ Characters ~ '"' ~ push(
      sb.toString) ~ IgnoredNoComment.* ~>
      ((comment, location, s) => ast.StringValue(s, false, None, comment, location))
  }

  private[this] def Characters = rule((NormalCharacter | '\\' ~ EscapedChar).*)

  private[this] val QuoteBackslash = CharPredicate("\"\\")

  private[this] def EscapedChar = rule {
    QuoteBackslash ~ appendSB() |
      'b' ~ appendSB('\b') |
      'f' ~ appendSB('\f') |
      'n' ~ appendSB('\n') |
      'r' ~ appendSB('\r') |
      't' ~ appendSB('\t') |
      Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
  }

  private[this] def Unicode = rule('u' ~ capture(4.times(HexDigit)) ~> (Integer.parseInt(_, 16)))

  protected[this] def Keyword(s: String): Rule[HNil, HNil] = rule(
    atomic(Ignored.* ~ s ~ !NameChar ~ IgnoredNoComment.*))
}

/** Mix-in that defines GraphQL grammar productions that are typically ignored (whitespace,
  * comments, etc.).
  */
sealed trait Ignored extends PositionTracking { this: Parser =>

  /** Whether comments should be parsed into the resulting AST. */
  protected[this] def parseComments: Boolean

  private[this] val WhiteSpace = CharPredicate("\u0009\u0020")

  protected[this] def CRLF: Rule[HNil, HNil] = rule('\u000D' ~ '\u000A')

  protected[this] val LineTerminator: CharPredicate = CharPredicate("\u000A")

  private[this] val UnicodeBOM = CharPredicate('\uFEFF')

  protected[this] def Ignored: Rule[HNil, HNil] = rule {
    quiet(UnicodeBOM | WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | Comment | ',')
  }

  protected[this] def IgnoredNoComment: Rule[HNil, HNil] = rule {
    quiet(UnicodeBOM | WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | ',')
  }

  protected[this] def Comments: Rule[HNil, Vector[ast.Comment] :: HNil] = rule {
    test(
      parseComments) ~ CommentCap.* ~ Ignored.* ~> (_.toVector) | CommentNoCap.* ~ Ignored.* ~ push(
      Vector.empty)
  }

  private[this] def CommentCap = rule {
    trackPos ~ "#" ~ capture(CommentChar.*) ~ IgnoredNoComment.* ~> ((location, comment) =>
      ast.Comment(comment, location))
  }

  private[this] def CommentNoCap: Rule0 = rule("#" ~ CommentChar.* ~ IgnoredNoComment.*)

  private[this] def Comment = rule("#" ~ CommentChar.*)

  private[this] def CommentChar = rule(!(CRLF | LineTerminator) ~ ANY)

  protected[this] def ws(char: Char): Rule0 = rule(quiet(Ignored.* ~ ch(char) ~ Ignored.*))

  protected[this] def wsNoComment(char: Char): Rule0 = rule(
    quiet(Ignored.* ~ ch(char) ~ IgnoredNoComment.*))

  protected[this] def wsCapture(s: String): Rule[HNil, String :: HNil] = rule(
    quiet(Ignored.* ~ capture(str(s)) ~ IgnoredNoComment.*))
}

sealed trait Document {
  this: Parser
    with Operations
    with Ignored
    with Fragments
    with Operations
    with Values
    with TypeSystemDefinitions =>

  protected def Document = rule {
    IgnoredNoComment.* ~ trackPos ~ Definition.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~>
      ((location, d, comments) => ast.Document(d.toVector, comments, location))
  }

  protected def InputDocument = rule {
    IgnoredNoComment.* ~ trackPos ~ ValueConst.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~>
      ((location, vs, comments) => ast.InputDocument(vs.toVector, comments, location))
  }

  protected def InputDocumentWithVariables = rule {
    IgnoredNoComment.* ~ trackPos ~ Value.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~>
      ((location, vs, comments) => ast.InputDocument(vs.toVector, comments, location))
  }

  private[this] def Definition = rule {
    ExecutableDefinition |
      TypeSystemDefinition |
      TypeSystemExtension
  }

  private[this] def ExecutableDefinition = rule {
    OperationDefinition |
      FragmentDefinition
  }
}

sealed trait TypeSystemDefinitions {
  this: Parser
    with Tokens
    with Ignored
    with Directives
    with Types
    with Operations
    with Values
    with Fragments =>

  private[this] def scalar = rule(Keyword("scalar"))
  private[this] def `type` = rule(Keyword("type"))
  private[this] def interface = rule(Keyword("interface"))
  private[this] def union = rule(Keyword("union"))
  private[this] def `enum` = rule(Keyword("enum"))
  private[this] def inputType = rule(Keyword("input"))
  private[this] def implements = rule(Keyword("implements"))
  private[this] def extend = rule(Keyword("extend"))
  private[this] def directive = rule(Keyword("directive"))
  private[this] def schema = rule(Keyword("schema"))

  protected[this] def TypeSystemDefinition = rule {
    SchemaDefinition |
      TypeDefinition |
      DirectiveDefinition
  }

  private[this] def TypeDefinition = rule {
    ScalarTypeDefinition |
      ObjectTypeDefinition |
      InterfaceTypeDefinition |
      UnionTypeDefinition |
      EnumTypeDefinition |
      InputObjectTypeDefinition
  }

  private[this] def ScalarTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ scalar ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~> ((descr, comment, location, name, dirs) =>
      ast.ScalarTypeDefinition(name, dirs, descr, comment, location))
  }

  private[this] def ObjectTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_.getOrElse(
      Vector.empty))) ~ (DirectivesConst.? ~> (_.getOrElse(Vector.empty))) ~ FieldsDefinition.? ~> (
      (descr, comment, location, name, interfaces, dirs, fields) =>
        ast.ObjectTypeDefinition(
          name,
          interfaces,
          fields.fold(Vector.empty[ast.FieldDefinition])(_._1.toVector),
          dirs,
          descr,
          comment,
          fields.fold(Vector.empty[ast.Comment])(_._2),
          location
        ))
  }

  protected[this] def TypeSystemExtension = rule {
    SchemaExtension |
      TypeExtension
  }

  private[this] def TypeExtension = rule {
    ScalarTypeExtensionDefinition |
      ObjectTypeExtensionDefinition |
      InterfaceTypeExtensionDefinition |
      UnionTypeExtensionDefinition |
      EnumTypeExtensionDefinition |
      InputObjectTypeExtensionDefinition
  }

  private[this] def SchemaExtension = rule {
    (Comments ~ trackPos ~ extend ~ schema ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ wsNoComment('{') ~ OperationTypeDefinition.+ ~ Comments ~ wsNoComment(
      '}') ~> ((comment, location, dirs, ops, tc) =>
      ast.SchemaExtensionDefinition(ops.toVector, dirs, comment, tc, location))) |
      (Comments ~ trackPos ~ extend ~ schema ~ DirectivesConst ~> ((comment, location, dirs) =>
        ast.SchemaExtensionDefinition(Vector.empty, dirs, comment, Vector.empty, location)))
  }

  private[this] def ObjectTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_.getOrElse(
      Vector.empty))) ~ (DirectivesConst.? ~> (_.getOrElse(Vector.empty))) ~ FieldsDefinition ~> (
      (comment, location, name, interfaces, dirs, fields) =>
        ast.ObjectTypeExtensionDefinition(
          name,
          interfaces,
          fields._1.toVector,
          dirs,
          comment,
          fields._2,
          location))) |
      (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_.getOrElse(
        Vector.empty))) ~ DirectivesConst ~> ((comment, location, name, interfaces, dirs) =>
        ast.ObjectTypeExtensionDefinition(
          name,
          interfaces,
          Vector.empty,
          dirs,
          comment,
          Vector.empty,
          location))) |
      (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ ImplementsInterfaces ~> (
        (comment, location, name, interfaces) =>
          ast.ObjectTypeExtensionDefinition(
            name,
            interfaces,
            Vector.empty,
            Vector.empty,
            comment,
            Vector.empty,
            location)))
  }

  private[this] def InterfaceTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ interface ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ FieldsDefinition ~> ((comment, location, name, dirs, fields) =>
      ast.InterfaceTypeExtensionDefinition(
        name,
        fields._1.toVector,
        dirs,
        comment,
        fields._2,
        location))) |
      (Comments ~ trackPos ~ extend ~ interface ~ Name ~ DirectivesConst ~> (
        (comment, location, name, dirs) =>
          ast.InterfaceTypeExtensionDefinition(
            name,
            Vector.empty,
            dirs,
            comment,
            Vector.empty,
            location)))
  }

  private[this] def UnionTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ union ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ UnionMemberTypes ~> ((comment, location, name, dirs, types) =>
      ast.UnionTypeExtensionDefinition(name, types, dirs, comment, location))) |
      (Comments ~ trackPos ~ extend ~ union ~ Name ~ DirectivesConst ~> (
        (comment, location, name, dirs) =>
          ast.UnionTypeExtensionDefinition(name, Vector.empty, dirs, comment, location)))
  }

  private[this] def EnumTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ enum ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ EnumValuesDefinition ~> ((comment, location, name, dirs, values) =>
      ast.EnumTypeExtensionDefinition(
        name,
        values._1.toVector,
        dirs,
        comment,
        values._2,
        location))) |
      (Comments ~ trackPos ~ extend ~ enum ~ Name ~ DirectivesConst ~> (
        (comment, location, name, dirs) =>
          ast.EnumTypeExtensionDefinition(
            name,
            Vector.empty,
            dirs,
            comment,
            Vector.empty,
            location)))
  }

  private[this] def InputObjectTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ inputType ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ InputFieldsDefinition ~> ((comment, location, name, dirs, fields) =>
      ast.InputObjectTypeExtensionDefinition(
        name,
        fields._1.toVector,
        dirs,
        comment,
        fields._2,
        location))) |
      (Comments ~ trackPos ~ extend ~ inputType ~ Name ~ DirectivesConst ~> (
        (comment, location, name, dirs) =>
          ast.InputObjectTypeExtensionDefinition(
            name,
            Vector.empty,
            dirs,
            comment,
            Vector.empty,
            location)))
  }

  private[this] def ScalarTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ scalar ~ Name ~ DirectivesConst ~> (
      (comment, location, name, dirs) =>
        ast.ScalarTypeExtensionDefinition(name, dirs, comment, location)))
  }

  private[this] def ImplementsInterfaces = rule {
    implements ~ ws('&').? ~ NamedType.+(ws('&')) ~> (_.toVector)
  }

  private[this] def FieldsDefinition = rule {
    wsNoComment('{') ~ FieldDefinition.+ ~ Comments ~ wsNoComment('}') ~> (_ -> _)
  }

  private[this] def FieldDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ (ArgumentsDefinition.? ~> (_.getOrElse(
      Vector.empty))) ~ ws(':') ~ Type ~ (Directives.? ~> (_.getOrElse(Vector.empty))) ~> (
      (descr, comment, location, name, args, fieldType, dirs) =>
        ast.FieldDefinition(name, fieldType, args, dirs, descr, comment, location))
  }

  private[this] def ArgumentsDefinition = rule {
    wsNoComment('(') ~ InputValueDefinition.+ ~ wsNoComment(')') ~> (_.toVector)
  }

  private[this] def InputValueDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ ws(
      ':') ~ Type ~ DefaultValue.? ~ (DirectivesConst.? ~> (_.getOrElse(Vector.empty))) ~> (
      (descr, comment, location, name, valueType, default, dirs) =>
        ast.InputValueDefinition(name, valueType, default, dirs, descr, comment, location))
  }

  private[this] def InterfaceTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ interface ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ FieldsDefinition.? ~> ((descr, comment, location, name, dirs, fields) =>
      ast.InterfaceTypeDefinition(
        name,
        fields.fold(Vector.empty[ast.FieldDefinition])(_._1.toVector),
        dirs,
        descr,
        comment,
        fields.fold(Vector.empty[ast.Comment])(_._2),
        location))
  }

  private[this] def UnionTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ union ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ (UnionMemberTypes.? ~> (_.getOrElse(Vector.empty))) ~> (
      (descr, comment, location, name, dirs, members) =>
        ast.UnionTypeDefinition(name, members, dirs, descr, comment, location))
  }

  private[this] def UnionMemberTypes = rule(wsNoComment('=') ~ UnionMembers)

  private[this] def UnionMembers = rule(ws('|').? ~ NamedType.+(ws('|')) ~> (_.toVector))

  private[this] def EnumTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ enum ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ EnumValuesDefinition.? ~> ((descr, comment, location, name, dirs, values) =>
      ast.EnumTypeDefinition(
        name,
        values.fold(Vector.empty[ast.EnumValueDefinition])(_._1.toVector),
        dirs,
        descr,
        comment,
        values.fold(Vector.empty[ast.Comment])(_._2),
        location))
  }

  private[this] def EnumValuesDefinition = rule {
    wsNoComment('{') ~ EnumValueDefinition.+ ~ Comments ~ wsNoComment('}') ~> (_ -> _)
  }

  private[this] def EnumValueDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~> ((descr, comments, location, name, dirs) =>
      ast.EnumValueDefinition(name, dirs, descr, comments, location))
  }

  private[this] def InputObjectTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ inputType ~ Name ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ InputFieldsDefinition.? ~> (
      (descr, comment, location, name, dirs, fields) =>
        ast.InputObjectTypeDefinition(
          name,
          fields.fold(Vector.empty[ast.InputValueDefinition])(_._1.toVector),
          dirs,
          descr,
          comment,
          fields.fold(Vector.empty[ast.Comment])(_._2),
          location))
  }

  private[this] def InputFieldsDefinition = rule {
    wsNoComment('{') ~ InputValueDefinition.+ ~ Comments ~ wsNoComment('}') ~> (_ -> _)
  }

  private[this] def DirectiveDefinition = rule {
    Description ~ Comments ~ trackPos ~ directive ~ '@' ~ NameStrict ~ (ArgumentsDefinition.? ~> (_.getOrElse(
      Vector.empty))) ~ on ~ DirectiveLocations ~> (
      (descr, comment, location, name, args, locations) =>
        ast.DirectiveDefinition(name, args, locations, descr, comment, location))
  }

  private[this] def DirectiveLocations = rule {
    ws('|').? ~ DirectiveLocation.+(wsNoComment('|')) ~> (_.toVector)
  }

  private[this] def DirectiveLocation = rule {
    Comments ~ trackPos ~ DirectiveLocationName ~> ((comment, location, name) =>
      ast.DirectiveLocation(name, comment, location))
  }

  private[this] def DirectiveLocationName = rule {
    TypeSystemDirectiveLocation | ExecutableDirectiveLocation
  }

  private[this] def ExecutableDirectiveLocation = rule {
    wsCapture("QUERY") |
      wsCapture("MUTATION") |
      wsCapture("SUBSCRIPTION") |
      wsCapture("FIELD") |
      wsCapture("FRAGMENT_DEFINITION") |
      wsCapture("FRAGMENT_SPREAD") |
      wsCapture("INLINE_FRAGMENT")
  }

  private[this] def TypeSystemDirectiveLocation = rule {
    wsCapture("SCHEMA") |
      wsCapture("SCALAR") |
      wsCapture("OBJECT") |
      wsCapture("FIELD_DEFINITION") |
      wsCapture("ARGUMENT_DEFINITION") |
      wsCapture("INTERFACE") |
      wsCapture("UNION") |
      wsCapture("ENUM_VALUE") |
      wsCapture("ENUM") |
      wsCapture("INPUT_OBJECT") |
      wsCapture("INPUT_FIELD_DEFINITION") |
      wsCapture("VARIABLE_DEFINITION")
  }

  private[this] def SchemaDefinition = rule {
    Description ~ Comments ~ trackPos ~ schema ~ (DirectivesConst.? ~> (_.getOrElse(
      Vector.empty))) ~ wsNoComment('{') ~ OperationTypeDefinition.+ ~ Comments ~ wsNoComment(
      '}') ~> ((descr, comment, location, dirs, ops, tc) =>
      ast.SchemaDefinition(ops.toVector, dirs, descr, comment, tc, location))
  }

  private[this] def OperationTypeDefinition = rule {
    Comments ~ trackPos ~ OperationType ~ ws(':') ~ NamedType ~> (
      (comment, location, opType, tpe) =>
        ast.OperationTypeDefinition(opType, tpe, comment, location))
  }

  private[this] def Description = rule(StringValue.?)
}

sealed trait Operations extends PositionTracking {
  this: Parser with Tokens with Ignored with Fragments with Values with Types with Directives =>

  protected[this] def OperationDefinition = rule {
    Comments ~ trackPos ~ SelectionSet ~> ((comment, location, s) =>
      ast.OperationDefinition(
        selections = s._1,
        comments = comment,
        trailingComments = s._2,
        location = location)) |
      Comments ~ trackPos ~ OperationType ~ OperationName.? ~ (VariableDefinitions.? ~> (_.getOrElse(
        Vector.empty))) ~ (Directives.? ~> (_.getOrElse(Vector.empty))) ~ SelectionSet ~>
      ((comment, location, opType, name, vars, dirs, sels) =>
        ast.OperationDefinition(opType, name, vars, dirs, sels._1, comment, sels._2, location))
  }

  private[this] def OperationName = rule(Name)

  protected[this] def OperationType = rule {
    Query ~ push(ast.OperationType.Query) |
      Mutation ~ push(ast.OperationType.Mutation) |
      Subscription ~ push(ast.OperationType.Subscription)
  }

  private[this] def Query = rule(Keyword("query"))
  private[this] def Mutation = rule(Keyword("mutation"))
  private[this] def Subscription = rule(Keyword("subscription"))

  protected[this] def VariableDefinitions = rule {
    wsNoComment('(') ~ VariableDefinition.+ ~ wsNoComment(')') ~> (_.toVector)
  }

  private[this] def VariableDefinition = rule {
    Comments ~ trackPos ~ Variable ~ ws(
      ':') ~ Type ~ DefaultValue.? ~ (DirectivesConst.? ~> (_.getOrElse(Vector.empty))) ~>
      ((comment, location, name, tpe, defaultValue, dirs) =>
        ast.VariableDefinition(name, tpe, defaultValue, dirs, comment, location))
  }

  protected[this] def Variable: Rule[HNil, String :: HNil] = rule(Ignored.* ~ '$' ~ NameStrict)

  protected[this] def DefaultValue: Rule[HNil, ast.Value :: HNil] =
    rule(wsNoComment('=') ~ ValueConst)

  protected[this] def SelectionSet: Rule1[(Vector[ast.Selection], Vector[ast.Comment])] = rule {
    wsNoComment('{') ~ Selection.+ ~ Comments ~ wsNoComment('}') ~>
      ((x: Seq[ast.Selection], comments: Vector[ast.Comment]) => x.toVector -> comments)
  }

  private[this] def Selection = rule(Field | FragmentSpread | InlineFragment)

  private[this] def Field = rule {
    Comments ~ trackPos ~ Alias.? ~ Name ~
      (Arguments.? ~> (_.getOrElse(Vector.empty))) ~
      (Directives.? ~> (_.getOrElse(Vector.empty))) ~
      (SelectionSet.? ~> (_.getOrElse(Vector.empty -> Vector.empty))) ~>
      ((comment, location, alias, name, args, dirs, sels) =>
        ast.Field(alias, name, args, dirs, sels._1, comment, sels._2, location))
  }

  private[this] def Alias = rule(Name ~ ws(':'))

  protected[this] def Arguments = rule {
    Ignored.* ~ wsNoComment('(') ~ Argument.+ ~ wsNoComment(')') ~> (_.toVector)
  }

  protected[this] def ArgumentsConst = rule {
    Ignored.* ~ wsNoComment('(') ~ ArgumentConst.+ ~ wsNoComment(')') ~> (_.toVector)
  }

  private[this] def Argument = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> ((comment, location, name, value) =>
      ast.Argument(name, value, comment, location))
  }

  private[this] def ArgumentConst = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> (
      (comment, location, name, value) => ast.Argument(name, value, comment, location))
  }
}

sealed trait Fragments {
  this: Parser with Tokens with Ignored with Directives with Types with Operations =>

  protected[this] def experimentalFragmentVariables: Boolean

  protected[this] def FragmentSpread: Rule[HNil, ast.FragmentSpread :: HNil] = rule {
    Comments ~ trackPos ~ Ellipsis ~ FragmentName ~ (Directives.? ~> (_.getOrElse(Vector.empty))) ~>
      ((comment, location, name, dirs) => ast.FragmentSpread(name, dirs, comment, location))
  }

  protected[this] def InlineFragment = rule {
    Comments ~ trackPos ~ Ellipsis ~ TypeCondition.? ~ (Directives.? ~> (_.getOrElse(
      Vector.empty))) ~ SelectionSet ~>
      ((comment, location, typeCondition, dirs, sels) =>
        ast.InlineFragment(typeCondition, dirs, sels._1, comment, sels._2, location))
  }

  protected[this] def on: Rule[HNil, HNil] = rule(Keyword("on"))
  private[this] def Fragment = rule(Keyword("fragment"))

  protected[this] def FragmentDefinition = rule {
    Comments ~ trackPos ~ Fragment ~ FragmentName ~ ExperimentalFragmentVariables ~ TypeCondition ~ (Directives.? ~> (_.getOrElse(
      Vector.empty))) ~ SelectionSet ~>
      ((comment, location, name, vars, typeCondition, dirs, sels) =>
        ast.FragmentDefinition(
          name,
          typeCondition,
          dirs,
          sels._1,
          vars,
          comment,
          sels._2,
          location))
  }

  private[this] def ExperimentalFragmentVariables = rule {
    test(experimentalFragmentVariables) ~ VariableDefinitions.? ~> (_.getOrElse(
      Vector.empty)) | push(Vector.empty)
  }

  private[this] def FragmentName = rule(!on ~ Name)
  private[this] def TypeCondition = rule(on ~ NamedType)
}

sealed trait Values { this: Parser with Tokens with Ignored with Operations =>
  protected[this] def ValueConst: Rule1[ast.Value] = rule {
    NumberValue | StringValue | BooleanValue | NullValue | EnumValue | ListValueConst | ObjectValueConst
  }

  protected[this] def Value: Rule1[ast.Value] = rule {
    Comments ~ trackPos ~ Variable ~> ((comment, location, name) =>
      ast.VariableValue(name, comment, location)) |
      NumberValue |
      StringValue |
      BooleanValue |
      NullValue |
      EnumValue |
      ListValue |
      ObjectValue
  }

  private[this] def BooleanValue = rule {
    Comments ~ trackPos ~ True ~> ((comment, location) =>
      ast.BooleanValue(true, comment, location)) |
      Comments ~ trackPos ~ False ~> ((comment, location) =>
        ast.BooleanValue(false, comment, location))
  }

  private[this] def True = rule(Keyword("true"))

  private[this] def False = rule(Keyword("false"))

  private[this] def Null = rule(Keyword("null"))

  private[this] def NullValue = rule {
    Comments ~ trackPos ~ Null ~> ((comment, location) => ast.NullValue(comment, location))
  }

  private[this] def EnumValue = rule {
    Comments ~ !(True | False) ~ trackPos ~ Name ~> ((comment, location, name) =>
      ast.EnumValue(name, comment, location))
  }

  private[this] def ListValueConst = rule {
    Comments ~ trackPos ~ wsNoComment('[') ~ ValueConst.* ~ wsNoComment(']') ~> (
      (comment, location, v) => ast.ListValue(v.toVector, comment, location))
  }

  private[this] def ListValue = rule {
    Comments ~ trackPos ~ wsNoComment('[') ~ Value.* ~ wsNoComment(']') ~> (
      (comment, location, v) => ast.ListValue(v.toVector, comment, location))
  }

  private[this] def ObjectValueConst = rule {
    Comments ~ trackPos ~ wsNoComment('{') ~ ObjectFieldConst.* ~ wsNoComment('}') ~> (
      (comment, location, f) => ast.ObjectValue(f.toVector, comment, location))
  }

  private[this] def ObjectValue = rule {
    Comments ~ trackPos ~ wsNoComment('{') ~ ObjectField.* ~ wsNoComment('}') ~> (
      (comment, location, f) => ast.ObjectValue(f.toVector, comment, location))
  }

  private[this] def ObjectFieldConst = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> (
      (comment, location, name, value) => ast.ObjectField(name, value, comment, location))
  }

  private[this] def ObjectField = rule {
    Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> ((comment, location, name, value) =>
      ast.ObjectField(name, value, comment, location))
  }
}

sealed trait Directives { this: Parser with Tokens with Operations with Ignored =>
  protected[this] def Directives = rule(Directive.+ ~> (_.toVector))
  protected[this] def DirectivesConst = rule(DirectiveConst.+ ~> (_.toVector))

  private[this] def Directive = rule {
    Comments ~ trackPos ~ '@' ~ NameStrict ~ (Arguments.? ~> (_.getOrElse(Vector.empty))) ~>
      ((comment, location, name, args) => ast.Directive(name, args, comment, location))
  }

  private[this] def DirectiveConst = rule {
    Comments ~ trackPos ~ '@' ~ NameStrict ~ (ArgumentsConst.? ~> (_.getOrElse(Vector.empty))) ~>
      ((comment, location, name, args) => ast.Directive(name, args, comment, location))
  }
}

sealed trait Types { this: Parser with Tokens with Ignored =>
  protected[this] def Type: Rule1[ast.Type] = rule(NonNullType | ListType | NamedType)

  private[this] def TypeName = rule(Name)

  protected[this] def NamedType: Rule[HNil, ast.NamedType :: HNil] = rule {
    Ignored.* ~ trackPos ~ TypeName ~> ((location, name) => ast.NamedType(name, location))
  }

  private[this] def ListType = rule {
    trackPos ~ ws('[') ~ Type ~ wsNoComment(']') ~> ((location, tpe) => ast.ListType(tpe, location))
  }

  private[this] def NonNullType = rule {
    trackPos ~ TypeName ~ wsNoComment('!') ~> ((location, name) =>
      ast.NotNullType(ast.NamedType(name, location), location)) |
      trackPos ~ ListType ~ wsNoComment('!') ~> ((location, tpe) => ast.NotNullType(tpe, location))
  }
}

class QueryParser private (
    val input: ParserInput,
    val sourceId: String,
    val experimentalFragmentVariables: Boolean = false,
    val parseLocations: Boolean = true,
    override val parseComments: Boolean = true
) extends Parser
    with Tokens
    with Ignored
    with Document
    with Operations
    with Fragments
    with Values
    with Directives
    with Types
    with TypeSystemDefinitions

object QueryParser {
  def parse(input: String, config: ParserConfig = ParserConfig.default): Try[ast.Document] =
    parse(ParserInput(input), config)

  def parse(input: ParserInput, config: ParserConfig): Try[ast.Document] = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(
      input,
      id,
      config.experimentalFragmentVariables,
      config.parseLocations,
      config.parseComments)

    parser.Document.run() match {
      case Success(res) => Success(res.copy(sourceMapper = config.sourceMapperFn(id, input)))
      case Failure(e: ParseError) => Failure(SyntaxError(parser, input, e))
      case f @ Failure(_) => f
    }
  }

  def parseInput(input: String): Try[ast.Value] = parseInput(ParserInput(input))

  def parseInput(input: ParserInput): Try[ast.Value] = {
    val parser = new QueryParser(input, "")

    parser.InputDocument.run() match {
      case Success(res) if res.values.nonEmpty => Success(res.values.head)
      case Success(_) =>
        Failure(
          new IllegalArgumentException("Input document does not contain any value definitions."))
      case Failure(e: ParseError) => Failure(SyntaxError(parser, input, e))
      case Failure(e) => Failure(e)
    }
  }

  def parseInputDocument(
      input: String,
      config: ParserConfig = ParserConfig.default): Try[ast.InputDocument] =
    parseInputDocument(ParserInput(input), config)

  def parseInputDocument(input: ParserInput, config: ParserConfig): Try[ast.InputDocument] = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(
      input,
      id,
      config.experimentalFragmentVariables,
      config.parseLocations,
      config.parseComments)

    parser.InputDocument.run() match {
      case Success(res) => Success(res.copy(sourceMapper = config.sourceMapperFn(id, input)))
      case Failure(e: ParseError) => Failure(SyntaxError(parser, input, e))
      case f @ Failure(_) => f
    }
  }

  def parseInputWithVariables(input: String): Try[ast.Value] =
    parseInputWithVariables(ParserInput(input))

  def parseInputWithVariables(input: ParserInput): Try[ast.Value] = {
    val parser = new QueryParser(input, "")

    parser.InputDocumentWithVariables.run() match {
      case Success(res) if res.values.nonEmpty => Success(res.values.head)
      case Success(_) =>
        Failure(
          new IllegalArgumentException("Input document does not contain any value definitions."))
      case Failure(e: ParseError) => Failure(SyntaxError(parser, input, e))
      case Failure(e) => Failure(e)
    }
  }

  def parseInputDocumentWithVariables(
      input: String,
      config: ParserConfig = ParserConfig.default): Try[ast.InputDocument] =
    parseInputDocumentWithVariables(ParserInput(input), config)

  def parseInputDocumentWithVariables(
      input: ParserInput,
      config: ParserConfig): Try[ast.InputDocument] = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(input, id)

    parser.InputDocumentWithVariables.run() match {
      case Success(res) => Success(res.copy(sourceMapper = config.sourceMapperFn(id, input)))
      case Failure(e: ParseError) => Failure(SyntaxError(parser, input, e))
      case f @ Failure(_) => f
    }
  }
}
