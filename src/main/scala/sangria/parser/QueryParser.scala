package sangria.parser

import org.parboiled2._
import CharPredicate.{HexDigit, Digit19, AlphaNum}

import sangria.ast

import scala.util.{Success, Failure}

trait Tokens extends StringBuilding with PositionTracking { this: Parser with Ignored ⇒

  def Token =  rule { Punctuator | Name | NumberValue | StringValue }

  val PunctuatorChar = CharPredicate("!$():=@[]{|}")

  def Punctuator = rule { PunctuatorChar | Ellipsis }

  def Ellipsis = rule { quiet(str("...") ~ IgnoredNoComment.*) }

  val NameFirstChar = CharPredicate.Alpha ++ '_'

  val NameChar = NameFirstChar ++ CharPredicate.Digit

  def NameStrict = rule { capture(NameFirstChar ~ NameChar.*) ~ IgnoredNoComment.* }

  def Name = rule { Ignored.* ~ NameStrict }

  def NumberValue = rule { atomic(Comments ~ trackPos ~ IntegerValuePart ~ FloatValuePart.? ~ IgnoredNoComment.*) ~>
      ((comment, pos, intPart, floatPart) ⇒
        floatPart map (f ⇒ ast.BigDecimalValue(BigDecimal(intPart + f), comment, Some(pos))) getOrElse
          ast.BigIntValue(BigInt(intPart), comment, Some(pos))) }

  def FloatValuePart = rule { atomic(capture(FractionalPart ~ ExponentPart.? | ExponentPart)) }

  def FractionalPart = rule { '.' ~ Digit.+ }

  def IntegerValuePart = rule { capture(NegativeSign.? ~ IntegerPart) }

  def IntegerPart = rule { ch('0') | NonZeroDigit ~ Digit.* }

  def ExponentPart = rule { ExponentIndicator ~ Sign.? ~ Digit.+ }

  def ExponentIndicator = rule { ch('e') | ch('E') }

  def Sign = rule { ch('-') | '+' }

  val NegativeSign  = '-'

  val NonZeroDigit = Digit19

  def Digit = rule { ch('0') | NonZeroDigit }

  def StringValue = rule { atomic(Comments ~ trackPos ~ '"' ~ clearSB() ~ Characters ~ '"' ~ push(sb.toString) ~ IgnoredNoComment.* ~> ((comment, pos, s) ⇒ ast.StringValue(s, comment, Some(pos))))}

  def Characters = rule { (NormalChar | '\\' ~ EscapedChar).* }

  val QuoteBackslash = CharPredicate("\"\\")

  def NormalChar = rule { !(QuoteBackslash | LineTerminator) ~ ANY ~ appendSB() }

  def EscapedChar = rule {
    QuoteBackslash ~ appendSB() |
    'b' ~ appendSB('\b') |
    'f' ~ appendSB('\f') |
    'n' ~ appendSB('\n') |
    'r' ~ appendSB('\r') |
    't' ~ appendSB('\t') |
    Unicode ~> { code ⇒ sb.append(code.asInstanceOf[Char]); () }
  }

  def Unicode = rule { 'u' ~ capture(4 times HexDigit) ~> (Integer.parseInt(_, 16)) }

  def Keyword(s: String) = rule { atomic(Ignored.* ~ s ~ !NameChar ~ IgnoredNoComment.*) }
}

trait Ignored extends PositionTracking { this: Parser ⇒

  val WhiteSpace = CharPredicate("\u0009\u0020")

  def CRLF = rule { '\u000D' ~ '\u000A' }

  val LineTerminator = CharPredicate("\u000A")

  val UnicodeBOM = CharPredicate('\uFEFF')

  def Ignored = rule { quiet(UnicodeBOM | WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | Comment | ',') }

  def IgnoredNoComment = rule { quiet(UnicodeBOM | WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | ',') }

  def Comments = rule { trackPos ~ CommentCap.* ~ Ignored.* ~> ((pos, lines) ⇒ if (lines.nonEmpty) Some(ast.Comment(lines, Some(pos))) else None) }

  def CommentCap = rule { "#" ~ capture(CommentChar.*) ~ IgnoredNoComment.* }

  def Comment = rule { "#" ~ CommentChar.* }

  def CommentChar = rule { !(CRLF | LineTerminator) ~ ANY }

  def ws(char: Char): Rule0 = rule { quiet(Ignored.* ~ ch(char) ~ Ignored.*) }

  def wsNoComment(char: Char): Rule0 = rule { quiet(Ignored.* ~ ch(char) ~ IgnoredNoComment.*) }

  def ws(s: String): Rule0 = rule { quiet(Ignored.* ~ str(s) ~ Ignored.*) }

}

trait Document { this: Parser with Operations with Ignored with Fragments with Operations with Values ⇒

  def Document = rule { IgnoredNoComment.* ~ trackPos ~ Definition.+ ~ Ignored.* ~ EOI ~> ((pos, d) ⇒ ast.Document(d.toList, Some(pos))) }

  def InputDocument = rule { Ignored.* ~ ValueConst ~ Ignored.* ~ EOI }

  def Definition = rule { OperationDefinition | FragmentDefinition }

}

trait Operations extends PositionTracking { this: Parser with Tokens with Ignored with Fragments with Values with Types with Directives ⇒

  def OperationDefinition = rule {
    Comments ~ trackPos ~ SelectionSet ~> ((comment, pos, s) ⇒ ast.OperationDefinition(selections = s, comment = comment, position = Some(pos))) |
    Comments ~ trackPos ~ OperationType ~ OperationName.? ~ (VariableDefinitions.? ~> (_ getOrElse Nil)) ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~>
        ((comment, pos, opType, name, vars, dirs, sels) ⇒ ast.OperationDefinition(opType, name, vars, dirs, sels, comment, Some(pos)))
  }

  def OperationName = rule { Name }

  def OperationType = rule {
    Query ~ push(ast.OperationType.Query) |
    Mutation ~ push(ast.OperationType.Mutation) |
    Subscription ~ push(ast.OperationType.Subscription)
  }

  def Query = rule { Keyword("query") }

  def Mutation = rule { Keyword("mutation") }

  def Subscription = rule { Keyword("subscription") }

  def VariableDefinitions = rule { wsNoComment('(') ~ VariableDefinition.+ ~ wsNoComment(')') ~> (_.toList)}

  def VariableDefinition = rule { Comments ~ trackPos ~ Variable ~ ws(':') ~ Type ~ DefaultValue.? ~>
      ((comment, pos, name, tpe, defaultValue) ⇒ ast.VariableDefinition(name, tpe, defaultValue, comment, Some(pos))) }

  def Variable = rule { Ignored.* ~ '$' ~ NameStrict }

  def DefaultValue = rule { wsNoComment('=') ~ ValueConst }

  def SelectionSet: Rule1[List[ast.Selection]] = rule { wsNoComment('{') ~ Selection.+ ~ wsNoComment('}') ~> ((x: Seq[ast.Selection]) ⇒ x.toList) }

  def Selection = rule { Field | FragmentSpread | InlineFragment }

  def Field = rule { Comments ~ trackPos ~ Alias.? ~ Name ~
      (Arguments.? ~> (_ getOrElse Nil)) ~
      (Directives.? ~> (_ getOrElse Nil)) ~
      (SelectionSet.? ~> (_ getOrElse Nil)) ~>
        ((comment, pos, alias, name, args, dirs, sels) ⇒ ast.Field(alias, name, args, dirs, sels, comment, Some(pos))) }

  def Alias = rule { Name ~ ws(':') }

  def Arguments = rule { Ignored.* ~ wsNoComment('(') ~ Argument.+ ~ wsNoComment(')') ~> (_.toList) }

  def Argument = rule { Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> ((comment, pos, name, value) ⇒ ast.Argument(name, value, comment, Some(pos))) }

}

trait Fragments { this: Parser with Tokens with Ignored with Directives with Types with Operations ⇒

  def FragmentSpread = rule { Comments ~ trackPos ~ Ellipsis ~ FragmentName ~ (Directives.? ~> (_ getOrElse Nil)) ~>
      ((comment, pos, name, dirs) ⇒ ast.FragmentSpread(name, dirs, comment, Some(pos))) }

  def InlineFragment = rule { Comments ~ trackPos ~ Ellipsis ~ TypeCondition.? ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~>
      ((comment, pos, typeCondition, dirs, sels) ⇒ ast.InlineFragment(typeCondition, dirs, sels, comment, Some(pos))) }

  def on = rule { Keyword("on") }

  def Fragment = rule { Keyword("fragment") }

  def FragmentDefinition = rule { Comments ~ trackPos ~ Fragment ~ FragmentName ~ TypeCondition ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~>
    ((comment, pos, name, typeCondition, dirs, sels) ⇒ ast.FragmentDefinition(name, typeCondition, dirs, sels, comment, Some(pos))) }

  def FragmentName = rule { !on ~ Name }

  def TypeCondition = rule { on ~ NamedType }

}

trait Values { this: Parser with Tokens with Ignored with Operations ⇒

  def ValueConst: Rule1[ast.Value] = rule {
    NumberValue | StringValue | BooleanValue | NullValue | EnumValue | ListValueConst | ObjectValueConst
  }

  def Value: Rule1[ast.Value] = rule {
    Comments ~ trackPos ~ Variable ~> ((comment, pos, name) ⇒ ast.VariableValue(name, comment, Some(pos))) |
    NumberValue |
    StringValue |
    BooleanValue |
    NullValue |
    EnumValue |
    ListValue |
    ObjectValue
  }

  def BooleanValue = rule {
    Comments ~ trackPos ~ True ~> ((comment, pos) ⇒ ast.BooleanValue(true, comment, Some(pos))) |
    Comments ~ trackPos ~ False ~> ((comment, pos) ⇒ ast.BooleanValue(false, comment, Some(pos)))
  }

  def True = rule { Keyword("true") }

  def False = rule { Keyword("false") }

  def Null = rule { Keyword("null") }

  def NullValue = rule { Comments ~ trackPos ~ Null ~> ((comment, pos) ⇒ ast.NullValue(comment, Some(pos))) }

  def EnumValue = rule { Comments ~ !True ~ !False ~ trackPos ~ Name ~> ((comment, pos, name) ⇒ ast.EnumValue(name, comment, Some(pos))) }

  def ListValueConst = rule { Comments ~ trackPos ~ wsNoComment('[') ~ ValueConst.* ~ wsNoComment(']')  ~> ((comment, pos, v) ⇒ ast.ListValue(v.toList, comment, Some(pos))) }

  def ListValue = rule { Comments ~ trackPos ~ wsNoComment('[') ~ Value.* ~ wsNoComment(']') ~> ((comment, pos, v) ⇒ ast.ListValue(v.toList, comment, Some(pos))) }

  def ObjectValueConst = rule { Comments ~ trackPos ~ wsNoComment('{') ~ ObjectFieldConst.* ~ wsNoComment('}') ~> ((comment, pos, f) ⇒ ast.ObjectValue(f.toList, comment, Some(pos))) }

  def ObjectValue = rule { Comments ~ trackPos ~ wsNoComment('{') ~ ObjectField.* ~ wsNoComment('}') ~> ((comment, pos, f) ⇒ ast.ObjectValue(f.toList, comment, Some(pos))) }

  def ObjectFieldConst = rule { Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> ((comment, pos, name, value) ⇒ ast.ObjectField(name, value, comment, Some(pos))) }

  def ObjectField = rule { Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> ((comment, pos, name, value) ⇒ ast.ObjectField(name, value, comment, Some(pos))) }

}

trait Directives { this: Parser with Tokens with Operations with Ignored ⇒

  def Directives = rule { Directive.+ ~> (_.toList) }

  def Directive = rule { Comments ~ trackPos ~ '@' ~ NameStrict ~ (Arguments.? ~> (_ getOrElse Nil)) ~>
      ((comment, pos, name, args) ⇒ ast.Directive(name, args, comment, Some(pos))) }

}

trait Types { this: Parser with Tokens with Ignored ⇒
  def Type: Rule1[ast.Type] = rule { NonNullType | ListType | NamedType }

  def TypeName = rule { Name }

  def NamedType = rule { Ignored.* ~ trackPos ~ TypeName ~> ((pos, name) ⇒ ast.NamedType(name, Some(pos)))}

  def ListType = rule { trackPos ~ ws('[') ~ Type ~ ws(']') ~> ((pos, tpe) ⇒ ast.ListType(tpe, Some(pos))) }

  def NonNullType = rule {
    trackPos ~ TypeName ~ ws('!')  ~> ((pos, name) ⇒ ast.NotNullType(ast.NamedType(name, Some(pos)), Some(pos))) |
    trackPos ~ ListType ~ ws('!') ~> ((pos, tpe) ⇒ ast.NotNullType(tpe, Some(pos)))
  }
}

class QueryParser private (val input: ParserInput) 
    extends Parser with Tokens with Ignored with Document with Operations with Fragments with Values with Directives with Types

object QueryParser {
  def parse(input: String)(implicit scheme: DeliveryScheme[ast.Document]): scheme.Result =
    parse(ParserInput(input))(scheme)

  def parse(input: ParserInput)(implicit scheme: DeliveryScheme[ast.Document]): scheme.Result = {
    val parser = new QueryParser(input)

    parser.Document.run() match {
      case Success(res) ⇒
        scheme.success(res.copy(sourceMapper = Some(new Parboiled2SourceMapper(input))))
      case Failure(e: ParseError) ⇒ scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) ⇒ scheme.failure(e)
    }
  }

  def parseInput(input: String)(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result =
    parseInput(ParserInput(input))( scheme)

  def parseInput(input: ParserInput)(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result = {
    val parser = new QueryParser(input)

    parser.InputDocument.run() match {
      case Success(res) ⇒ scheme.success(res)
      case Failure(e: ParseError) ⇒ scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) ⇒ scheme.failure(e)
    }
  }
}