package sangria.parser

import org.parboiled2._
import CharPredicate.{HexDigit, Digit19}

import sangria.{DeliveryScheme, ast}

import scala.util.{Success, Failure}

trait Tokens extends StringBuilding with PositionTracking { this: Parser with Ignored =>

  def Token =  rule { Punctuator | Name | NumberValue | StringValue }

  val PunctuatorChar = CharPredicate("!$():=@[]{|}")

  def Punctuator = rule { PunctuatorChar | Ellipsis }

  def Ellipsis = rule { 3 times '.' }

  val NameFirstChar = CharPredicate.Alpha ++ '_'

  val NameChar = NameFirstChar ++ CharPredicate.Digit

  def Name = rule { capture(NameFirstChar ~ NameChar.*) ~ Ignored.* }

  def NumberValue = rule { trackPos ~ capture(Sign.? ~ IntegerPart) ~ capture('.' ~ Digit.+ ~ ExponentPart.?).? ~ Ignored.* ~>
      ((pos, intPart, floatPart) =>
        floatPart map (f => ast.FloatValue((intPart + f).toDouble, Some(pos))) getOrElse
          ast.IntValue(intPart.toInt, Some(pos))) }

  def IntegerPart = rule { ch('0') | NonZeroDigit ~ Digit.* }

  def ExponentPart = rule { 'e' ~ Sign.? ~ Digit.+ }

  val Sign = '-'

  val NonZeroDigit = Digit19

  def Digit = rule { ch('0') | NonZeroDigit }

  def StringValue = rule { trackPos ~ '"' ~ clearSB() ~ Characters ~ '"' ~ push(sb.toString) ~ Ignored.* ~> ((pos, s) => ast.StringValue(s, Some(pos)))}

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
    Unicode ~> { code => sb.append(code.asInstanceOf[Char]); () }
  }

  def Unicode = rule { 'u' ~ capture(4 times HexDigit) ~> (Integer.parseInt(_, 16)) }

  def Keyword(s: String) = rule { s ~ Ignored.* }
}

trait Ignored extends PositionTracking { this: Parser =>

  val WhiteSpace = CharPredicate("\u0009\u000B\u000C\u0020\u00A0")

  def CRLF = rule { '\u000D' ~ '\u000A' }

  val LineTerminator = CharPredicate("\u000A\u000D\u2028\u2029")

  def Ignored = rule { quiet(WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | Comment | ',') }


  def Comment = rule { "#" ~ CommentChar.* }

  def CommentChar = rule { !LineTerminator ~ ANY }

  def ws(char: Char): Rule0 = rule { ch(char) ~ Ignored.* }

  def ws(s: String): Rule0 = rule { str(s) ~ Ignored.* }

}

trait Document { this: Parser with Operations with Ignored with Fragments with Operations =>

  def Document = rule { Ignored.* ~ trackPos ~ Definition.+ ~ EOI ~> ((pos, d) => ast.Document(d.toList, Some(pos))) }

  def Definition = rule { OperationDefinition | FragmentDefinition }

}

trait Operations extends PositionTracking { this: Parser with Tokens with Ignored with Fragments with Values with Types with Directives =>

  def OperationDefinition = rule {
    trackPos ~ SelectionSet ~> ((pos, s) => ast.OperationDefinition(selections = s, position = Some(pos))) |
    trackPos ~ OperationType ~ (OperationName ~> (Some(_))) ~ (VariableDefinitions.? ~> (_ getOrElse Nil)) ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~>
        ((pos, opType, name, vars, dirs, sels) => ast.OperationDefinition(opType, name, vars, dirs, sels, Some(pos)))
  }

  def OperationName = rule { Name }

  def OperationType = rule { Query ~ push(ast.OperationType.Query) | Mutation ~ push(ast.OperationType.Mutation) }

  def Query = rule { Keyword("query") }

  def Mutation = rule { Keyword("mutation") }

  def VariableDefinitions = rule { ws('(') ~ VariableDefinition.+ ~ ws(')') ~> (_.toList)}

  def VariableDefinition = rule { trackPos ~ Variable ~ ws(':') ~ Type ~ DefaultValue.? ~>
      ((pos, name, tpe, defaultValue) => ast.VariableDefinition(name, tpe, defaultValue, Some(pos))) }

  def Variable = rule { '$' ~ Name }

  def DefaultValue = rule { ws('=') ~ ValueConst }

  def SelectionSet: Rule1[List[sangria.ast.Selection]] = rule { ws('{') ~ Selection.+ ~ ws('}') ~> ((x: Seq[sangria.ast.Selection]) => x.toList) }

  def Selection = rule { Field | FragmentSpread | InlineFragment }

  def Field = rule { trackPos ~ Alias.? ~ Name ~
      (Arguments.? ~> (_ getOrElse Nil)) ~
      (Directives.? ~> (_ getOrElse Nil)) ~
      (SelectionSet.? ~> (_ getOrElse Nil)) ~>
        ((pos, alias, name, args, dirs, sels) => ast.Field(alias, name, args, dirs, sels, Some(pos))) }

  def Alias = rule { Name ~ ws(':') }

  def Arguments = rule { ws('(') ~ Argument.+ ~ ws(')') ~> (_.toList) }

  def Argument = rule { trackPos ~ Name ~ ws(':') ~ Value ~> ((pos, name, value) => ast.Argument(name, value, Some(pos))) }

}

trait Fragments { this: Parser with Tokens with Ignored with Directives with Types with Operations =>

  def FragmentSpread = rule { trackPos ~ Ellipsis ~ Ignored.* ~ FragmentName ~ (Directives.? ~> (_ getOrElse Nil)) ~>
      ((pos, name, dirs) => ast.FragmentSpread(name, dirs, Some(pos))) }

  def InlineFragment = rule { trackPos ~ Ellipsis ~ Ignored.* ~ On ~ TypeCondition ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~>
      ((pos, typeCondition, dirs, sels) => ast.InlineFragment(typeCondition, dirs, sels, Some(pos))) }

  def On = rule { Keyword("on") }

  def Fragment = rule { Keyword("fragment") }

  def FragmentDefinition = rule { trackPos ~ Fragment ~ FragmentName ~ On ~ TypeCondition ~ (Directives.?  ~> (_ getOrElse Nil)) ~ SelectionSet ~>
      ((pos, name, typeCondition, dirs, sels) => ast.FragmentDefinition(name, typeCondition, dirs, sels, Some(pos))) }

  def FragmentName = rule { !On ~ Name }

  def TypeCondition = rule { TypeName }

}

trait Values { this: Parser with Tokens with Ignored with Operations =>

  def ValueConst: Rule1[sangria.ast.Value] = rule {
    NumberValue | StringValue | BooleanValue | EnumValue | ArrayValueConst | ObjectValueConst
  }

  def Value: Rule1[sangria.ast.Value] = rule {
    trackPos ~ Variable ~> ((pos, name) => ast.VariableValue(name, Some(pos))) |
    NumberValue |
    StringValue |
    BooleanValue |
    EnumValue |
    ArrayValue |
    ObjectValue
  }

  def BooleanValue = rule {
    trackPos ~ True ~> (pos => ast.BooleanValue(true, Some(pos))) |
    trackPos ~ False ~> (pos => ast.BooleanValue(false, Some(pos)))
  }

  def True = rule { Keyword("true") }

  def False = rule { Keyword("false") }

  def EnumValue = rule { trackPos ~ Name ~> ((pos, name) => ast.EnumValue(name, Some(pos))) }

  def ArrayValueConst = rule { trackPos ~ ws('[') ~ ValueConst.* ~ ws(']')  ~> ((pos, v) => ast.ArrayValue(v.toList, Some(pos))) }

  def ArrayValue = rule { trackPos ~ ws('[') ~ Value.* ~ ws(']') ~> ((pos, v) => ast.ArrayValue(v.toList, Some(pos))) }

  def ObjectValueConst = rule { trackPos ~ ws('{') ~ ObjectFieldConst.* ~ ws('}') ~> ((pos, f) => ast.ObjectValue(f.toList, Some(pos))) }

  def ObjectValue = rule { trackPos ~ ws('{') ~ ObjectField.* ~ ws('}') ~> ((pos, f) => ast.ObjectValue(f.toList, Some(pos))) }

  def ObjectFieldConst = rule { trackPos ~ Name ~ ws(':') ~ ValueConst ~> ((pos, name, value) => ast.ObjectField(name, value, Some(pos))) }

  def ObjectField = rule { trackPos ~ Name ~ ws(':') ~ Value ~> ((pos, name, value) => ast.ObjectField(name, value, Some(pos))) }

}

trait Directives { this: Parser with Tokens with Operations =>

  def Directives = rule { Directive.+ ~> (_.toList) }

  def Directive = rule { trackPos ~ '@' ~ Name ~ (Arguments.? ~> (_ getOrElse Nil)) ~>
      ((pos, name, args) => ast.Directive(name, args, Some(pos))) }

}

trait Types { this: Parser with Tokens with Ignored =>
  def Type: Rule1[sangria.ast.Type] = rule {
    trackPos ~ TypeName ~> ((pos, name) => ast.Type(name, isList = false, isNotNull = false, position = Some(pos))) |
    ListType |
    NonNullType
  }

  def TypeName = rule { Name }

  def ListType = rule { ws('[') ~ Type ~ ws(']') ~> (_.copy(isList = true)) }

  def NonNullType = rule {
    trackPos ~ TypeName ~ ws('!')  ~> ((pos, name) => ast.Type(name, isList = false, isNotNull = true, position = Some(pos))) |
    trackPos ~ ListType ~ ws('!') ~> ((pos, tpe) => tpe.copy(isNotNull = true, position = Some(pos)))
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
      case Success(res) => scheme.success(res)
      case Failure(e: ParseError) => scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) => scheme.failure(e)
    }
  }
}