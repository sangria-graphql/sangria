package sangria.parser

import org.parboiled2._
import CharPredicate.{HexDigit, Digit19}

import scala.util.{Success, Failure, Try}

trait Tokens extends StringBuilding { this: Parser with Ignored =>

  def Token =  rule { Punctuator | Name | IntValue | FloatValue | StringValue }

  val PunctuatorChar = CharPredicate("!$():=@[]{|}")

  def Punctuator = rule { PunctuatorChar | Ellipsis }

  def Ellipsis = rule { 3 times '.' }

  val NameFirstChar = CharPredicate.Alpha ++ '_'

  val NameChar = NameFirstChar ++ CharPredicate.Digit

  def Name = rule { capture(NameFirstChar ~ NameChar.*) ~ Ignored.* }

  def IntValue = rule { capture(Sign.? ~ IntegerPart) ~ Ignored.* ~> (i => ast.IntValue(i.toInt))}

  def FloatValue = rule { capture(Sign.?  ~ IntegerPart ~ '.' ~ Digit.+ ~ ExponentPart.?) ~ Ignored.* ~> (f => ast.FloatValue(f.toDouble)) }

  def IntegerPart = rule { ch('0') | NonZeroDigit | NonZeroDigit ~ Digit.+ }

  def ExponentPart = rule { 'e' ~ Sign.? ~ Digit.+ }

  val Sign = '-'

  val NonZeroDigit = Digit19

  def Digit = rule { ch('0') | NonZeroDigit }

  def StringValue = rule { '"' ~ clearSB() ~ Characters ~ '"' ~ push(sb.toString) ~ Ignored.* ~> (ast.StringValue(_))}

  def Characters = rule { (NormalChar | '\\' ~ EscapedChar).* }

  val QuoteBackslash = CharPredicate("\"\\")

  val QuoteSlashBackSlash = QuoteBackslash ++ "/"

  def NormalChar = rule { !(QuoteBackslash | LineTerminator) ~ ANY ~ appendSB() }


  def EscapedChar = rule {
    QuoteSlashBackSlash ~ appendSB() |
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

trait Ignored { this: Parser =>

  val WhiteSpace = CharPredicate("\u0009\u000B\u000C\u0020\u00A0")

  val LineTerminator = CharPredicate("\u000A\u000D\u2028\u2029")

  def Ignored = rule { WhiteSpace | LineTerminator | Comment | ',' }


  def Comment = rule { "#" ~ CommentChar.* }

  def CommentChar = rule { !LineTerminator ~ ANY }

  def ws(char: Char): Rule0 = rule { ch(char) ~ Ignored.* }

  def ws(s: String): Rule0 = rule { str(s) ~ Ignored.* }

}

trait Document { this: Parser with Operations with Ignored with Fragments with Operations =>

  def Document = rule { Ignored.* ~ Definition.+ ~ EOI ~> (d => ast.Document(d.toList)) }

  def Definition = rule { OperationDefinition | FragmentDefinition }

}

trait Operations { this: Parser with Tokens with Ignored with Fragments with Values with Types with Directives =>

  def OperationDefinition = rule {
    SelectionSet ~> (s => ast.OperationDefinition(selections = s)) |
    OperationType ~ (OperationName ~> (Some(_))) ~ (VariableDefinitions.? ~> (_ getOrElse Nil)) ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~>
        (ast.OperationDefinition(_, _, _, _, _))
  }

  def OperationName = rule { Name }

  def OperationType = rule { Query ~ push(ast.OperationType.Query) | Mutation ~ push(ast.OperationType.Mutation) }

  def Query = Keyword("query")

  def Mutation = Keyword("mutation")

  def VariableDefinitions = rule { ws('(') ~ VariableDefinition.+ ~ ws(')') ~> (_.toList)}

  def VariableDefinition = rule { Variable ~ ws(':') ~ Type ~ DefaultValue.? ~> (ast.VariableDefinition(_, _, _)) }

  def Variable = rule { '$' ~ Name }

  def DefaultValue = rule { ws('=') ~ ValueConst }

  def SelectionSet: Rule1[List[ast.Selection]] = rule { ws('{') ~ Selection.+ ~ ws('}') ~> ((x: Seq[ast.Selection]) => x.toList) }

  def Selection = rule { Field | FragmentSpread | InlineFragment }

  def Field = rule { Alias.? ~ Name ~
      (Arguments.? ~> (_ getOrElse Nil)) ~
      (Directives.? ~> (_ getOrElse Nil)) ~
      (SelectionSet.? ~> (_ getOrElse Nil)) ~> (ast.Field(_, _, _, _, _))}

  def Alias = rule { Name ~ ws(':') }

  def Arguments = rule { ws('(') ~ Argument.+ ~ ws(')') ~> (_.toList) }

  def Argument = rule { Name ~ ws(':') ~ Value ~> (ast.Argument(_, _))}

}

trait Fragments { this: Parser with Tokens with Ignored with Directives with Types with Operations =>

  def FragmentSpread = rule { Ellipsis ~ Ignored.* ~ FragmentName ~ (Directives.? ~> (_ getOrElse Nil)) ~> (ast.FragmentSpread(_, _)) }

  def InlineFragment = rule { Ellipsis ~ Ignored.* ~ On ~ TypeCondition ~ (Directives.? ~> (_ getOrElse Nil)) ~ SelectionSet ~>
      (ast.InlineFragment(_, _, _)) }

  def On = Keyword("on")

  def Fragment = Keyword("fragment")

  def FragmentDefinition = rule { Fragment ~ FragmentName ~ On ~ TypeCondition ~ (Directives.?  ~> (_ getOrElse Nil)) ~ SelectionSet ~>
      (ast.FragmentDefinition(_, _, _, _)) }

  def FragmentName = rule { !On ~ Name }

  def TypeCondition = rule { TypeName }

}

trait Values { this: Parser with Tokens with Ignored with Operations =>

  def ValueConst: Rule1[ast.Value] = rule { FloatValue | IntValue | StringValue | BooleanValue | EnumValue | ArrayValueConst | ObjectValueConst }

  def Value: Rule1[ast.Value] = rule { Variable ~> (ast.VariableValue(_)) | FloatValue | IntValue | StringValue | BooleanValue | EnumValue | ArrayValue | ObjectValue }

  def BooleanValue = rule { True ~ push(ast.BooleanValue(true)) | False ~ push(ast.BooleanValue(false)) }

  def True = Keyword("true")

  def False = Keyword("false")

  def EnumValue = rule { Name ~> (ast.EnumValue(_)) }

  def ArrayValueConst = rule { ws('[') ~ ValueConst.* ~ ws(']')  ~> (v => ast.ArrayValue(v.toList)) }

  def ArrayValue = rule { ws('[') ~ Value.* ~ ws(']') ~> (v => ast.ArrayValue(v.toList)) }

  def ObjectValueConst = rule { ws('{') ~ ObjectFieldConst.* ~ ws('}') ~> (f => ast.ObjectValue(f.toList)) }

  def ObjectValue = rule { ws('{') ~ ObjectField.* ~ ws('}') ~> (f => ast.ObjectValue(f.toList)) }

  def ObjectFieldConst = rule { Name ~ ws(':') ~ ValueConst ~> (ast.ObjectField(_, _)) }

  def ObjectField = rule { Name ~ ws(':') ~ Value ~> (ast.ObjectField(_, _)) }

}

trait Directives { this: Parser with Tokens with Operations =>

  def Directives = rule { Directive.+ ~> (_.toList) }

  def Directive = rule { '@' ~ Name ~ (Arguments.? ~> (_ getOrElse Nil)) ~> (ast.Directive(_, _))}

}

trait Types { this: Parser with Tokens with Ignored =>
  def Type: Rule1[ast.Type] = rule { TypeName ~> (ast.Type(_, isList = false, isNotNull = false)) | ListType | NonNullType }

  def TypeName = rule { Name }

  def ListType = rule { ws('[') ~ Type ~ ws(']') ~> (_.copy(isList = true)) }

  def NonNullType = rule {
    TypeName ~ ws('!')  ~> (ast.Type(_, isList = false, isNotNull = true)) |
    ListType ~ ws('!') ~> (_.copy(isNotNull = true))
  }
}

class QueryParser private (val input: ParserInput) 
    extends Parser with Tokens with Ignored with Document with Operations with Fragments with Values with Directives with Types

object QueryParser {
  def parse(input: ParserInput): Try[ast.Document] = {
    val parser = new QueryParser(input)

    parser.Document.run().transform(Success(_), {
      case e: ParseError => Failure(SyntaxError(parser, input, e))
      case otherError => Failure(otherError)
    })
  }
}