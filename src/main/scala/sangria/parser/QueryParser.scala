package sangria.parser

import java.util.UUID

import org.parboiled2._
import CharPredicate.{Digit19, HexDigit}
import sangria.ast
import sangria.util.StringUtil

import scala.util.{Failure, Success}

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
      ((comment, location, intPart, floatPart) ⇒
        floatPart map (f ⇒ ast.BigDecimalValue(BigDecimal(intPart + f), comment, location)) getOrElse
          ast.BigIntValue(BigInt(intPart), comment, location)) }

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

  def StringValue = rule { BlockStringValue | NonBlockStringValue }

  def BlockStringValue = rule {
    Comments ~ trackPos ~ BlockString ~ clearSB() ~ BlockStringCharacters ~ BlockString ~ push(sb.toString) ~ IgnoredNoComment.* ~>
      ((comment, location, s) ⇒ ast.StringValue(StringUtil.blockStringValue(s), true, Some(s), comment, location))
  }

  def BlockStringCharacters = rule { (BlockStringCharacter | BlockStringEscapedChar).* }

  def BlockString = rule { str("\"\"\"") }

  def QuotedBlockString = rule { str("\\\"\"\"") }

  def BlockStringCharacter = rule { !(QuotedBlockString | BlockString) ~ ((CRLF | LineTerminator) ~ trackNewLine | ANY) ~ appendSB() }

  def BlockStringEscapedChar = rule {
    QuotedBlockString ~ appendSB("\"\"\"")
  }

  def NormalCharacter = rule { !(QuoteBackslash | LineTerminator) ~ ANY ~ appendSB() }

  def NonBlockStringValue = rule {
    Comments ~ trackPos ~ '"' ~ clearSB() ~ Characters ~ '"' ~ push(sb.toString) ~ IgnoredNoComment.* ~>
        ((comment, location, s) ⇒ ast.StringValue(s, false, None, comment, location))
  }

  def Characters = rule { (NormalCharacter | '\\' ~ EscapedChar).* }

  val QuoteBackslash = CharPredicate("\"\\")

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
  def parseComments: Boolean

  val WhiteSpace = CharPredicate("\u0009\u0020")

  def CRLF = rule { '\u000D' ~ '\u000A' }

  val LineTerminator = CharPredicate("\u000A")

  val UnicodeBOM = CharPredicate('\uFEFF')

  def Ignored = rule { quiet(UnicodeBOM | WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | Comment | ',') }

  def IgnoredNoComment = rule { quiet(UnicodeBOM | WhiteSpace | (CRLF | LineTerminator) ~ trackNewLine | ',') }

  def Comments = rule { test(parseComments) ~ CommentCap.* ~ Ignored.* ~> (_.toVector) | CommentNoCap.* ~ Ignored.* ~ push(Vector.empty) }

  def CommentCap = rule { trackPos ~ "#" ~ capture(CommentChar.*) ~ IgnoredNoComment.* ~> ((location, comment) ⇒ ast.Comment(comment, location)) }

  def CommentNoCap: Rule0 = rule { "#" ~ CommentChar.* ~ IgnoredNoComment.* }

  def Comment = rule { "#" ~ CommentChar.* }

  def CommentChar = rule { !(CRLF | LineTerminator) ~ ANY }

  def ws(char: Char): Rule0 = rule { quiet(Ignored.* ~ ch(char) ~ Ignored.*) }

  def wsNoComment(char: Char): Rule0 = rule { quiet(Ignored.* ~ ch(char) ~ IgnoredNoComment.*) }

  def ws(s: String): Rule0 = rule { quiet(Ignored.* ~ str(s) ~ Ignored.*) }

  def wsCapture(s: String) = rule { quiet(Ignored.* ~ capture(str(s)) ~ IgnoredNoComment.*) }

}

trait Document { this: Parser with Operations with Ignored with Fragments with Operations with Values with TypeSystemDefinitions ⇒

  def Document = rule {
    IgnoredNoComment.* ~ trackPos ~ Definition.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~>
      ((location, d, comments) ⇒ ast.Document(d.toVector, comments, location))
  }

  def InputDocument = rule {
    IgnoredNoComment.* ~ trackPos ~ ValueConst.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~>
      ((location, vs, comments) ⇒ ast.InputDocument(vs.toVector, comments, location))
  }

  def InputDocumentWithVariables = rule {
    IgnoredNoComment.* ~ trackPos ~ Value.+ ~ IgnoredNoComment.* ~ Comments ~ EOI ~>
      ((location, vs, comments) ⇒ ast.InputDocument(vs.toVector, comments, location))
  }

  def Definition = rule {
    ExecutableDefinition |
    TypeSystemDefinition |
    TypeSystemExtension
  }

  def ExecutableDefinition = rule {
    OperationDefinition |
    FragmentDefinition
  }

}

trait TypeSystemDefinitions { this: Parser with Tokens with Ignored with Directives with Types with Operations with Values with Fragments ⇒
  def legacyImplementsInterface: Boolean
  def legacyEmptyFields: Boolean

  def scalar = rule { Keyword("scalar") }
  def `type` = rule { Keyword("type") }
  def interface = rule { Keyword("interface") }
  def union = rule { Keyword("union") }
  def enum = rule { Keyword("enum") }
  def inputType = rule { Keyword("input") }
  def implements = rule { Keyword("implements") }
  def extend = rule { Keyword("extend") }
  def directive = rule { Keyword("directive") }
  def schema = rule { Keyword("schema") }

  def TypeSystemDefinition = rule {
    SchemaDefinition |
    TypeDefinition |
    DirectiveDefinition
  }

  def TypeDefinition = rule {
    ScalarTypeDefinition |
    ObjectTypeDefinition |
    InterfaceTypeDefinition |
    UnionTypeDefinition |
    EnumTypeDefinition |
    InputObjectTypeDefinition
  }

  def ScalarTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ scalar ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~> (
      (descr, comment, location, name, dirs) ⇒ ast.ScalarTypeDefinition(name, dirs, descr, comment, location))
  }

  def ObjectTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Vector.empty)) ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ FieldsDefinition.? ~> (
      (descr, comment, location, name, interfaces, dirs, fields) ⇒ ast.ObjectTypeDefinition(name, interfaces, fields.fold(Vector.empty[ast.FieldDefinition])(_._1.toVector), dirs, descr, comment, fields.fold(Vector.empty[ast.Comment])(_._2), location))
  }

  def TypeSystemExtension = rule {
    SchemaExtension |
    TypeExtension
  }

  def TypeExtension = rule {
    ScalarTypeExtensionDefinition |
    ObjectTypeExtensionDefinition |
    InterfaceTypeExtensionDefinition |
    UnionTypeExtensionDefinition |
    EnumTypeExtensionDefinition |
    InputObjectTypeExtensionDefinition
  }

  def SchemaExtension = rule {
    (Comments ~ trackPos ~ extend ~ schema ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ wsNoComment('{') ~ OperationTypeDefinition.+ ~ Comments ~ wsNoComment('}') ~> (
      (comment, location, dirs, ops, tc) ⇒ ast.SchemaExtensionDefinition(ops.toVector, dirs, comment, tc, location))) |
    (Comments ~ trackPos ~ extend ~ schema ~ DirectivesConst ~> (
      (comment, location, dirs) ⇒ ast.SchemaExtensionDefinition(Vector.empty, dirs, comment, Vector.empty, location)))
  }

  def ObjectTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Vector.empty)) ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ FieldsDefinition ~> (
      (comment, location, name, interfaces, dirs, fields) ⇒ ast.ObjectTypeExtensionDefinition(name, interfaces, fields._1.toVector, dirs, comment, fields._2, location))) |
    (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ (ImplementsInterfaces.? ~> (_ getOrElse Vector.empty)) ~ DirectivesConst ~> (
      (comment, location, name, interfaces, dirs) ⇒ ast.ObjectTypeExtensionDefinition(name, interfaces, Vector.empty, dirs, comment, Vector.empty, location))) |
    (Comments ~ trackPos ~ extend ~ `type` ~ Name ~ ImplementsInterfaces ~> (
      (comment, location, name, interfaces) ⇒ ast.ObjectTypeExtensionDefinition(name, interfaces, Vector.empty, Vector.empty, comment, Vector.empty, location)))
  }

  def InterfaceTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ interface ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ FieldsDefinition ~> (
      (comment, location, name, dirs, fields) ⇒ ast.InterfaceTypeExtensionDefinition(name, fields._1.toVector, dirs, comment, fields._2, location))) |
    (Comments ~ trackPos ~ extend ~ interface ~ Name ~ DirectivesConst ~> (
      (comment, location, name, dirs) ⇒ ast.InterfaceTypeExtensionDefinition(name, Vector.empty, dirs, comment, Vector.empty, location)))
  }

  def UnionTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ union ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ UnionMemberTypes ~> (
      (comment, location, name, dirs, types) ⇒ ast.UnionTypeExtensionDefinition(name, types, dirs, comment, location))) |
    (Comments ~ trackPos ~ extend ~ union ~ Name ~ DirectivesConst ~> (
      (comment, location, name, dirs) ⇒ ast.UnionTypeExtensionDefinition(name, Vector.empty, dirs, comment, location)))
  }

  def EnumTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ enum ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ EnumValuesDefinition ~> (
      (comment, location, name, dirs, values) ⇒ ast.EnumTypeExtensionDefinition(name, values._1.toVector, dirs, comment, values._2, location))) |
    (Comments ~ trackPos ~ extend ~ enum ~ Name ~ DirectivesConst ~> (
      (comment, location, name, dirs) ⇒ ast.EnumTypeExtensionDefinition(name, Vector.empty, dirs, comment, Vector.empty, location)))
  }

  def InputObjectTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ inputType ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ InputFieldsDefinition ~> (
      (comment, location, name, dirs, fields) ⇒ ast.InputObjectTypeExtensionDefinition(name, fields._1.toVector, dirs, comment, fields._2, location))) |
    (Comments ~ trackPos ~ extend ~ inputType ~ Name ~ DirectivesConst ~> (
      (comment, location, name, dirs) ⇒ ast.InputObjectTypeExtensionDefinition(name, Vector.empty, dirs, comment, Vector.empty, location)))
  }

  def ScalarTypeExtensionDefinition = rule {
    (Comments ~ trackPos ~ extend ~ scalar ~ Name ~ DirectivesConst ~> (
      (comment, location, name, dirs) ⇒ ast.ScalarTypeExtensionDefinition(name, dirs, comment, location)))
  }

  def ImplementsInterfaces = rule {
    test(legacyImplementsInterface) ~ implements ~ NamedType.+ ~> (_.toVector) |
    implements ~ ws('&').? ~ NamedType.+(ws('&')) ~> (_.toVector)
  }

  def FieldsDefinition = rule {
    wsNoComment('{') ~ (test(legacyEmptyFields) ~ FieldDefinition.* | FieldDefinition.+) ~ Comments ~ wsNoComment('}') ~> (_ → _)
  }

  def FieldDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ (ArgumentsDefinition.? ~> (_ getOrElse Vector.empty)) ~ ws(':') ~ Type ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~> (
      (descr, comment, location, name, args, fieldType, dirs) ⇒ ast.FieldDefinition(name, fieldType, args, dirs, descr, comment, location))
  }

  def ArgumentsDefinition = rule { wsNoComment('(') ~ InputValueDefinition.+ ~ wsNoComment(')') ~> (_.toVector) }

  def InputValueDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ ws(':') ~ Type ~ DefaultValue.? ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~> (
      (descr, comment, location, name, valueType, default, dirs) ⇒ ast.InputValueDefinition(name, valueType, default, dirs, descr, comment, location))
  }

  def InterfaceTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ interface ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ FieldsDefinition.? ~> (
      (descr, comment, location, name, dirs, fields) ⇒ ast.InterfaceTypeDefinition(name, fields.fold(Vector.empty[ast.FieldDefinition])(_._1.toVector), dirs, descr, comment, fields.fold(Vector.empty[ast.Comment])(_._2), location))
  }

  def UnionTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ union ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ (UnionMemberTypes.? ~> (_ getOrElse Vector.empty)) ~> (
      (descr, comment, location, name, dirs, members) ⇒ ast.UnionTypeDefinition(name, members, dirs, descr, comment, location))
  }

  def UnionMemberTypes = rule { wsNoComment('=') ~ UnionMembers }

  def UnionMembers = rule { ws('|').? ~ NamedType.+(ws('|')) ~> (_.toVector) }

  def EnumTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ enum ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ EnumValuesDefinition.? ~> (
      (descr, comment, location, name, dirs, values) ⇒ ast.EnumTypeDefinition(name, values.fold(Vector.empty[ast.EnumValueDefinition])(_._1.toVector), dirs, descr, comment, values.fold(Vector.empty[ast.Comment])(_._2), location))
  }

  def EnumValuesDefinition = rule { wsNoComment('{') ~ EnumValueDefinition.+ ~ Comments ~ wsNoComment('}') ~> (_ → _) }

  def EnumValueDefinition = rule {
    Description ~ Comments ~ trackPos ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~> ((descr, comments, location, name, dirs) ⇒ ast.EnumValueDefinition(name, dirs, descr, comments, location))
  }

  def InputObjectTypeDefinition = rule {
    Description ~ Comments ~ trackPos ~ inputType ~ Name ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ InputFieldsDefinition.? ~> (
      (descr, comment, location, name, dirs, fields) ⇒ ast.InputObjectTypeDefinition(name, fields.fold(Vector.empty[ast.InputValueDefinition])(_._1.toVector), dirs, descr, comment, fields.fold(Vector.empty[ast.Comment])(_._2), location))
  }

  def InputFieldsDefinition = rule {
    wsNoComment('{') ~ (test(legacyEmptyFields) ~ InputValueDefinition.* | InputValueDefinition.+) ~ Comments ~ wsNoComment('}') ~> (_ → _)
  }

  def DirectiveDefinition = rule {
    Description ~ Comments ~ trackPos ~ directive ~ '@' ~ NameStrict ~ (ArgumentsDefinition.? ~> (_ getOrElse Vector.empty)) ~ on ~ DirectiveLocations ~> (
      (descr, comment, location, name, args, locations) ⇒ ast.DirectiveDefinition(name, args, locations, descr, comment, location))
  }

  def DirectiveLocations = rule { ws('|').? ~ DirectiveLocation.+(wsNoComment('|')) ~> (_.toVector) }

  def DirectiveLocation = rule { Comments ~ trackPos ~ DirectiveLocationName ~> ((comment, location, name) ⇒ ast.DirectiveLocation(name, comment, location)) }

  def DirectiveLocationName = rule {
    TypeSystemDirectiveLocation | ExecutableDirectiveLocation
  }

  def ExecutableDirectiveLocation = rule {
    wsCapture("QUERY") |
    wsCapture("MUTATION") |
    wsCapture("SUBSCRIPTION") |
    wsCapture("FIELD") |
    wsCapture("FRAGMENT_DEFINITION") |
    wsCapture("FRAGMENT_SPREAD") |
    wsCapture("INLINE_FRAGMENT")
  }

  def TypeSystemDirectiveLocation = rule {
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
    wsCapture("INPUT_FIELD_DEFINITION")
  }

  def SchemaDefinition = rule {
    Comments ~ trackPos ~ schema ~ (DirectivesConst.? ~> (_ getOrElse Vector.empty)) ~ wsNoComment('{') ~ OperationTypeDefinition.+ ~ Comments ~ wsNoComment('}') ~> (
      (comment, location, dirs, ops, tc) ⇒ ast.SchemaDefinition(ops.toVector, dirs, comment, tc, location))
  }

  def OperationTypeDefinition = rule {
    Comments ~ trackPos ~ OperationType ~ ws(':') ~ NamedType ~> (
      (comment, location, opType, tpe) ⇒ ast.OperationTypeDefinition(opType, tpe, comment, location))
  }

  def Description = rule { StringValue.? }

}

trait Operations extends PositionTracking { this: Parser with Tokens with Ignored with Fragments with Values with Types with Directives ⇒

  def OperationDefinition = rule {
    Comments ~ trackPos ~ SelectionSet ~> ((comment, location, s) ⇒ ast.OperationDefinition(selections = s._1, comments = comment, trailingComments = s._2, location = location)) |
    Comments ~ trackPos ~ OperationType ~ OperationName.? ~ (VariableDefinitions.? ~> (_ getOrElse Vector.empty)) ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~ SelectionSet ~>
        ((comment, location, opType, name, vars, dirs, sels) ⇒ ast.OperationDefinition(opType, name, vars, dirs, sels._1, comment, sels._2, location))
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

  def VariableDefinitions = rule { wsNoComment('(') ~ VariableDefinition.+ ~ wsNoComment(')') ~> (_.toVector)}

  def VariableDefinition = rule { Comments ~ trackPos ~ Variable ~ ws(':') ~ Type ~ DefaultValue.? ~>
      ((comment, location, name, tpe, defaultValue) ⇒ ast.VariableDefinition(name, tpe, defaultValue, comment, location)) }

  def Variable = rule { Ignored.* ~ '$' ~ NameStrict }

  def DefaultValue = rule { wsNoComment('=') ~ ValueConst }

  def SelectionSet: Rule1[(Vector[ast.Selection], Vector[ast.Comment])] = rule {
    wsNoComment('{') ~ Selection.+ ~ Comments ~ wsNoComment('}') ~>
        ((x: Seq[ast.Selection], comments: Vector[ast.Comment]) ⇒ x.toVector → comments)
  }

  def Selection = rule { Field | FragmentSpread | InlineFragment }

  def Field = rule {
    Comments ~ trackPos ~ Alias.? ~ Name ~
      (Arguments.? ~> (_ getOrElse Vector.empty)) ~
      (Directives.? ~> (_ getOrElse Vector.empty)) ~
      (SelectionSet.? ~> (_ getOrElse (Vector.empty → Vector.empty))) ~>
        ((comment, location, alias, name, args, dirs, sels) ⇒ ast.Field(alias, name, args, dirs, sels._1, comment, sels._2, location))
  }

  def Alias = rule { Name ~ ws(':') }

  def Arguments = rule { Ignored.* ~ wsNoComment('(') ~ Argument.+ ~ wsNoComment(')') ~> (_.toVector) }

  def ArgumentsConst = rule { Ignored.* ~ wsNoComment('(') ~ ArgumentConst.+ ~ wsNoComment(')') ~> (_.toVector) }

  def Argument = rule { Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> ((comment, location, name, value) ⇒ ast.Argument(name, value, comment, location)) }

  def ArgumentConst = rule { Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> ((comment, location, name, value) ⇒ ast.Argument(name, value, comment, location)) }

}

trait Fragments { this: Parser with Tokens with Ignored with Directives with Types with Operations ⇒

  def experimentalFragmentVariables: Boolean

  def FragmentSpread = rule { Comments ~ trackPos ~ Ellipsis ~ FragmentName ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~>
      ((comment, location, name, dirs) ⇒ ast.FragmentSpread(name, dirs, comment, location)) }

  def InlineFragment = rule { Comments ~ trackPos ~ Ellipsis ~ TypeCondition.? ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~ SelectionSet ~>
      ((comment, location, typeCondition, dirs, sels) ⇒ ast.InlineFragment(typeCondition, dirs, sels._1, comment, sels._2, location)) }

  def on = rule { Keyword("on") }

  def Fragment = rule { Keyword("fragment") }

  def FragmentDefinition = rule { Comments ~ trackPos ~ Fragment ~ FragmentName ~ ExperimentalFragmentVariables ~ TypeCondition ~ (Directives.? ~> (_ getOrElse Vector.empty)) ~ SelectionSet ~>
    ((comment, location, name, vars, typeCondition, dirs, sels) ⇒ ast.FragmentDefinition(name, typeCondition, dirs, sels._1, vars, comment, sels._2, location)) }

  def ExperimentalFragmentVariables = rule {
    test(experimentalFragmentVariables) ~ VariableDefinitions.? ~> (_ getOrElse Vector.empty) | push(Vector.empty)
  }

  def FragmentName = rule { !on ~ Name }

  def TypeCondition = rule { on ~ NamedType }

}

trait Values { this: Parser with Tokens with Ignored with Operations ⇒

  def ValueConst: Rule1[ast.Value] = rule {
    NumberValue | StringValue | BooleanValue | NullValue | EnumValue | ListValueConst | ObjectValueConst
  }

  def Value: Rule1[ast.Value] = rule {
    Comments ~ trackPos ~ Variable ~> ((comment, location, name) ⇒ ast.VariableValue(name, comment, location)) |
    NumberValue |
    StringValue |
    BooleanValue |
    NullValue |
    EnumValue |
    ListValue |
    ObjectValue
  }

  def BooleanValue = rule {
    Comments ~ trackPos ~ True ~> ((comment, location) ⇒ ast.BooleanValue(true, comment, location)) |
    Comments ~ trackPos ~ False ~> ((comment, location) ⇒ ast.BooleanValue(false, comment, location))
  }

  def True = rule { Keyword("true") }

  def False = rule { Keyword("false") }

  def Null = rule { Keyword("null") }

  def NullValue = rule { Comments ~ trackPos ~ Null ~> ((comment, location) ⇒ ast.NullValue(comment, location)) }

  def EnumValue = rule { Comments ~ !(True | False) ~ trackPos ~ Name ~> ((comment, location, name) ⇒ ast.EnumValue(name, comment, location)) }

  def ListValueConst = rule { Comments ~ trackPos ~ wsNoComment('[') ~ ValueConst.* ~ wsNoComment(']')  ~> ((comment, location, v) ⇒ ast.ListValue(v.toVector, comment, location)) }

  def ListValue = rule { Comments ~ trackPos ~ wsNoComment('[') ~ Value.* ~ wsNoComment(']') ~> ((comment, location, v) ⇒ ast.ListValue(v.toVector, comment, location)) }

  def ObjectValueConst = rule { Comments ~ trackPos ~ wsNoComment('{') ~ ObjectFieldConst.* ~ wsNoComment('}') ~> ((comment, location, f) ⇒ ast.ObjectValue(f.toVector, comment, location)) }

  def ObjectValue = rule { Comments ~ trackPos ~ wsNoComment('{') ~ ObjectField.* ~ wsNoComment('}') ~> ((comment, location, f) ⇒ ast.ObjectValue(f.toVector, comment, location)) }

  def ObjectFieldConst = rule { Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ ValueConst ~> ((comment, location, name, value) ⇒ ast.ObjectField(name, value, comment, location)) }

  def ObjectField = rule { Comments ~ trackPos ~ Name ~ wsNoComment(':') ~ Value ~> ((comment, location, name, value) ⇒ ast.ObjectField(name, value, comment, location)) }

}

trait Directives { this: Parser with Tokens with Operations with Ignored ⇒
  
  def Directives = rule { Directive.+ ~> (_.toVector) }

  def DirectivesConst = rule { DirectiveConst.+ ~> (_.toVector) }

  def Directive = rule { Comments ~ trackPos ~ '@' ~ NameStrict ~ (Arguments.? ~> (_ getOrElse Vector.empty)) ~>
    ((comment, location, name, args) ⇒ ast.Directive(name, args, comment, location)) }

  def DirectiveConst = rule { Comments ~ trackPos ~ '@' ~ NameStrict ~ (ArgumentsConst.? ~> (_ getOrElse Vector.empty)) ~>
    ((comment, location, name, args) ⇒ ast.Directive(name, args, comment, location)) }

}

trait Types { this: Parser with Tokens with Ignored ⇒
  def Type: Rule1[ast.Type] = rule { NonNullType | ListType | NamedType }

  def TypeName = rule { Name }

  def NamedType = rule { Ignored.* ~ trackPos ~ TypeName ~> ((location, name) ⇒ ast.NamedType(name, location))}

  def ListType = rule { trackPos ~ ws('[') ~ Type ~ wsNoComment(']') ~> ((location, tpe) ⇒ ast.ListType(tpe, location)) }

  def NonNullType = rule {
    trackPos ~ TypeName ~ wsNoComment('!')  ~> ((location, name) ⇒ ast.NotNullType(ast.NamedType(name, location), location)) |
    trackPos ~ ListType ~ wsNoComment('!') ~> ((location, tpe) ⇒ ast.NotNullType(tpe, location))
  }
}

class QueryParser private (
  val input: ParserInput,
  val sourceId: String,
  val legacyImplementsInterface: Boolean = false,
  val legacyEmptyFields: Boolean = false,
  val experimentalFragmentVariables: Boolean = false,
  val parseLocations: Boolean = true,
  val parseComments: Boolean = true
) extends Parser with Tokens with Ignored with Document with Operations with Fragments with Values with Directives with Types with TypeSystemDefinitions

object QueryParser {
  def parse(input: String, config: ParserConfig = ParserConfig.default)(implicit scheme: DeliveryScheme[ast.Document]): scheme.Result = {
    parse(ParserInput(input), config)(scheme)
  }

  def parse(input: ParserInput, config: ParserConfig)(implicit scheme: DeliveryScheme[ast.Document]): scheme.Result = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(
      input,
      id,
      config.legacyImplementsInterface,
      config.legacyEmptyFields,
      config.experimentalFragmentVariables,
      config.parseLocations,
      config.parseComments)

    parser.Document.run() match {
      case Success(res) ⇒ scheme.success(res.copy(sourceMapper = config.sourceMapperFn(id, input)))
      case Failure(e: ParseError) ⇒ scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) ⇒ scheme.failure(e)
    }
  }

  def parseInput(input: String)(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result =
    parseInput(ParserInput(input))(scheme)

  def parseInput(input: ParserInput)(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result = {
    val parser = new QueryParser(input, "")

    parser.InputDocument.run() match {
      case Success(res) if res.values.nonEmpty ⇒ scheme.success(res.values.head)
      case Success(res) ⇒ scheme.failure(new IllegalArgumentException("Input document does not contain any value definitions."))
      case Failure(e: ParseError) ⇒ scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) ⇒ scheme.failure(e)
    }
  }

  def parseInputDocument(input: String, config: ParserConfig = ParserConfig.default)(implicit scheme: DeliveryScheme[ast.InputDocument]): scheme.Result =
    parseInputDocument(ParserInput(input), config)(scheme)

  def parseInputDocument(input: ParserInput, config: ParserConfig)(implicit scheme: DeliveryScheme[ast.InputDocument]): scheme.Result = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(
      input,
      id,
      config.legacyImplementsInterface,
      config.legacyEmptyFields,
      config.experimentalFragmentVariables,
      config.parseLocations,
      config.parseComments)

    parser.InputDocument.run() match {
      case Success(res) ⇒ scheme.success(res.copy(sourceMapper = config.sourceMapperFn(id, input)))
      case Failure(e: ParseError) ⇒ scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) ⇒ scheme.failure(e)
    }
  }

  def parseInputWithVariables(input: String)(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result =
    parseInputWithVariables(ParserInput(input))( scheme)

  def parseInputWithVariables(input: ParserInput)(implicit scheme: DeliveryScheme[ast.Value]): scheme.Result = {
    val parser = new QueryParser(input, "")

    parser.InputDocumentWithVariables.run() match {
      case Success(res) if res.values.nonEmpty ⇒ scheme.success(res.values.head)
      case Success(res) ⇒ scheme.failure(new IllegalArgumentException("Input document does not contain any value definitions."))
      case Failure(e: ParseError) ⇒ scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) ⇒ scheme.failure(e)
    }
  }

  def parseInputDocumentWithVariables(input: String, config: ParserConfig = ParserConfig.default)(implicit scheme: DeliveryScheme[ast.InputDocument]): scheme.Result =
    parseInputDocumentWithVariables(ParserInput(input), config)( scheme)

  def parseInputDocumentWithVariables(input: ParserInput, config: ParserConfig)(implicit scheme: DeliveryScheme[ast.InputDocument]): scheme.Result = {
    val id = config.sourceIdFn(input)
    val parser = new QueryParser(input, id)

    parser.InputDocumentWithVariables.run() match {
      case Success(res) ⇒ scheme.success(res.copy(sourceMapper = config.sourceMapperFn(id, input)))
      case Failure(e: ParseError) ⇒ scheme.failure(SyntaxError(parser, input, e))
      case Failure(e) ⇒ scheme.failure(e)
    }
  }
}

case class ParserConfig(
  legacyImplementsInterface: Boolean = false,
  legacyEmptyFields: Boolean = false,
  experimentalFragmentVariables: Boolean = false,
  sourceIdFn: ParserInput ⇒ String = ParserConfig.defaultSourceIdFn,
  sourceMapperFn: (String, ParserInput) ⇒ Option[SourceMapper] = ParserConfig.defaultSourceMapperFn,
  parseLocations: Boolean = true,
  parseComments: Boolean = true
) {
  @deprecated("Use new syntax: `type Foo implements Bar & Baz`", "1.4.0")
  def withLegacyImplementsInterface: ParserConfig = copy(legacyImplementsInterface = true)

  @deprecated("Use new syntax: `type Foo` instead of legacy `type Foo {}`", "1.4.0")
  def withLegacyEmptyFields: ParserConfig = copy(legacyEmptyFields = true)

  def withExperimentalFragmentVariables: ParserConfig = copy(experimentalFragmentVariables = true)

  def withEmptySourceId: ParserConfig = copy(sourceIdFn = ParserConfig.emptySourceIdFn)

  def withSourceMapper(fn: (String, ParserInput) ⇒ Option[SourceMapper]): ParserConfig = copy(sourceMapperFn = fn)

  def withoutSourceMapper: ParserConfig = copy(sourceMapperFn = ParserConfig.emptySourceMapperFn)

  def withoutLocations: ParserConfig = copy(parseLocations = false)

  def withoutComments: ParserConfig = copy(parseComments = false)
}

object ParserConfig {
  lazy val default: ParserConfig = ParserConfig()

  lazy val emptySourceIdFn: ParserInput ⇒ String = _ ⇒ ""
  lazy val defaultSourceIdFn: ParserInput ⇒ String = _ ⇒ UUID.randomUUID().toString

  lazy val emptySourceMapperFn: (String, ParserInput) ⇒ Option[SourceMapper] = (_, _) ⇒ None
  lazy val defaultSourceMapperFn: (String, ParserInput) ⇒ Option[SourceMapper] =
    (id, input) ⇒ Some(new DefaultSourceMapper(id, input))
}