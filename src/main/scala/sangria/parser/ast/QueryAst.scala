package sangria.parser.ast

case class Document(definitions: List[Definition])

sealed trait Definition

case class OperationDefinition(
  operationType: OperationType = OperationType.Query,
  name: Option[String] = None,
  variables: List[VariableDefinition] = Nil,
  directives: List[Directive] = Nil,
  selections: List[Selection]) extends Definition

case class FragmentDefinition(
  name: String,
  typeCondition: String,
  directives: List[Directive],
  selections: List[Selection]) extends Definition

sealed trait OperationType

object OperationType {
  case object Query extends OperationType
  case object Mutation extends OperationType
}

case class VariableDefinition(name: String, tpe: Type, defaultValue: Option[Value])

case class Type(name: String, isList: Boolean, isNotNull: Boolean)

sealed class Selection

case class Field(
  alias: Option[String],
  name: String,
  arguments: List[Argument],
  directives: List[Directive],
  selections: List[Selection]) extends Selection
case class FragmentSpread(name: String, directives: List[Directive]) extends Selection
case class InlineFragment(
  typeCondition: String,
  directives: List[Directive],
  selections: List[Selection]) extends Selection

sealed trait NameValue {
  def name: String
  def value: Value
}

case class Directive(name: String, arguments: List[Argument])
case class Argument(name: String, value: Value) extends NameValue

sealed trait Value

case class IntValue(value: Int) extends Value
case class FloatValue(value: Double) extends Value
case class StringValue(value: String) extends Value
case class BooleanValue(value: Boolean) extends Value
case class EnumValue(value: String) extends Value
case class ArrayValue(value: List[Value]) extends Value
case class ObjectValue(value: List[ObjectField]) extends Value
case class VariableValue(name: String) extends Value

case class ObjectField(name: String, value: Value) extends NameValue