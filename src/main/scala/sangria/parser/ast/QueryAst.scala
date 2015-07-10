package sangria.parser.ast

import org.parboiled2.Position

case class Document(definitions: List[Definition], position: Option[Position] = None) extends Positional

sealed trait Definition extends Positional

case class OperationDefinition(
  operationType: OperationType = OperationType.Query,
  name: Option[String] = None,
  variables: List[VariableDefinition] = Nil,
  directives: List[Directive] = Nil,
  selections: List[Selection],
  position: Option[Position] = None) extends Definition

case class FragmentDefinition(
  name: String,
  typeCondition: String,
  directives: List[Directive],
  selections: List[Selection],
  position: Option[Position] = None) extends Definition

sealed trait OperationType

object OperationType {
  case object Query extends OperationType
  case object Mutation extends OperationType
}

case class VariableDefinition(
  name: String,
  tpe: Type,
  defaultValue: Option[Value],
  position: Option[Position] = None) extends Positional

case class Type(
  name: String,
  isList: Boolean,
  isNotNull: Boolean,
  position: Option[Position] = None) extends Positional

sealed trait Selection extends Positional

case class Field(
  alias: Option[String],
  name: String,
  arguments: List[Argument],
  directives: List[Directive],
  selections: List[Selection],
  position: Option[Position] = None) extends Selection
case class FragmentSpread(
  name: String,
  directives: List[Directive],
  position: Option[Position] = None) extends Selection
case class InlineFragment(
  typeCondition: String,
  directives: List[Directive],
  selections: List[Selection],
  position: Option[Position] = None) extends Selection

sealed trait NameValue extends Positional {
  def name: String
  def value: Value
}

case class Directive(name: String, arguments: List[Argument], position: Option[Position] = None) extends Positional
case class Argument(name: String, value: Value, position: Option[Position] = None) extends NameValue

sealed trait Value extends Positional

case class IntValue(value: Int, position: Option[Position] = None) extends Value
case class FloatValue(value: Double, position: Option[Position] = None) extends Value
case class StringValue(value: String, position: Option[Position] = None) extends Value
case class BooleanValue(value: Boolean, position: Option[Position] = None) extends Value
case class EnumValue(value: String, position: Option[Position] = None) extends Value
case class ArrayValue(value: List[Value], position: Option[Position] = None) extends Value
case class ObjectValue(value: List[ObjectField], position: Option[Position] = None) extends Value
case class VariableValue(name: String, position: Option[Position] = None) extends Value

case class ObjectField(name: String, value: Value, position: Option[Position] = None) extends NameValue

trait Positional {
  def position: Option[Position]
}