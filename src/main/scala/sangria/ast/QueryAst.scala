package sangria.ast

import org.parboiled2.Position
import sangria.parser.SourceMapper

import scala.collection.immutable.ListMap

case class Document(definitions: List[Definition], position: Option[Position] = None, sourceMapper: Option[SourceMapper] = None) extends AstNode {
  lazy val operations = Map(definitions collect {case op: OperationDefinition ⇒ op.name → op}: _*)
  lazy val fragments = Map(definitions collect {case fragment: FragmentDefinition ⇒ fragment.name → fragment}: _*)
  lazy val source = sourceMapper map (_.source)

  def operationType(operationName: Option[String] = None): Option[OperationType] =
    operation(operationName) map (_.operationType)

  def operation(operationName: Option[String] = None): Option[OperationDefinition] =
    if (operations.size != 1 && operationName.isEmpty)
      None
    else
      operationName flatMap (opName ⇒ operations get Some(opName)) orElse operations.values.headOption

  override def canEqual(other: Any): Boolean = other.isInstanceOf[Document]

  override def equals(other: Any): Boolean = other match {
    case that: Document ⇒
      (that canEqual this) &&
        definitions == that.definitions &&
        position == that.position
    case _ ⇒ false
  }

  override def hashCode(): Int =
    Seq(definitions, position).map(_.hashCode()).foldLeft(0)((a, b) ⇒ 31 * a + b)
}

sealed trait ConditionalFragment extends AstNode {
  def typeConditionOpt: Option[NamedType]
}

sealed trait SelectionContainer extends AstNode {
  def selections: List[Selection]
  def position: Option[Position]
}

sealed trait Definition extends AstNode

case class OperationDefinition(
  operationType: OperationType = OperationType.Query,
  name: Option[String] = None,
  variables: List[VariableDefinition] = Nil,
  directives: List[Directive] = Nil,
  selections: List[Selection],
  position: Option[Position] = None) extends Definition with WithDirectives with SelectionContainer

case class FragmentDefinition(
    name: String,
    typeCondition: NamedType,
    directives: List[Directive],
    selections: List[Selection],
    position: Option[Position] = None) extends Definition with ConditionalFragment with WithDirectives with SelectionContainer {
  lazy val typeConditionOpt = Some(typeCondition)
}

sealed trait OperationType

object OperationType {
  case object Query extends OperationType
  case object Mutation extends OperationType
  case object Subscription extends OperationType
}

case class VariableDefinition(
  name: String,
  tpe: Type,
  defaultValue: Option[Value],
  position: Option[Position] = None) extends AstNode

sealed trait Type extends AstNode

case class NamedType(name: String, position: Option[Position] = None) extends Type
case class NotNullType(ofType: Type, position: Option[Position] = None) extends Type
case class ListType(ofType: Type, position: Option[Position] = None) extends Type

sealed trait Selection extends AstNode with WithDirectives

case class Field(
    alias: Option[String],
    name: String,
    arguments: List[Argument],
    directives: List[Directive],
    selections: List[Selection],
    position: Option[Position] = None) extends Selection with SelectionContainer {
  lazy val outputName = alias getOrElse name
}

case class FragmentSpread(
  name: String,
  directives: List[Directive],
  position: Option[Position] = None) extends Selection

case class InlineFragment(
    typeCondition: Option[NamedType],
    directives: List[Directive],
    selections: List[Selection],
    position: Option[Position] = None) extends Selection with ConditionalFragment with SelectionContainer {
  def typeConditionOpt = typeCondition
}

sealed trait NameValue extends AstNode {
  def name: String
  def value: Value
}

sealed trait WithDirectives {
  def directives: List[Directive]
}

case class Directive(name: String, arguments: List[Argument], position: Option[Position] = None) extends AstNode
case class Argument(name: String, value: Value, position: Option[Position] = None) extends NameValue

sealed trait Value extends AstNode
sealed trait ScalarValue extends Value

case class IntValue(value: Int, position: Option[Position] = None) extends ScalarValue
case class BigIntValue(value: BigInt, position: Option[Position] = None) extends ScalarValue
case class FloatValue(value: Double, position: Option[Position] = None) extends ScalarValue
case class BigDecimalValue(value: BigDecimal, position: Option[Position] = None) extends ScalarValue
case class StringValue(value: String, position: Option[Position] = None) extends ScalarValue
case class BooleanValue(value: Boolean, position: Option[Position] = None) extends ScalarValue
case class EnumValue(value: String, position: Option[Position] = None) extends Value
case class ListValue(values: List[Value], position: Option[Position] = None) extends Value
case class VariableValue(name: String, position: Option[Position] = None) extends Value
case class NullValue(position: Option[Position] = None) extends Value
case class ObjectValue(fields: List[ObjectField], position: Option[Position] = None) extends Value {
  lazy val fieldsByName =
    fields.foldLeft(ListMap.empty[String, Value]) {
      case (acc, field) ⇒ acc + (field.name → field.value)
    }
}

case class ObjectField(name: String, value: Value, position: Option[Position] = None) extends NameValue

sealed trait AstNode {
  def position: Option[Position]
  def cacheKeyHash: Int = System.identityHashCode(this)
}

object AstNode {
  def withoutPosition[T <: AstNode](node: T): T = node match {
    case n: Document ⇒ n.copy(definitions = n.definitions map withoutPosition, position = None, sourceMapper = None).asInstanceOf[T]
    case n: OperationDefinition ⇒
      n.copy(
        variables = n.variables map withoutPosition,
        directives = n.directives map withoutPosition,
        selections = n.selections map withoutPosition,
        position = None).asInstanceOf[T]
    case n: FragmentDefinition ⇒
      n.copy(
        typeCondition = withoutPosition(n.typeCondition),
        directives = n.directives map withoutPosition,
        selections = n.selections map withoutPosition,
        position = None).asInstanceOf[T]
    case n: VariableDefinition ⇒
      n.copy(
        tpe = withoutPosition(n.tpe),
        defaultValue = n.defaultValue map withoutPosition,
        position = None).asInstanceOf[T]
    case n: NamedType ⇒ n.copy(position = None).asInstanceOf[T]
    case n: NotNullType ⇒ n.copy(ofType = withoutPosition(n.ofType), position = None).asInstanceOf[T]
    case n: ListType ⇒ n.copy(ofType = withoutPosition(n.ofType), position = None).asInstanceOf[T]
    case n: Field ⇒
      n.copy(
        arguments = n.arguments map withoutPosition,
        directives = n.directives map withoutPosition,
        selections = n.selections map withoutPosition,
        position = None).asInstanceOf[T]
    case n: FragmentSpread ⇒
      n.copy(
        directives = n.directives map withoutPosition,
        position = None).asInstanceOf[T]
    case n: InlineFragment ⇒
      n.copy(
        typeCondition = n.typeCondition map withoutPosition,
        directives = n.directives map withoutPosition,
        selections = n.selections map withoutPosition,
        position = None).asInstanceOf[T]
    case n: Directive ⇒
      n.copy(
        arguments = n.arguments map withoutPosition,
        position = None).asInstanceOf[T]
    case n: Argument ⇒ n.copy(value = withoutPosition(n.value), position = None).asInstanceOf[T]
    case n: IntValue ⇒ n.copy(position = None).asInstanceOf[T]
    case n: BigIntValue ⇒ n.copy(position = None).asInstanceOf[T]
    case n: FloatValue ⇒ n.copy(position = None).asInstanceOf[T]
    case n: BigDecimalValue ⇒ n.copy(position = None).asInstanceOf[T]
    case n: StringValue ⇒ n.copy(position = None).asInstanceOf[T]
    case n: BooleanValue ⇒ n.copy(position = None).asInstanceOf[T]
    case n: NullValue ⇒ n.copy(position = None).asInstanceOf[T]
    case n: EnumValue ⇒ n.copy(position = None).asInstanceOf[T]
    case n: ListValue ⇒
      n.copy(
        values = n.values map withoutPosition,
        position = None).asInstanceOf[T]
    case n: ObjectValue ⇒ n.copy(fields = n.fields map withoutPosition, position = None).asInstanceOf[T]
    case n: ObjectField ⇒ n.copy(value = withoutPosition(n.value), position = None).asInstanceOf[T]
    case n: VariableValue ⇒ n.copy(position = None).asInstanceOf[T]
  }
}