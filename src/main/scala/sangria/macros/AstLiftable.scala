package sangria.macros

import sangria.ast._

import scala.reflect.api.Universe
import scala.reflect.macros.blackbox

/**
 * Implements the Liftable type class for AstNode, so we can use them in
 * quasiquotes.
 */
trait AstLiftable {
  val universe: Universe

  import universe._

  implicit def liftPosition: Liftable[org.parboiled2.Position] = Liftable {
    case org.parboiled2.Position(i, l, c) =>
      q"_root_.org.parboiled2.Position($i, $l, $c)"
  }

  implicit def liftOperationType: Liftable[OperationType] = Liftable {
    case OperationType.Query => q"_root_.sangria.ast.OperationType.Query"
    case OperationType.Mutation => q"_root_.sangria.ast.OperationType.Mutation"
  }

  implicit def liftType[T <: sangria.ast.Type]: Liftable[T] = Liftable {
    case NamedType(n, p) => q"_root_.sangria.ast.NamedType($n, $p)"
    case NotNullType(o, p) => q"_root_.sangria.ast.NotNullType($o, $p)"
    case ListType(o, p) => q"_root_.sangria.ast.ListType($o, $p)"
  }

  implicit def liftVarDef: Liftable[VariableDefinition] = Liftable {
    case VariableDefinition(n, t, d, p) =>
      q"_root_.sangria.ast.VariableDefinition($n, $t, $d, $p)"
  }

  implicit def liftDefinition[T <: Definition]: Liftable[T] = Liftable {
    case OperationDefinition(o, n, v, d, s, p) =>
      q"_root_.sangria.ast.OperationDefinition($o, $n, $v, $d, $s, $p)"
    case FragmentDefinition(n, t, d, s, p) =>
      q"_root_.sangria.ast.FragmentDefinition($n, $t, $d, $s, $p)"
  }

  implicit def liftNamedValue[T <: NameValue]: Liftable[T] = Liftable {
    case Argument(n, v, p) => q"_root_.sangria.ast.Argument($n, $v, $p)"
    case ObjectField(n, v, p) => q"_root_.sangria.ast.ObjectField($n, $v, $p)"
  }

  implicit def liftValue[T <: sangria.ast.Value]: Liftable[T] = Liftable {
    case IntValue(v, p) => q"_root_.sangria.ast.IntValue($v, $p)"
    case FloatValue(v, p) => q"_root_.sangria.ast.FloatValue($v, $p)"
    case StringValue(v, p) => q"_root_.sangria.ast.StringValue($v, $p)"
    case BooleanValue(v, p) => q"_root_.sangria.ast.BooleanValue($v, $p)"
    case EnumValue(v, p) => q"_root_.sangria.ast.EnumValue($v, $p)"
    case ListValue(v, p) => q"_root_.sangria.ast.ListValue($v, $p)"
    case ObjectValue(f, p) => q"_root_.sangria.ast.ObjectValue($f, $p)"
    case VariableValue(n, p) => q"_root_.sangria.ast.VariableValue($n, $p)"
    case BigIntValue(v, p) =>
      q"_root_.sangria.ast.BigIntValue(_root_.scala.math.BigInt(${v.toByteArray}), $p)"
    case sangria.ast.BigDecimalValue(v, p) =>
      q"_root_.sangria.ast.BigDecimalValue(_root_.scala.math.BigDecimal(${v.toString()}), $p)"
  }

  implicit def directive: Liftable[sangria.ast.Directive] = Liftable {
    case Directive(n, a, p) => q"_root_.sangria.ast.Directive($n, $a, $p)"
  }

  implicit def selection[T <: Selection]: Liftable[T] = Liftable {
    case Field(a, n, arg, d, s, p) =>
      q"_root_.sangria.ast.Field($a, $n, $arg, $d, $s, $p)"
    case FragmentSpread(n, d, p) =>
      q"_root_.sangria.ast.FragmentSpread($n, $d, $p)"
    case InlineFragment(t, d, s, p) =>
      q"_root_.sangria.ast.InlineFragment($t, $d, $s, $p)"
  }

  implicit def liftDocument: Liftable[Document] = Liftable {
    case doc @ Document(d, p, _) => q"_root_.sangria.ast.Document($d, $p, _root_.scala.Some(new _root_.sangria.parser.Parboiled2SourceMapper(_root_.org.parboiled2.ParserInput(${doc.source.get}))))"
  }
}

trait MacroAstLiftable extends AstLiftable {
  val c: blackbox.Context
  val universe: c.universe.type = c.universe
}