package sangria.macros

import sangria.ast._

import scala.annotation.nowarn
import scala.reflect.api.Universe
import scala.reflect.macros.blackbox

/** Implements the Liftable type class for AstNode, so we can use them in quasiquotes.
  */
trait AstLiftable {
  val universe: Universe

  import universe._

  implicit def liftSeq[T: Liftable]: Liftable[Seq[T]] = Liftable { seq =>
    q"_root_.scala.collection.immutable.Vector(..$seq)"
  }

  implicit def liftPosition: Liftable[sangria.ast.AstLocation] = Liftable {
    case sangria.ast.AstLocation(id, i, l, c) =>
      q"_root_.sangria.ast.AstLocation($id, $i, $l, $c)"
  }

  implicit def liftOperationType: Liftable[OperationType] = Liftable {
    case OperationType.Query => q"_root_.sangria.ast.OperationType.Query"
    case OperationType.Mutation => q"_root_.sangria.ast.OperationType.Mutation"
    case OperationType.Subscription => q"_root_.sangria.ast.OperationType.Subscription"
  }

  @nowarn("msg=Implicit resolves to enclosing method liftType")
  implicit def liftType[T <: sangria.ast.Type]: Liftable[T] = Liftable {
    case NamedType(n, p) => q"_root_.sangria.ast.NamedType($n, $p)"
    case NotNullType(o, p) => q"_root_.sangria.ast.NotNullType($o, $p)"
    case ListType(o, p) => q"_root_.sangria.ast.ListType($o, $p)"
  }

  implicit def liftComment: Liftable[Comment] = Liftable { case Comment(l, p) =>
    q"_root_.sangria.ast.Comment($l, $p)"
  }

  implicit def liftVarDef: Liftable[VariableDefinition] = Liftable {
    case VariableDefinition(n, t, d, dirs, c, p) =>
      q"_root_.sangria.ast.VariableDefinition($n, $t, $d, $dirs, $c, $p)"
  }

  implicit def liftInpValDef: Liftable[InputValueDefinition] = Liftable {
    case InputValueDefinition(n, v, de, di, desc, c, p) =>
      q"_root_.sangria.ast.InputValueDefinition($n, $v, $de, $di, $desc, $c, $p)"
  }

  implicit def liftInpOpTpeDef: Liftable[OperationTypeDefinition] = Liftable {
    case OperationTypeDefinition(o, t, c, p) =>
      q"_root_.sangria.ast.OperationTypeDefinition($o, $t, $c, $p)"
  }

  implicit def liftEnumValDef: Liftable[EnumValueDefinition] = Liftable {
    case EnumValueDefinition(n, d, desc, c, p) =>
      q"_root_.sangria.ast.EnumValueDefinition($n, $d, $desc, $c, $p)"
  }

  implicit def liftFieldDef: Liftable[FieldDefinition] = Liftable {
    case FieldDefinition(n, f, a, d, desc, c, p) =>
      q"_root_.sangria.ast.FieldDefinition($n, $f, $a, $d, $desc, $c, $p)"
  }

  implicit def liftDirLocDef: Liftable[DirectiveLocation] = Liftable {
    case DirectiveLocation(n, c, p) =>
      q"_root_.sangria.ast.DirectiveLocation($n, $c, $p)"
  }

  implicit def liftDefinition[T <: Definition]: Liftable[T] = Liftable {
    case OperationDefinition(o, n, v, d, s, c, tc, p) =>
      q"_root_.sangria.ast.OperationDefinition($o, $n, $v, $d, $s, $c, $tc, $p)"
    case FragmentDefinition(n, t, d, s, v, c, tc, p) =>
      q"_root_.sangria.ast.FragmentDefinition($n, $t, $d, $s, $v, $c, $tc, $p)"

    case DirectiveDefinition(n, a, l, desc, r, c, p) =>
      q"_root_.sangria.ast.DirectiveDefinition($n, $a, $l, $desc, $r, $c, $p)"
    case SchemaDefinition(o, d, desc, c, tc, p) =>
      q"_root_.sangria.ast.SchemaDefinition($o, $d, $desc, $c, $tc, $p)"

    case ObjectTypeExtensionDefinition(n, i, f, d, c, tc, p) =>
      q"_root_.sangria.ast.ObjectTypeExtensionDefinition($n, $i, $f, $d, $c, $tc, $p)"
    case InterfaceTypeExtensionDefinition(n, f, i, d, c, tc, p) =>
      q"_root_.sangria.ast.InterfaceTypeExtensionDefinition($n, $f, $i, $d, $c, $tc, $p)"
    case InputObjectTypeExtensionDefinition(n, f, d, c, tc, p) =>
      q"_root_.sangria.ast.InputObjectTypeExtensionDefinition($n, $f, $d, $c, $tc, $p)"
    case UnionTypeExtensionDefinition(n, t, d, c, p) =>
      q"_root_.sangria.ast.UnionTypeExtensionDefinition($n, $t, $d, $c, $p)"
    case EnumTypeExtensionDefinition(n, v, d, c, tc, p) =>
      q"_root_.sangria.ast.EnumTypeExtensionDefinition($n, $v, $d, $c, $tc, $p)"
    case ScalarTypeExtensionDefinition(n, d, c, p) =>
      q"_root_.sangria.ast.ScalarTypeExtensionDefinition($n, $d, $c, $p)"
    case SchemaExtensionDefinition(o, d, c, tc, p) =>
      q"_root_.sangria.ast.SchemaExtensionDefinition($o, $d, $c, $tc, $p)"

    case EnumTypeDefinition(n, v, d, desc, c, tc, p) =>
      q"_root_.sangria.ast.EnumTypeDefinition($n, $v, $d, $desc, $c, $tc, $p)"
    case InputObjectTypeDefinition(n, f, d, desc, c, tc, p) =>
      q"_root_.sangria.ast.InputObjectTypeDefinition($n, $f, $d, $desc, $c, $tc, $p)"
    case InterfaceTypeDefinition(n, f, i, d, desc, c, tc, p) =>
      q"_root_.sangria.ast.InterfaceTypeDefinition($n, $f, $i, $d, $desc, $c, $tc, $p)"
    case ObjectTypeDefinition(n, i, f, d, desc, c, tc, p) =>
      q"_root_.sangria.ast.ObjectTypeDefinition($n, $i, $f, $d, $desc, $c, $tc, $p)"
    case ScalarTypeDefinition(n, d, desc, c, p) =>
      q"_root_.sangria.ast.ScalarTypeDefinition($n, $d, $desc, $c, $p)"
    case UnionTypeDefinition(n, t, d, desc, c, p) =>
      q"_root_.sangria.ast.UnionTypeDefinition($n, $t, $d, $desc, $c, $p)"
  }

  implicit def liftNamedValue[T <: NameValue]: Liftable[T] = Liftable {
    case Argument(n, v, c, p) => q"_root_.sangria.ast.Argument($n, $v, $c, $p)"
    case ObjectField(n, v, c, p) => q"_root_.sangria.ast.ObjectField($n, $v, $c, $p)"
  }

  implicit def liftValue[T <: sangria.ast.Value]: Liftable[T] = Liftable {
    case IntValue(v, c, p) => q"_root_.sangria.ast.IntValue($v, $c, $p)"
    case FloatValue(v, c, p) => q"_root_.sangria.ast.FloatValue($v, $c, $p)"
    case StringValue(v, b, r, c, p) => q"_root_.sangria.ast.StringValue($v, $b, $r, $c, $p)"
    case BooleanValue(v, c, p) => q"_root_.sangria.ast.BooleanValue($v, $c, $p)"
    case NullValue(c, p) => q"_root_.sangria.ast.NullValue($c, $p)"
    case EnumValue(v, c, p) => q"_root_.sangria.ast.EnumValue($v, $c, $p)"
    case ListValue(v, c, p) => q"_root_.sangria.ast.ListValue($v, $c, $p)"
    case ObjectValue(f, c, p) => q"_root_.sangria.ast.ObjectValue($f, $c, $p)"
    case VariableValue(n, c, p) => q"_root_.sangria.ast.VariableValue($n, $c, $p)"
    case BigIntValue(v, c, p) =>
      q"_root_.sangria.ast.BigIntValue(_root_.scala.math.BigInt(${v.toByteArray}), $c, $p)"
    case sangria.ast.BigDecimalValue(v, c, p) =>
      q"_root_.sangria.ast.BigDecimalValue(_root_.scala.math.BigDecimal(${v.toString()}), $c, $p)"
  }

  implicit def directive: Liftable[sangria.ast.Directive] = Liftable { case Directive(n, a, c, p) =>
    q"_root_.sangria.ast.Directive($n, $a, $c, $p)"
  }

  implicit def selection[T <: Selection]: Liftable[T] = Liftable {
    case Field(a, n, arg, d, s, c, tc, p) =>
      q"_root_.sangria.ast.Field($a, $n, $arg, $d, $s, $c, $tc, $p)"
    case FragmentSpread(n, d, c, p) =>
      q"_root_.sangria.ast.FragmentSpread($n, $d, $c, $p)"
    case InlineFragment(t, d, s, c, tc, p) =>
      q"_root_.sangria.ast.InlineFragment($t, $d, $s, $c, $tc, $p)"
  }

  implicit def liftDocument: Liftable[Document] = Liftable { case doc @ Document(d, c, p, _) =>
    q"""_root_.sangria.ast.Document($d, $c, $p, _root_.scala.Some(
          new _root_.sangria.ast.DefaultSourceMapper(
            ${doc.sourceMapper.get.id},
            _root_.sangria.parser.ParserConfig.parboiledToSourceMapper(_root_.org.parboiled2.ParserInput(${doc.source.get}))
          )
        ))"""
  }

  implicit def liftInputDocument: Liftable[InputDocument] = Liftable {
    case doc @ InputDocument(d, c, p, _) =>
      q"""_root_.sangria.ast.InputDocument($d, $c, $p, _root_.scala.Some(
            new _root_.sangria.ast.DefaultSourceMapper(
              ${doc.sourceMapper.get.id},
              _root_.sangria.parser.ParserConfig.parboiledToSourceMapper(_root_.org.parboiled2.ParserInput(${doc.source.get}))
            )
          ))"""
  }
}

trait MacroAstLiftable extends AstLiftable {
  val c: blackbox.Context
  val universe: c.universe.type = c.universe
}
