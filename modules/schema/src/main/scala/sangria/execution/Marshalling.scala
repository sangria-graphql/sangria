package sangria.execution

import sangria.ast
import sangria.marshalling.{ResultMarshaller, ScalarValueInfo}

object Marshalling {
  val DefaultComplexity = 1.0d

  def marshalEnumValue(
      value: String,
      marshaller: ResultMarshaller,
      typeName: String): marshaller.Node =
    marshaller.enumNode(value, typeName)

  def marshalScalarValue(
      value: Any,
      marshaller: ResultMarshaller,
      typeName: String,
      scalarInfo: Set[ScalarValueInfo]): marshaller.Node =
    value match {
      case astValue: ast.Value => marshalAstValue(astValue, marshaller, typeName, scalarInfo)
      case null => marshaller.nullNode
      case v => marshaller.scalarNode(value, typeName, scalarInfo)
    }

  def marshalAstValue(
      value: ast.Value,
      marshaller: ResultMarshaller,
      typeName: String,
      scalarInfo: Set[ScalarValueInfo]): marshaller.Node = value match {
    case ast.StringValue(str, _, _, _, _) => marshaller.scalarNode(str, typeName, scalarInfo)
    case ast.IntValue(i, _, _) => marshaller.scalarNode(i, typeName, scalarInfo)
    case ast.BigIntValue(i, _, _) => marshaller.scalarNode(i, typeName, scalarInfo)
    case ast.FloatValue(f, _, _) => marshaller.scalarNode(f, typeName, scalarInfo)
    case ast.BigDecimalValue(f, _, _) => marshaller.scalarNode(f, typeName, scalarInfo)
    case ast.BooleanValue(b, _, _) => marshaller.scalarNode(b, typeName, scalarInfo)
    case ast.NullValue(_, _) => marshaller.nullNode
    case ast.EnumValue(enum, _, _) => marshaller.enumNode(enum, typeName)
    case ast.ListValue(values, _, _) =>
      marshaller.arrayNode(values.map(marshalAstValue(_, marshaller, typeName, scalarInfo)))
    case ast.ObjectValue(values, _, _) =>
      marshaller.mapNode(
        values.map(v => v.name -> marshalAstValue(v.value, marshaller, typeName, scalarInfo)))
    case ast.VariableValue(name, _, _) => marshaller.enumNode(name, typeName)
  }
}
