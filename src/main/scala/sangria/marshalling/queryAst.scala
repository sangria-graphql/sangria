package sangria.marshalling

import sangria.ast
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer

object queryAst {
  implicit val queryAstInputUnmarshaller = new QueryAstInputUnmarshaller

  implicit val queryAstResultMarshaller = new QueryAstResultMarshaller

  implicit object QueryAstMarshallerForType extends ResultMarshallerForType[ast.Value] {
    val marshaller = queryAstResultMarshaller
  }

  private object QueryAstToInput extends ToInput[ast.Value, ast.Value] {
    def toInput(value: ast.Value) = (value, queryAstInputUnmarshaller)
  }

  implicit def queryAstToInput[T <: ast.Value]: ToInput[T, ast.Value] =
    QueryAstToInput.asInstanceOf[ToInput[T, ast.Value]]

  private object QueryAstFromInput extends FromInput[ast.Value] {
    val marshaller = queryAstResultMarshaller
    def fromResult(node: marshaller.Node) = node
  }

  implicit def queryAstFromInput[T <: ast.Value]: FromInput[T] =
    QueryAstFromInput.asInstanceOf[FromInput[T]]

  implicit object QueryAstInputParser extends InputParser[ast.Value] {
    def parse(str: String) = QueryParser.parseInput(str)
  }
}

class QueryAstInputUnmarshaller extends InputUnmarshaller[ast.Value] {
  def getRootMapValue(node: ast.Value, key: String) = node.asInstanceOf[ast.ObjectValue].fieldsByName get key

  def isMapNode(node: ast.Value) = node.isInstanceOf[ast.ObjectValue]
  def getMapValue(node: ast.Value, key: String) = node.asInstanceOf[ast.ObjectValue].fieldsByName get key
  def getMapKeys(node: ast.Value) = node.asInstanceOf[ast.ObjectValue].fieldsByName.keys

  def isListNode(node: ast.Value) = node.isInstanceOf[ast.ListValue]
  def getListValue(node: ast.Value) = node.asInstanceOf[ast.ListValue].values

  def isDefined(node: ast.Value) = !node.isInstanceOf[ast.NullValue]

  def isEnumNode(node: ast.Value) = node.isInstanceOf[ast.EnumValue]

  def isScalarNode(node: ast.Value) = node.isInstanceOf[ast.ScalarValue]
  def getScalarValue(node: ast.Value) = node
  def getScalaScalarValue(node: ast.Value) = node match {
    case ast.BooleanValue(b, _) ⇒ b
    case ast.BigIntValue(i, _) ⇒ i
    case ast.BigDecimalValue(d, _) ⇒ d
    case ast.FloatValue(f, _) ⇒ f
    case ast.IntValue(i, _) ⇒ i
    case ast.StringValue(s, _) ⇒ s
    case ast.EnumValue(s, _) ⇒ s
    case node ⇒ throw new IllegalStateException("Unsupported scalar node: " + node)
  }

  def isVariableNode(node: ast.Value) = node.isInstanceOf[ast.VariableValue]
  def getVariableName(node: ast.Value) = node.asInstanceOf[ast.VariableValue].name

  def render(node: ast.Value) = QueryRenderer.render(node, QueryRenderer.Compact)
}

class QueryAstResultMarshaller extends ResultMarshaller {
  type Node = ast.Value
  type MapBuilder = ArrayMapBuilder[Node]

  def emptyMapNode(keys: Seq[String]) = new ArrayMapBuilder[Node](keys)
  def addMapNodeElem(builder: MapBuilder, key: String, value: Node, optional: Boolean) = builder.add(key, value)

  def booleanNode(value: Boolean) = ast.BooleanValue(value)
  def floatNode(value: Double) = ast.FloatValue(value)
  def stringNode(value: String) = ast.StringValue(value)
  def intNode(value: Int) = ast.IntValue(value)
  def bigIntNode(value: BigInt) = ast.BigIntValue(value)
  def bigDecimalNode(value: BigDecimal) = ast.BigDecimalValue(value)

  def arrayNode(values: Vector[Node]) = ast.ListValue(values.toList)
  def optionalArrayNodeValue(value: Option[Node]) = value match {
    case Some(v) ⇒ v
    case None ⇒ nullNode
  }

  def mapNode(builder: MapBuilder) = mapNode(builder.toList)
  def mapNode(keyValues: Seq[(String, Node)]) =
    ast.ObjectValue(keyValues.toList.map{case (k, v) ⇒ ast.ObjectField(k, v)})

  def nullNode = ast.NullValue()

  def renderCompact(node: Node) = QueryRenderer.render(node, QueryRenderer.Compact)
  def renderPretty(node: Node) = QueryRenderer.render(node, QueryRenderer.Pretty)
}