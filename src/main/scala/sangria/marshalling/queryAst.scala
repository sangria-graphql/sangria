package sangria.marshalling

import sangria.ast
import sangria.renderer.QueryRenderer

object queryAst {
  implicit val queryAstInputUnmarshaller = new QueryAstInputUnmarshaller

  implicit val queryAstResultMarshaller = new QueryAstResultMarshaller

  private object PlayJsonToInput extends ToInput[ast.Value, ast.Value] {
    def toInput(value: ast.Value) = (value, queryAstInputUnmarshaller)
  }

  implicit def playJsonToInput[T <: ast.Value]: ToInput[T, ast.Value] =
    PlayJsonToInput.asInstanceOf[ToInput[T, ast.Value]]
}

class QueryAstInputUnmarshaller extends InputUnmarshaller[ast.Value] {
  def getRootMapValue(node: ast.Value, key: String) = node.asInstanceOf[ast.ObjectValue].fieldsByName get key

  def isMapNode(node: ast.Value) = node.isInstanceOf[ast.ObjectValue]
  def getMapValue(node: ast.Value, key: String) = node.asInstanceOf[ast.ObjectValue].fieldsByName get key
  def getMapKeys(node: ast.Value) = node.asInstanceOf[ast.ObjectValue].fieldsByName.keySet

  def isArrayNode(node: ast.Value) = node.isInstanceOf[ast.ListValue]
  def getListValue(node: ast.Value) = node.asInstanceOf[ast.ListValue].values

  def isDefined(node: ast.Value) = !node.isInstanceOf[ast.NullValue]

  def isEnumNode(node: ast.Value) = node.isInstanceOf[ast.EnumValue]

  def isScalarNode(node: ast.Value) = node.isInstanceOf[ast.ScalarValue]
  def getScalarValue(node: ast.Value) = node

  def isVariableNode(node: ast.Value) = node.isInstanceOf[ast.VariableValue]
  def getVariableName(node: ast.Value) = node.asInstanceOf[ast.VariableValue].name

  def render(node: ast.Value) = QueryRenderer.render(node, QueryRenderer.Compact)
}

class QueryAstResultMarshaller extends ResultMarshaller {
  type Node = ast.Value

  def booleanNode(value: Boolean) = ast.BooleanValue(value)
  def floatNode(value: Double) = ast.FloatValue(value)
  def stringNode(value: String) = ast.StringValue(value)
  def intNode(value: Int) = ast.IntValue(value)
  def bigIntNode(value: BigInt) = ast.BigIntValue(value)
  def bigDecimalNode(value: BigDecimal) = ast.BigDecimalValue(value)

  def arrayNode(values: Vector[Node]) = ast.ListValue(values.toList)

  def mapNode(keyValues: Seq[(String, Node)]) =
    ast.ObjectValue(keyValues.toList.map{case (k, v) ⇒ ast.ObjectField(k, v)})

  def emptyMapNode = ast.ObjectValue(Nil)
  def addMapNodeElem(node: Node, key: String, value: Node) = {
    val obj = node.asInstanceOf[ast.ObjectValue]

    obj.copy(fields = obj.fields :+ ast.ObjectField(key, value))
  }

  def nullNode = ast.NullValue()

  def renderCompact(node: Node) = QueryRenderer.render(node, QueryRenderer.Compact)
  def renderPretty(node: Node) = QueryRenderer.render(node, QueryRenderer.Pretty)
}