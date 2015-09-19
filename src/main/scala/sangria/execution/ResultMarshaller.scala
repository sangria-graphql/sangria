package sangria.execution

trait ResultMarshaller {
  type Node

  def emptyMapNode: Node
  def addMapNodeElem(node: Node, key: String, value: Node): Node
  def mapNode(keyValues: Seq[(String, Node)]): Node

  def emptyArrayNode: Node
  def isEmptyArrayNode(array: Node): Boolean
  def arrayNode(values: Seq[Node]): Node
  def addArrayNodeElem(array: Node, elem: Node): Node

  def stringNode(value: String): Node
  def intNode(value: Int): Node
  def bigIntNode(value: BigInt): Node
  def floatNode(value: Double): Node
  def bigDecimalNode(value: BigDecimal): Node
  def booleanNode(value: Boolean): Node

  def nullNode: Node

  def renderCompact(node: Node): String
  def renderPretty(node: Node): String
}

object ResultMarshaller {
  implicit val defaultResultMarshaller = new ScalaResultMarshaller
}

class ScalaResultMarshaller extends ResultMarshaller {
  type Node = Any

  def booleanNode(value: Boolean) = value
  def floatNode(value: Double) = value
  def stringNode(value: String) = value
  def intNode(value: Int) = value
  def bigIntNode(value: BigInt) = value
  def bigDecimalNode(value: BigDecimal) = value

  def arrayNode(values: Seq[Node]) = values
  def isEmptyArrayNode(array: Node) = array.asInstanceOf[List[_]].isEmpty
  def emptyArrayNode = Nil
  def addArrayNodeElem(array: Node, elem: Node) = array.asInstanceOf[List[_]] :+ elem

  def mapNode(keyValues: Seq[(String, Node)]) = Map(keyValues: _*)
  def emptyMapNode = Map.empty[String, Any]
  def addMapNodeElem(node: Node, key: String, value: Node) =
    node.asInstanceOf[Map[String, Any]] + (key -> value)

  def nullNode = null

  def renderCompact(node: Any) = "" + node
  def renderPretty(node: Any) = "" + node
}
