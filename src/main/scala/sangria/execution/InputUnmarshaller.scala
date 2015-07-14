package sangria.execution

trait InputUnmarshaller[Node] {
  type LeafNode

  def emptyNode: Node

  def getRootMapValue(node: Node, key: String): Option[LeafNode]

  def isMapNode(node: LeafNode): Boolean
  def getMapValue(node: LeafNode, key: String): Option[LeafNode]

  def isArrayNode(node: LeafNode): Boolean
  def getArrayValue(node: LeafNode): Seq[LeafNode]

  // Scalar values are Scala String, Int, Double, Boolean and Enum values defined in the schema

  def isDefined(node: LeafNode): Boolean
  def isScalarNode(node: LeafNode): Boolean
  def getScalarValue(node: LeafNode): Any

  def render(node: LeafNode): String
}

object InputUnmarshaller {
  implicit def scalaInputUnmarshaller[T] = ScalaInputUnmarshaller.asInstanceOf[InputUnmarshaller[Map[String, T]]]
}

object ScalaInputUnmarshaller extends InputUnmarshaller[Map[String, Any]] {
  type LeafNode = Any

  def emptyNode = Map.empty

  def getRootMapValue(node: Map[String, Any], key: String) = node get key

  def isMapNode(node: Any) = node.isInstanceOf[Map[_, _]]
  def getMapValue(node: Any, key: String) = node.asInstanceOf[Map[String, _]] get key

  def isArrayNode(node: Any) = node.isInstanceOf[Seq[_]]
  def getArrayValue(node: Any) = node.asInstanceOf[Seq[_]]

  def isDefined(node: Any) = true
  def isScalarNode(node: Any) = !(isMapNode(node) && isArrayNode(node))
  def getScalarValue(node: Any) = node

  def render(node: LeafNode) = node.toString
}
