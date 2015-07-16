package sangria.execution

trait ResultMarshaller {
  type Node

  def mapNode(keyValues: Seq[(String, Node)]): Node
  def arrayNode(values: Seq[Node]): Node
  def emptyArrayNode: Node
  def addArrayNodeElem(array: Node, elem: Node): Node

  def stringNode(value: String): Node
  def intNode(value: Int): Node
  def floatNode(value: Double): Node
  def booleanNode(value: Boolean): Node
}

object ResultMarshaller {
  implicit val defaultResultMarshaller = new ScalaResultMarshaller
}

class ScalaResultMarshaller extends ResultMarshaller {
  type Node = Any

  override def booleanNode(value: Boolean) = value
  override def arrayNode(values: Seq[Node]) = values
  override def emptyArrayNode = Nil
  override def addArrayNodeElem(array: Node, elem: Node) = array.asInstanceOf[List[_]] :+ elem
  override def mapNode(keyValues: Seq[(String, Node)]) = Map(keyValues: _*)
  override def floatNode(value: Double) = value
  override def stringNode(value: String) = value
  override def intNode(value: Int) = value
}
