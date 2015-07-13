package sangria.execution

trait ResultMarshaller {
  type Node
  
  def mapNode(keyValues: Map[String, Node]): Node
  def arrayNode(values: Seq[Node]): Node
  
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
  override def mapNode(keyValues: Map[String, Node]) = keyValues
  override def floatNode(value: Double) = value
  override def stringNode(value: String) = value
  override def intNode(value: Int) = value
}