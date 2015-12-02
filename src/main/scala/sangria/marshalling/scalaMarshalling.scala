package sangria.marshalling

import sangria.util.tag
import sangria.util.tag._

object scalaMarshalling {
  private val scalaScalaUnmarshallerGen = new ScalaInputUnmarshaller[Any]

  implicit val scalaResultMarshaller = new ScalaResultMarshaller

  implicit def scalaInputUnmarshaller[T] = scalaScalaUnmarshallerGen.asInstanceOf[InputUnmarshaller[T @@ ScalaInput]]
}

class ScalaResultMarshaller extends ResultMarshaller {
  type Node = Any

  def booleanNode(value: Boolean) = value
  def floatNode(value: Double) = value
  def stringNode(value: String) = value
  def intNode(value: Int) = value
  def bigIntNode(value: BigInt) = value
  def bigDecimalNode(value: BigDecimal) = value

  def arrayNode(values: Vector[Node]) = values
  def optionalArrayNodeValue(value: Option[Node]) = value match {
    case Some(v) ⇒ v
    case None ⇒ nullNode
  }

  def mapNode(keyValues: Seq[(String, Node)]) = Map(keyValues: _*)
  def emptyMapNode = Map.empty[String, Any]
  def addMapNodeElem(node: Node, key: String, value: Node, optional: Boolean) =
    node.asInstanceOf[Map[String, Any]] + (key → value)

  def nullNode = null

  def renderCompact(node: Any) = "" + node
  def renderPretty(node: Any) = "" + node
}

class ScalaInputUnmarshaller[T] extends InputUnmarshaller[T @@ ScalaInput] {
  def getRootMapValue(node: T @@ ScalaInput, key: String) = node.asInstanceOf[Map[String, Any]] get key map (v ⇒ tag[ScalaInput](v.asInstanceOf[T]))

  def isMapNode(node: T @@ ScalaInput) = node.isInstanceOf[Map[_, _]]
  def getMapValue(node: T @@ ScalaInput, key: String) = node.asInstanceOf[Map[String, _]] get key map (v ⇒ tag[ScalaInput](v.asInstanceOf[T]))
  def getMapKeys(node: T @@ ScalaInput) = node.asInstanceOf[Map[String, _]].keySet

  def isArrayNode(node: T @@ ScalaInput) = node.isInstanceOf[Seq[_]]
  def getListValue(node: T @@ ScalaInput) = node.asInstanceOf[Seq[_]] map (v ⇒ tag[ScalaInput](v.asInstanceOf[T]))

  def isDefined(node: T @@ ScalaInput) = node != null

  def isEnumNode(node: T @@ ScalaInput) = isScalarNode(node)

  def isScalarNode(node: T @@ ScalaInput) = !(isMapNode(node) || isArrayNode(node))
  def getScalarValue(node: T @@ ScalaInput) = node

  def isVariableNode(node: T @@ ScalaInput) = false
  def getVariableName(node: T @@ ScalaInput) = throw new IllegalArgumentException("variables are not supported")

  def render(node: T @@ ScalaInput) = if (node == null) "null" else node.toString
}

sealed trait ScalaInput

object ScalaInput {
  def scalaInput[T](value: T) = tag[ScalaInput](value)
}
