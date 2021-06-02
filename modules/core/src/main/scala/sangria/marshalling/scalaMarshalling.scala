package sangria.marshalling

import sangria.util.tag
import sangria.util.tag._

import scala.collection.immutable.ListMap

object scalaMarshalling {
  private val scalaScalaUnmarshallerGen = new ScalaInputUnmarshaller[Any]
  implicit val scalaResultMarshaller: ScalaResultMarshaller = new ScalaResultMarshaller

  implicit object ScalaMarshallerForType extends ResultMarshallerForType[Any @@ ScalaInput] {
    val marshaller = scalaResultMarshaller
  }

  implicit def scalaInputUnmarshaller[T]: InputUnmarshaller[T @@ ScalaInput] =
    scalaScalaUnmarshallerGen.asInstanceOf[InputUnmarshaller[T @@ ScalaInput]]
}

class ScalaResultMarshaller extends ResultMarshaller {
  type Node = Any
  type MapBuilder = ArrayMapBuilder[Node]

  def scalarNode(value: Any, typeName: String, info: Set[ScalarValueInfo]) = value
  def enumNode(value: String, typeName: String) = value

  def arrayNode(values: Vector[Node]) = values
  def optionalArrayNodeValue(value: Option[Node]) = value match {
    case Some(v) => v
    case None => nullNode
  }

  def emptyMapNode(keys: Seq[String]) = new ArrayMapBuilder[Node](keys)
  def addMapNodeElem(builder: MapBuilder, key: String, value: Node, optional: Boolean) =
    builder.add(key, value)

  def mapNode(builder: MapBuilder) = builder.toListMap
  def mapNode(keyValues: Seq[(String, Node)]) = ListMap(keyValues: _*)

  def nullNode = null

  def renderCompact(node: Any) = "" + node
  def renderPretty(node: Any) = "" + node
}

class ScalaInputUnmarshaller[T] extends InputUnmarshaller[T @@ ScalaInput] {
  def getRootMapValue(node: T @@ ScalaInput, key: String) =
    node.asInstanceOf[Map[String, Any]].get(key).map(v => tag[ScalaInput](v.asInstanceOf[T]))

  def isMapNode(node: T @@ ScalaInput) = node.isInstanceOf[Map[_, _]]
  def getMapValue(node: T @@ ScalaInput, key: String) =
    node.asInstanceOf[Map[String, _]].get(key).map(v => tag[ScalaInput](v.asInstanceOf[T]))
  def getMapKeys(node: T @@ ScalaInput) = node.asInstanceOf[Map[String, _]].keys

  def isListNode(node: T @@ ScalaInput) = node.isInstanceOf[Seq[_]]
  def getListValue(node: T @@ ScalaInput) =
    node.asInstanceOf[Seq[_]].map(v => tag[ScalaInput](v.asInstanceOf[T]))

  def isDefined(node: T @@ ScalaInput) = node != null

  def isEnumNode(node: T @@ ScalaInput) = isScalarNode(node)

  def isScalarNode(node: T @@ ScalaInput) = !(isMapNode(node) || isListNode(node))
  def getScalarValue(node: T @@ ScalaInput) = node
  def getScalaScalarValue(node: T @@ ScalaInput) = getScalarValue(node)

  def isVariableNode(node: T @@ ScalaInput) = false
  def getVariableName(node: T @@ ScalaInput) = throw new IllegalArgumentException(
    "variables are not supported")

  def render(node: T @@ ScalaInput) = if (node == null) "null" else node.toString
}

sealed trait ScalaInput

object ScalaInput {
  def scalaInput[T](value: T) = tag[ScalaInput](value)
}
