package sangria.marshalling

import scala.collection.immutable.ListMap

class CoercedScalaResultMarshaller extends RawResultMarshaller {
  type Node = Any
  type MapBuilder = ArrayMapBuilder[Node]

  override def rawScalarNode(rawValue: Any) = rawValue

  def arrayNode(values: Vector[Node]) = values
  def optionalArrayNodeValue(value: Option[Node]) = value

  def addMapNodeElem(builder: MapBuilder, key: String, value: Node, optional: Boolean) = {
    val res =
      if (optional && value.isInstanceOf[None.type])
        None
      else if (optional)
        Some(value)
      else
        value

    builder.add(key, res)
  }

  def emptyMapNode(keys: Seq[String]) = new ArrayMapBuilder[Node](keys)

  def mapNode(keyValues: Seq[(String, Node)]) = ListMap(keyValues: _*)
  def mapNode(builder: MapBuilder) = builder.toListMap

  def nullNode = None

  def renderCompact(node: Any) = "" + node
  def renderPretty(node: Any) = "" + node
}

object CoercedScalaResultMarshaller {
  val default = new CoercedScalaResultMarshaller
}
