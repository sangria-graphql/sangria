package sangria.marshalling

class CoercedScalaResultMarshaller extends RawResultMarshaller {
  type Node = Any

  override def rawScalarNode(rawValue: Any) = rawValue

  def arrayNode(values: Vector[Node]) = values
  def optionalArrayNodeValue(value: Option[Node]) = value

  def mapNode(keyValues: Seq[(String, Node)]) = Map(keyValues: _*)
  def emptyMapNode = Map.empty[String, Any]
  def addMapNodeElem(node: Node, key: String, value: Node, optional: Boolean) = {
    val res =
      if (optional && value.isInstanceOf[None.type])
        None
      else if (optional)
        Some(value)
      else
        value

    node.asInstanceOf[Map[String, Any]] + (key → res)
  }

  def nullNode = None

  def renderCompact(node: Any) = "" + node
  def renderPretty(node: Any) = "" + node
}

object CoercedScalaResultMarshaller {
  val default = new CoercedScalaResultMarshaller
}
