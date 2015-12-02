package sangria.marshalling

import scala.collection.immutable.VectorBuilder

trait ResultMarshaller {
  type Node

  def emptyMapNode: Node
  def addMapNodeElem(node: Node, key: String, value: Node, optional: Boolean): Node
  def mapNode(keyValues: Seq[(String, Node)]): Node

  def arrayNode(values: Vector[Node]): Node
  def optionalArrayNodeValue(value: Option[Node]): Node

  def stringNode(value: String): Node
  def intNode(value: Int): Node
  def bigIntNode(value: BigInt): Node
  def floatNode(value: Double): Node
  def bigDecimalNode(value: BigDecimal): Node
  def booleanNode(value: Boolean): Node

  def nullNode: Node

  def renderCompact(node: Node): String
  def renderPretty(node: Node): String

  def mapAndMarshal[T](seq: Seq[T], fn: T ⇒ Node): Node = {
    val res = new VectorBuilder[Node]

    for (elem ← seq) {
      res += fn(elem)
    }

    arrayNode(res.result())
  }
}

object ResultMarshaller {
  implicit val defaultResultMarshaller = scalaMarshalling.scalaResultMarshaller
}

/**
  * Alters the behaviour of the executor and marshals raw (in-scala coerced representation) or scalar values and enums.
  */
trait RawResultMarshaller extends ResultMarshaller {
  def rawScalarNode(rawValue: Any): Node

  private def onlyRawValuesExpected =
    throw new IllegalArgumentException("Only raw values expected in `RawResultMarshaller`!")

  final def stringNode(value: String) = onlyRawValuesExpected
  final def intNode(value: Int) = onlyRawValuesExpected
  final def bigIntNode(value: BigInt) = onlyRawValuesExpected
  final def floatNode(value: Double) = onlyRawValuesExpected
  final def bigDecimalNode(value: BigDecimal) = onlyRawValuesExpected
  final def booleanNode(value: Boolean) = onlyRawValuesExpected
}