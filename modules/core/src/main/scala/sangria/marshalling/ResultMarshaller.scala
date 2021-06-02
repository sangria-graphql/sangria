package sangria.marshalling

import scala.annotation.implicitNotFound
import scala.collection.immutable.VectorBuilder

trait ResultMarshaller {
  type Node
  type MapBuilder

  def emptyMapNode(keys: Seq[String]): MapBuilder
  def addMapNodeElem(builder: MapBuilder, key: String, value: Node, optional: Boolean): MapBuilder

  def mapNode(builder: MapBuilder): Node
  def mapNode(keyValues: Seq[(String, Node)]): Node

  def arrayNode(values: Vector[Node]): Node
  def optionalArrayNodeValue(value: Option[Node]): Node

  /** Marshals a coerced scalar value
    *
    * Following scala types must be supported:
    *
    *   - String
    *   - Boolean
    *   - Int
    *   - Long
    *   - Float
    *   - Double
    *   - scala.BigInt
    *   - scala.BigDecimal
    *
    * Implementation may also support additional scala types if underlying data format supports them (like Dates, or BLOBs).
    *
    * @param value coerced scalar value
    * @return marshaled node
    */
  def scalarNode(value: Any, typeName: String, info: Set[ScalarValueInfo]): Node

  def enumNode(value: String, typeName: String): Node

  def nullNode: Node

  def renderCompact(node: Node): String
  def renderPretty(node: Node): String

  def mapAndMarshal[T](seq: Seq[T], fn: T => Node): Node = {
    val res = new VectorBuilder[Node]

    for (elem <- seq)
      res += fn(elem)

    arrayNode(res.result())
  }

  def capabilities: Set[MarshallerCapability] = Set.empty
}

object ResultMarshaller {
  implicit val defaultResultMarshaller: ScalaResultMarshaller =
    scalaMarshalling.scalaResultMarshaller
}

/** Alters the behaviour of the executor and marshals raw (in-scala coerced representation) or scalar values and enums.
  */
trait RawResultMarshaller extends ResultMarshaller {
  def rawScalarNode(rawValue: Any): Node

  private def onlyRawValuesExpected =
    throw new IllegalArgumentException("Only raw values expected in `RawResultMarshaller`!")

  final def scalarNode(value: Any, typeName: String, info: Set[ScalarValueInfo]) =
    onlyRawValuesExpected
  final def enumNode(value: String, typeName: String) = onlyRawValuesExpected
}

@implicitNotFound(
  "Type ${T} cannot be marshaled. Please consider defining an implicit instance of `ResultMarshallerForType` for it or import appropriate marshaling from `sangria.marshalling`.")
trait ResultMarshallerForType[+T] {
  def marshaller: ResultMarshaller
}
