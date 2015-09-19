package sangria.integration

import sangria.execution.{InputUnmarshaller, ResultMarshaller}
import io.circe._

object CirceSupport extends CirceSupportLowPrioImplicits {

  implicit object CirceResultMarshaller extends ResultMarshaller {
    override type Node = Json

    override def addArrayNodeElem(array: Json, elem: Json) = array.mapArray(_ :+ elem)

    override def booleanNode(value: Boolean) = Json.bool(value)

    override def emptyMapNode = Json.obj()

    override def arrayNode(values: Seq[Json]) = Json.array(values.toVector: _*)

    override def mapNode(keyValues: Seq[(String, Json)]) = Json.obj(keyValues: _*)

    override def addMapNodeElem(node: Json, key: String, value: Json) = node.mapObject(_ + (key, value))

    override def floatNode(value: Double) = Json.number(value).get

    override def isEmptyArrayNode(array: Json) = array.asArray.get.isEmpty

    override lazy val emptyArrayNode = Json.array()

    override def stringNode(value: String) = Json.string(value)

    override def intNode(value: Int) = Json.int(value)

    override def nullNode = Json.empty

    override def renderCompact(node: Json) = node.noSpaces

    override def renderPretty(node: Json) = node.spaces2

    override def bigIntNode(value: BigInt) = Json.bigDecimal(BigDecimal(value))

    override def bigDecimalNode(value: BigDecimal) = Json.bigDecimal(value)
  }

  implicit object CirceInputUnmarshaller extends InputUnmarshaller[Json] {
    override type LeafNode = Json

    override def isDefined(node: Json) = !node.isNull

    override def getScalarValue(node: Json) =
      if (node.isBoolean)
        node.asBoolean.get
      else if (node.isNumber) {
        val num = node.asNumber.get.toBigDecimal

        num.toBigIntExact getOrElse num
      } else if (node.isString)
        node.asString.get
      else
        throw new IllegalStateException(s"$node is not a scalar value")

    override def isScalarNode(node: Json) =
      node.isBoolean || node.isNumber || node.isString

    override def isMapNode(node: Json) = node.isObject

    override def getListValue(node: Json) = node.asArray.get

    override def render(node: Json) = node.noSpaces

    override def isArrayNode(node: Json) = node.isArray

    override def getMapValue(node: Json, key: String) = node.asObject.get(key)

    override def emptyNode = Json.empty

    override def getRootMapValue(node: Json, key: String) = node.asObject.get(key)

    override def getMapKeys(node: Json) = node.asObject.get.fields
  }
}

trait CirceSupportLowPrioImplicits {
  implicit val CirceInputUnmarshallerJObject =
    CirceSupport.CirceInputUnmarshaller.asInstanceOf[InputUnmarshaller[JsonObject]]
}
