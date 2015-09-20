package sangria.integration

import io.circe._

object circe {
  implicit object CirceResultMarshaller extends ResultMarshaller {
    type Node = Json

    def emptyMapNode = Json.obj()
    def mapNode(keyValues: Seq[(String, Json)]) = Json.obj(keyValues: _*)
    def addMapNodeElem(node: Json, key: String, value: Json) = node.mapObject(_ + (key, value))

    lazy val emptyArrayNode = Json.array()
    def isEmptyArrayNode(array: Json) = array.asArray.get.isEmpty
    def arrayNode(values: Seq[Json]) = Json.array(values.toVector: _*)
    def addArrayNodeElem(array: Json, elem: Json) = array.mapArray(_ :+ elem)

    def booleanNode(value: Boolean) = Json.bool(value)
    def floatNode(value: Double) = Json.number(value).get
    def stringNode(value: String) = Json.string(value)
    def intNode(value: Int) = Json.int(value)
    def bigIntNode(value: BigInt) = Json.bigDecimal(BigDecimal(value))
    def bigDecimalNode(value: BigDecimal) = Json.bigDecimal(value)

    def nullNode = Json.empty

    def renderCompact(node: Json) = node.noSpaces
    def renderPretty(node: Json) = node.spaces2
  }

  implicit object CirceInputUnmarshaller extends InputUnmarshaller[Json] {
    def getRootMapValue(node: Json, key: String) = node.asObject.get(key)

    def isMapNode(node: Json) = node.isObject
    def getMapValue(node: Json, key: String) = node.asObject.get(key)
    def getMapKeys(node: Json) = node.asObject.get.fields

    def isArrayNode(node: Json) = node.isArray
    def getListValue(node: Json) = node.asArray.get

    def isDefined(node: Json) = !node.isNull
    def getScalarValue(node: Json) =
      if (node.isBoolean)
        node.asBoolean.get
      else if (node.isNumber) {
        val num = node.asNumber.get.toBigDecimal

        num.toBigIntExact getOrElse num
      } else if (node.isString)
        node.asString.get
      else
        throw new IllegalStateException(s"$node is not a scalar value")

    def isScalarNode(node: Json) =
      node.isBoolean || node.isNumber || node.isString

    def render(node: Json) = node.noSpaces
  }

  implicit object circeJsonToInput extends ToInput[Json, Json] {
    def toInput(value: Json) = (value, CirceInputUnmarshaller)
  }
}