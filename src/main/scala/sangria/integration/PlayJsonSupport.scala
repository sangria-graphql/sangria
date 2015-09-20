package sangria.integration

import play.api.libs.json._

object PlayJsonSupport extends PlayJsonSupportLowPrioImplicits {
  implicit object PlayJsonResultMarshaller extends ResultMarshaller {
    type Node = JsValue

    def emptyMapNode = JsObject(Seq.empty)
    def mapNode(keyValues: Seq[(String, JsValue)]) = JsObject(keyValues)
    def addMapNodeElem(node: JsValue, key: String, value: JsValue) = node.asInstanceOf[JsObject] + (key -> value)

    def emptyArrayNode = JsArray(Seq.empty)
    def isEmptyArrayNode(array: JsValue) = array.asInstanceOf[JsArray].value.isEmpty
    def arrayNode(values: Seq[JsValue]) = JsArray(values)
    def addArrayNodeElem(array: JsValue, elem: JsValue) = array.asInstanceOf[JsArray].append(elem)

    def stringNode(value: String) = JsString(value)
    def floatNode(value: Double) = JsNumber(value)
    def booleanNode(value: Boolean) = JsBoolean(value)
    def intNode(value: Int) = JsNumber(value)
    def bigIntNode(value: BigInt) = JsNumber(BigDecimal(value))
    def bigDecimalNode(value: BigDecimal) = JsNumber(value)

    def nullNode = JsNull

    def renderCompact(node: JsValue) = Json.stringify(node)
    def renderPretty(node: JsValue) = Json.prettyPrint(node)
  }

  implicit object PlayJsonInputUnmarshaller extends InputUnmarshaller[JsValue] {
    def getRootMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].value get key

    def isArrayNode(node: JsValue) = node.isInstanceOf[JsArray]
    def getListValue(node: JsValue) = node.asInstanceOf[JsArray].value

    def isMapNode(node: JsValue) = node.isInstanceOf[JsObject]
    def getMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].value get key
    def getMapKeys(node: JsValue) = node.asInstanceOf[JsObject].keys

    def isDefined(node: JsValue) = node != JsNull
    def getScalarValue(node: JsValue) = node match {
      case JsBoolean(b) => b
      case JsNumber(d) => d.toBigIntExact getOrElse d
      case JsString(s) => s
      case _ => throw new IllegalStateException(s"$node is not a scalar value")
    }
    def isScalarNode(node: JsValue) = node match {
      case _: JsBoolean | _: JsNumber | _: JsString => true
      case _ => false
    }

    def render(node: JsValue) = Json.stringify(node)
  }
}

trait PlayJsonSupportLowPrioImplicits {
  implicit val PlayJsonInputUnmarshallerJObject =
    PlayJsonSupport.PlayJsonInputUnmarshaller.asInstanceOf[InputUnmarshaller[JsObject]]
}
