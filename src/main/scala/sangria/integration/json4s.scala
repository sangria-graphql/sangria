package sangria.integration

import org.json4s.JsonAST._

import org.json4s.native.JsonMethods.{render ⇒ jsonRender, _}

object json4s extends Json4sSupportLowPrioImplicits {
  implicit object Json4sResultMarshaller extends ResultMarshaller {
    type Node = JValue

    def emptyMapNode = JObject(Nil)
    def mapNode(keyValues: Seq[(String, JValue)]) = JObject(keyValues.toList)
    def addMapNodeElem(node: JValue, key: String, value: JValue) = JObject(node.asInstanceOf[JObject].obj :+ (key → value))

    def emptyArrayNode = JArray(Nil)
    def isEmptyArrayNode(array: JValue) = array.asInstanceOf[JArray].arr.isEmpty
    def arrayNode(values: Seq[JValue]) = JArray(values.toList)
    def addArrayNodeElem(array: JValue, elem: JValue) = JArray(array.asInstanceOf[JArray].arr :+ elem)

    def stringNode(value: String) = JString(value)
    def floatNode(value: Double) = JDouble(value)
    def booleanNode(value: Boolean) = JBool(value)
    def intNode(value: Int) = JInt(value)
    def bigIntNode(value: BigInt) = JInt(value)
    def bigDecimalNode(value: BigDecimal) = JDecimal(value)

    def nullNode = JNull

    def renderCompact(node: JValue) = compact(jsonRender(node))
    def renderPretty(node: JValue) = pretty(jsonRender(node))
  }

  implicit object Json4sInputUnmarshaller extends InputUnmarshaller[JValue] {
    def getRootMapValue(node: JValue, key: String) = node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)

    def isMapNode(node: JValue) = node.isInstanceOf[JObject]
    def getMapValue(node: JValue, key: String) = node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)
    def getMapKeys(node: JValue) = node.asInstanceOf[JObject].values.keySet

    def isArrayNode(node: JValue) = node.isInstanceOf[JArray]
    def getListValue(node: JValue) = node.asInstanceOf[JArray].arr

    def isDefined(node: JValue) = node != JNull && node != JNothing
    def getScalarValue(node: JValue) = node match {
      case JBool(b) ⇒ b
      case JInt(i) ⇒ i
      case JDouble(d) ⇒ d
      case JDecimal(d) ⇒ d
      case JString(s) ⇒ s
      case _ ⇒ throw new IllegalStateException(s"$node is not a scalar value")
    }
    def isScalarNode(node: JValue) = node match {
      case _: JBool | _: JNumber | _: JString ⇒ true
      case _ ⇒ false
    }

    def render(node: JValue) = compact(jsonRender(node))
  }

  private object Json4sToInput extends ToInput[JValue, JValue] {
    def toInput(value: JValue) = (value, Json4sInputUnmarshaller)
  }

  implicit def json4sToInput[T <: JValue]: ToInput[T, JValue] =
    Json4sToInput.asInstanceOf[ToInput[T, JValue]]
}

trait Json4sSupportLowPrioImplicits {
  implicit val Json4sInputUnmarshallerJObject =
    json4s.Json4sInputUnmarshaller.asInstanceOf[InputUnmarshaller[JObject]]
}
