package sangria.integration

import sangria.execution.{InputUnmarshaller, ResultMarshaller}
import spray.json._


object SprayJsonSupport {

  implicit object SprayJsonResultMarshaller extends ResultMarshaller {
    override type Node = JsValue

    override def addArrayNodeElem(array: JsValue, elem: JsValue) = JsArray(array.asInstanceOf[JsArray].elements :+ elem)

    override def booleanNode(value: Boolean) = JsBoolean(value)

    override def emptyMapNode = JsObject.empty

    override def arrayNode(values: Seq[JsValue]) = JsArray(values.toVector)

    override def mapNode(keyValues: Seq[(String, JsValue)]) = JsObject(keyValues: _*)

    override def addMapNodeElem(node: JsValue, key: String, value: JsValue) = JsObject(node.asInstanceOf[JsObject].fields + (key -> value))

    override def floatNode(value: Double) = JsNumber(value)

    override def isEmptyArrayNode(array: JsValue) = array.asInstanceOf[JsArray].elements.isEmpty

    override def emptyArrayNode = JsArray.empty

    override def stringNode(value: String) = JsString(value)

    override def intNode(value: Int) = JsNumber(value)

    override def nullNode = JsNull

    override def renderCompact(node: JsValue) = node.compactPrint

    override def renderPretty(node: JsValue) = node.prettyPrint

    override def bigIntNode(value: BigInt) = JsNumber(value)

    override def bigDecimalNode(value: BigDecimal) = JsNumber(value)
  }

  implicit object SprayJsonInputUnmarshaller extends InputUnmarshaller[JsValue] {
    override type LeafNode = JsValue

    override def isDefined(node: JsValue) = node != JsNull

    override def getScalarValue(node: JsValue) = node match {
      case JsBoolean(b) => b
      case JsNumber(d) => d.toBigIntExact getOrElse d
      case JsString(s) => s
      case _ => throw new IllegalStateException(s"$node is not a scalar value")
    }

    override def isScalarNode(node: JsValue) = node match {
      case _: JsBoolean | _: JsNumber | _: JsString => true
      case _ => false
    }

    override def isMapNode(node: JsValue) = node.isInstanceOf[JsObject]

    override def getArrayValue(node: JsValue) = node.asInstanceOf[JsArray].elements

    override def render(node: JsValue) = node.compactPrint

    override def isArrayNode(node: JsValue) = node.isInstanceOf[JsArray]

    override def getMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].fields get key

    override def emptyNode = JsObject.empty

    override def getRootMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].fields get key

    override def getMapKeys(node: JsValue) = node.asInstanceOf[JsObject].fields.keySet
  }
}
