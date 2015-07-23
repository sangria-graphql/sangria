package sangria.integration

import play.api.libs.json._
import sangria.execution.{InputUnmarshaller, ResultMarshaller}

object PlayJsonSupport {
  implicit object PlayJsonResultMarshaller extends ResultMarshaller {
    override type Node = JsValue

    override def addArrayNodeElem(array: JsValue, elem: JsValue) = array.asInstanceOf[JsArray].append(elem)

    override def booleanNode(value: Boolean) = JsBoolean(value)

    override def emptyMapNode = JsObject(Seq.empty)

    override def arrayNode(values: Seq[JsValue]) = JsArray(values)

    override def mapNode(keyValues: Seq[(String, JsValue)]) = JsObject(keyValues)

    override def addMapNodeElem(node: JsValue, key: String, value: JsValue) = node.asInstanceOf[JsObject] + (key -> value)

    override def floatNode(value: Double) = JsNumber(value)

    override def isEmptyArrayNode(array: JsValue) = array.asInstanceOf[JsArray].value.isEmpty

    override def emptyArrayNode = JsArray(Seq.empty)

    override def stringNode(value: String) = JsString(value)

    override def intNode(value: Int) = JsNumber(value)

    override def nullNode = JsNull

    override def renderCompact(node: JsValue) = Json.stringify(node)

    override def renderPretty(node: JsValue) = Json.prettyPrint(node)
  }

  implicit object PlayJsonInputUnmarshaller extends InputUnmarshaller[JsValue] {
    override type LeafNode = JsValue

    override def isDefined(node: JsValue) = node != JsNull

    override def getScalarValue(node: JsValue) = node match {
      case JsBoolean(b) => b
      case JsNumber(d) => d.doubleValue()
      case JsString(s) => s
      case _ => throw new IllegalStateException(s"$node is not a scalar value")
    }

    override def isScalarNode(node: JsValue) = node match {
      case _: JsBoolean | _: JsNumber | _: JsString => true
      case _ => false
    }

    override def isMapNode(node: JsValue) = node.isInstanceOf[JsObject]

    override def getArrayValue(node: JsValue) = node.asInstanceOf[JsArray].value

    override def render(node: JsValue) = Json.stringify(node)

    override def isArrayNode(node: JsValue) = node.isInstanceOf[JsArray]

    override def getMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].value get key

    override def emptyNode = JsObject(Seq.empty)

    override def getRootMapValue(node: JsValue, key: String) = node.asInstanceOf[JsObject].value get key

    override def getMapKeys(node: JsValue) = node.asInstanceOf[JsObject].keys
  }
}
