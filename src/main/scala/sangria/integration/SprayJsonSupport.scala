package sangria.integration

import sangria.execution.ResultMarshaller
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
  }

}
