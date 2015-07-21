package sangria.integration

import org.json4s.JsonAST._
import sangria.execution.ResultMarshaller

object Json4sSupport {
  
  implicit object Json4sResultMarshaller extends ResultMarshaller {
    override type Node = JValue

    override def addArrayNodeElem(array: JValue, elem: JValue) = JArray(array.asInstanceOf[JArray].arr :+ elem)

    override def booleanNode(value: Boolean) = JBool(value)

    override def emptyMapNode = JObject(Nil)

    override def arrayNode(values: Seq[JValue]) = JArray(values.toList)

    override def mapNode(keyValues: Seq[(String, JValue)]) = JObject(keyValues.toList)

    override def addMapNodeElem(node: JValue, key: String, value: JValue) = JObject(node.asInstanceOf[JObject].obj :+ (key -> value))

    override def floatNode(value: Double) = JDouble(value)

    override def isEmptyArrayNode(array: JValue) = array.asInstanceOf[JArray].arr.isEmpty

    override def emptyArrayNode = JArray(Nil)

    override def stringNode(value: String) = JString(value)

    override def intNode(value: Int) = JInt(value)

    override def nullNode = JNull
  }

}
