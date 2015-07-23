package sangria.integration

import org.json4s.JsonAST._
import sangria.execution.{InputUnmarshaller, ResultMarshaller}

import org.json4s.native.JsonMethods.{render => jsonRender, _}

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

    override def renderCompact(node: JValue) = compact(jsonRender(node))

    override def renderPretty(node: JValue) = pretty(jsonRender(node))
  }

  implicit object Json4sInputUnmarshaller extends InputUnmarshaller[JValue] {
    override type LeafNode = JValue

    override def isDefined(node: JValue) = node != JNull && node != JNothing

    override def getScalarValue(node: JValue) = node match {
      case JBool(b) => b
      case JInt(i) => i.intValue()
      case JDouble(d) => d
      case JString(s) => s
      case _ => throw new IllegalStateException(s"$node is not a scalar value")
    }

    override def isScalarNode(node: JValue) = node match {
      case _: JBool | _: JNumber | _: JString => true
      case _ => false
    }

    override def isMapNode(node: JValue) = node.isInstanceOf[JObject]

    override def getArrayValue(node: JValue) = node.asInstanceOf[JArray].arr

    override def render(node: JValue) = compact(jsonRender(node))

    override def isArrayNode(node: JValue) = node.isInstanceOf[JArray]

    override def getMapValue(node: JValue, key: String) = node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)

    override def emptyNode = JObject()

    override def getRootMapValue(node: JValue, key: String) = node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)

    override def getMapKeys(node: JValue) = node.asInstanceOf[JObject].values.keySet
  }
  
}
