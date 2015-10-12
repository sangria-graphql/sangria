package sangria.integration.json4s

import org.json4s.JsonAST._
import sangria.integration.InputUnmarshaller

abstract class Json4sInputUnmarshaller extends InputUnmarshaller[JValue] {
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
    case JLong(l) ⇒ l
    case JDecimal(d) ⇒ d
    case JString(s) ⇒ s
    case _ ⇒ throw new IllegalStateException(s"$node is not a scalar value")
  }
  def isScalarNode(node: JValue) = node match {
    case _: JBool | _: JNumber | _: JString ⇒ true
    case _ ⇒ false
  }
}
