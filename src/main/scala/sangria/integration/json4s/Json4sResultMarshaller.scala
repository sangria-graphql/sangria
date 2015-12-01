package sangria.integration.json4s

import org.json4s.JsonAST._
import sangria.marshalling.ResultMarshaller

abstract class Json4sResultMarshaller extends ResultMarshaller {
  type Node = JValue

  def emptyMapNode = JObject(Nil)
  def mapNode(keyValues: Seq[(String, JValue)]) = JObject(keyValues.toList)
  def addMapNodeElem(node: JValue, key: String, value: JValue) = JObject(node.asInstanceOf[JObject].obj :+ (key → value))

  def arrayNode(values: Vector[JValue]) = JArray(values.toList)

  def stringNode(value: String) = JString(value)
  def floatNode(value: Double) = JDouble(value)
  def booleanNode(value: Boolean) = JBool(value)
  def intNode(value: Int) = JInt(value)
  def bigIntNode(value: BigInt) = JInt(value)
  def bigDecimalNode(value: BigDecimal) = JDecimal(value)

  def nullNode = JNull
}