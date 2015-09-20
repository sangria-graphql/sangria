package sangria.integration

import sangria.util.tag
import sangria.util.tag._

trait InputUnmarshaller[Node] {
  def getRootMapValue(node: Node, key: String): Option[Node]

  def isMapNode(node: Node): Boolean
  def getMapValue(node: Node, key: String): Option[Node]
  def getMapKeys(node: Node): Traversable[String]

  def isArrayNode(node: Node): Boolean
  def getListValue(node: Node): Seq[Node]

  def isDefined(node: Node): Boolean
  def isScalarNode(node: Node): Boolean

  // Scalar values are Scala String, Int, Double, Boolean and Enum values defined in the schema
  def getScalarValue(node: Node): Any

  def render(node: Node): String
}

sealed trait ScalaInput

object InputUnmarshaller {
  def mapVars(args: Map[String, Any]) = tag[ScalaInput](args)
  def mapVars(args: (String, Any)*) = tag[ScalaInput](args.toMap)

  def emptyMapVars = tag[ScalaInput](Map.empty[String, Any])

  implicit def scalaInputUnmarshaller[T] = new InputUnmarshaller[T @@ ScalaInput] {
      def getRootMapValue(node: T @@ ScalaInput, key: String) = node.asInstanceOf[Map[String, Any]] get key map (v => tag[ScalaInput](v.asInstanceOf[T]))

      def isMapNode(node: T @@ ScalaInput) = node.isInstanceOf[Map[_, _]]
      def getMapValue(node: T @@ ScalaInput, key: String) = node.asInstanceOf[Map[String, _]] get key map (v => tag[ScalaInput](v.asInstanceOf[T]))
      def getMapKeys(node: T @@ ScalaInput) = node.asInstanceOf[Map[String, _]].keySet

      def isArrayNode(node: T @@ ScalaInput) = node.isInstanceOf[Seq[_]]
      def getListValue(node: T @@ ScalaInput) = node.asInstanceOf[Seq[_]] map (v => tag[ScalaInput](v.asInstanceOf[T]))

      def isDefined(node: T @@ ScalaInput) = node != null
      def isScalarNode(node: T @@ ScalaInput) = !(isMapNode(node) || isArrayNode(node))
      def getScalarValue(node: T @@ ScalaInput) = node

      def render(node: T @@ ScalaInput) = if (node == null) "null" else node.toString
  }
}