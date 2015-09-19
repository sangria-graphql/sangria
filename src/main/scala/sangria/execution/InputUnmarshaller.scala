package sangria.execution

import sangria.util.tag._
import sangria.util.tag

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

sealed trait ScalaVariables

object InputUnmarshaller {
  def mapVars(args: Map[String, Any]) = tag[ScalaVariables](args)
  def mapVars(args: (String, Any)*) = tag[ScalaVariables](args.toMap)

  def emptyMapVars = tag[ScalaVariables](Map.empty[String, Any])

  implicit def scalaInputUnmarshaller[T] = new InputUnmarshaller[T @@ ScalaVariables] {
      def getRootMapValue(node: T @@ ScalaVariables, key: String) = node.asInstanceOf[Map[String, Any]] get key map (v => tag[ScalaVariables](v.asInstanceOf[T]))

      def isMapNode(node: T @@ ScalaVariables) = node.isInstanceOf[Map[_, _]]
      def getMapValue(node: T @@ ScalaVariables, key: String) = node.asInstanceOf[Map[String, _]] get key map (v => tag[ScalaVariables](v.asInstanceOf[T]))
      def getMapKeys(node: T @@ ScalaVariables) = node.asInstanceOf[Map[String, _]].keySet

      def isArrayNode(node: T @@ ScalaVariables) = node.isInstanceOf[Seq[_]]
      def getListValue(node: T @@ ScalaVariables) = node.asInstanceOf[Seq[_]] map (v => tag[ScalaVariables](v.asInstanceOf[T]))

      def isDefined(node: T @@ ScalaVariables) = node != null
      def isScalarNode(node: T @@ ScalaVariables) = !(isMapNode(node) || isArrayNode(node))
      def getScalarValue(node: T @@ ScalaVariables) = node

      def render(node: T @@ ScalaVariables) = if (node == null) "null" else node.toString
  }
}