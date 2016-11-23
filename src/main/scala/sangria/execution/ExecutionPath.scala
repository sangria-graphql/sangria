package sangria.execution

import sangria.marshalling.ResultMarshaller
import sangria.ast
import sangria.schema.ObjectType

case class ExecutionPath private (path: Vector[Any], cacheKeyPath: ExecutionPath.PathCacheKey) {
  def add(field: ast.Field, parentType: ObjectType[_, _]) =
    new ExecutionPath(path :+ field.outputName, cacheKey :+ field.outputName :+ parentType.name)

  def withIndex(idx: Int) = new ExecutionPath(path :+ idx, cacheKey)

  def isEmpty = path.isEmpty
  def nonEmpty = path.nonEmpty

  /**
    * @return last index in the path, if available
    */
  def lastIndex: Option[Int] = path.lastOption.collect {case i: Int ⇒ i}

  /**
    * @return the size of the path excluding the indexes
    */
  def size = cacheKeyPath.size / 2

  def marshal(m: ResultMarshaller): m.Node = m.arrayNode(path.map {
    case s: String ⇒ m.scalarNode(s, "String", Set.empty)
    case i: Int ⇒ m.scalarNode(i, "Int", Set.empty)
  })

  def cacheKey: ExecutionPath.PathCacheKey = cacheKeyPath

  override def toString = path.foldLeft("") {
    case ("", str: String) ⇒ str
    case (acc, str: String) ⇒ acc + "." + str
    case (acc, idx: Int) ⇒ acc + "[" + idx + "]"

    case ("", other) ⇒ other.toString
    case (acc, other) ⇒ acc + "." + other.toString
  }
}

object ExecutionPath {
  type PathCacheKey = Vector[String]

  val empty = new ExecutionPath(Vector.empty, Vector.empty)
}