package sangria.execution

import sangria.marshalling.ResultMarshaller
import sangria.ast
import sangria.schema.ObjectType

class ExecutionPath private (_path: List[Any], cacheKeyPath: ExecutionPath.PathCacheKey) {
  lazy val path: List[Any] = _path.reverse

  def add(field: ast.Field, parentType: ObjectType[_, _]) =
    new ExecutionPath(field.outputName :: _path, parentType.name :: field.outputName :: cacheKey)

  def withIndex(idx: Int) = new ExecutionPath(idx :: _path, cacheKey)

  def isEmpty = _path.isEmpty
  def nonEmpty = _path.nonEmpty

  /** @return
    *   last index in the path, if available
    */
  def lastIndex: Option[Int] = _path.headOption.collect { case i: Int => i }

  /** @return
    *   the size of the path excluding the indexes
    */
  def size = cacheKeyPath.size / 2

  def marshal(m: ResultMarshaller): m.Node = m.arrayNode(_path.reverse.iterator.map {
    case s: String => m.scalarNode(s, "String", Set.empty)
    case i: Int => m.scalarNode(i, "Int", Set.empty)
  }.toVector)

  def cacheKey: ExecutionPath.PathCacheKey = cacheKeyPath

  override def toString = _path.reverse.foldLeft("") {
    case ("", str: String) => str
    case (acc, str: String) => acc + "." + str
    case (acc, idx: Int) => acc + "[" + idx + "]"

    case ("", other) => other.toString
    case (acc, other) => acc + "." + other.toString
  }
}

object ExecutionPath {
  type PathCacheKey = List[String]

  val empty = new ExecutionPath(List.empty, List.empty)
}
