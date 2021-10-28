package sangria.execution

import sangria.marshalling.ResultMarshaller
import sangria.ast
import sangria.schema.ObjectType

class ExecutionPath private (
    _path: List[Any],
    cacheKeyPath: ExecutionPath.PathCacheKey,
    pathSizeWithoutIndexes: Int) {
  lazy val path: List[Any] = _path.reverse

  def add(field: ast.Field, parentType: ObjectType[_, _]) =
    new ExecutionPath(
      field.outputName :: _path,
      parentType.name :: field.outputName :: cacheKey,
      pathSizeWithoutIndexes = pathSizeWithoutIndexes + 1)

  def withIndex(idx: Int) = new ExecutionPath(idx :: _path, cacheKey, pathSizeWithoutIndexes)

  def isEmpty = _path.isEmpty
  def nonEmpty = _path.nonEmpty

  /** @return
    *   last index in the path, if available
    */
  def lastIndex: Option[Int] = _path.headOption.collect { case i: Int => i }

  /** @return
    *   the size of the path excluding the indexes
    */
  def size = pathSizeWithoutIndexes

  def marshal(m: ResultMarshaller): m.Node = m.arrayNode(_path.reverseIterator.map {
    case s: String => m.scalarNode(s, "String", Set.empty)
    case i: Int => m.scalarNode(i, "Int", Set.empty)
  }.toVector)

  def cacheKey: ExecutionPath.PathCacheKey = cacheKeyPath

  override def toString = _path.reverseIterator
    .foldLeft(new StringBuilder) {
      case (builder, str: String) =>
        if (builder.isEmpty) builder.append(str) else builder.append(".").append(str)
      case (builder, idx: Int) => builder.append("[").append(idx).append("]")

      case (builder, other) =>
        if (builder.isEmpty) builder.append(other.toString())
        else builder.append(".").append(other.toString)
    }
    .result()
}

object ExecutionPath {
  type PathCacheKey = List[String]

  val empty = new ExecutionPath(List.empty, List.empty, pathSizeWithoutIndexes = 0)
}
