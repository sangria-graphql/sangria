package sangria.execution

import sangria.marshalling.ResultMarshaller
import sangria.ast
import sangria.schema.ObjectType

class ExecutionPath private (
    _path: List[Any],
    cacheKeyPath: List[String],
    pathSizeWithoutIndexes: Int) {
  lazy val path: Vector[Any] = _path.reverseIterator.toVector

  def add(field: ast.Field, parentType: ObjectType[_, _]): ExecutionPath =
    new ExecutionPath(
      field.outputName :: _path,
      parentType.name :: field.outputName :: cacheKeyPath,
      pathSizeWithoutIndexes = pathSizeWithoutIndexes + 1)

  def withIndex(idx: Int): ExecutionPath =
    new ExecutionPath(idx :: _path, cacheKeyPath, pathSizeWithoutIndexes)

  def isEmpty: Boolean = _path.isEmpty
  def nonEmpty: Boolean = _path.nonEmpty

  /** @return
    *   last index in the path, if available
    */
  def lastIndex: Option[Int] = _path.headOption.collect { case i: Int => i }

  /** @return
    *   the size of the path excluding the indexes
    */
  def size: Int = pathSizeWithoutIndexes

  def marshal(m: ResultMarshaller): m.Node = m.arrayNode(_path.reverseIterator.map {
    case s: String => m.scalarNode(s, "String", Set.empty)
    case i: Int => m.scalarNode(i, "Int", Set.empty)
  }.toVector)

  def cacheKey: ExecutionPath.PathCacheKey = cacheKeyPath.reverseIterator.toVector
  def cacheKeyReversed: ExecutionPath.PathCacheKeyReversed = cacheKeyPath
  def cacheKeyReversedIterator: Iterator[String] = cacheKeyPath.iterator

  override def toString: String = _path.reverseIterator
    .foldLeft(new StringBuilder) {
      case (builder, str: String) =>
        if (builder.isEmpty) builder.append(str) else builder.append(".").append(str)
      case (builder, idx: Int) => builder.append("[").append(idx).append("]")

      case (builder, other) =>
        if (builder.isEmpty) builder.append(other.toString)
        else builder.append(".").append(other.toString)
    }
    .result()
}

object ExecutionPath {
  type PathCacheKey = Vector[String]
  type PathCacheKeyReversed = List[String]

  val empty = new ExecutionPath(List.empty, List.empty, pathSizeWithoutIndexes = 0)
}
