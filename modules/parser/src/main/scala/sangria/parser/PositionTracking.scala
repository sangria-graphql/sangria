package sangria.parser

import org.parboiled2._
import sangria.ast.AstLocation

import scala.collection.Searching._
import scala.collection.mutable.ArrayBuffer

private[parser] trait PositionTracking { this: Parser =>

  /** The indices (offsets into the source code) of the first characters of each line. */
  private[this] val lineIdx = ArrayBuffer[Int](0)

  def parseLocations: Boolean
  def sourceId: String

  def trackNewLine: Rule0 = rule {
    test(parseLocations) ~ run {
      lineIdx.search(cursor) match {
        case _: InsertionPoint => lineIdx += cursor
        case _ => None
      }
    } | MATCH
  }

  def trackPos: Rule1[Option[AstLocation]] = rule {
    test(parseLocations) ~ push {
      val (line, lastCursor) = findLastItem(lineIdx, cursor)

      Some(AstLocation(sourceId, cursor, line, cursor - lastCursor + 1))
    } | push(None)
  }

  /** Returns the first item that is less than or equal to the given item along with the number of
    * elements that come before it.
    *
    * Assumes that the array is sorted in ascending order.
    */
  private def findLastItem(arr: ArrayBuffer[Int], item: Int) =
    if (arr.nonEmpty) {
      arr.search(item) match {
        case Found(idx) => (idx + 1, arr(idx))
        case InsertionPoint(idx) if idx == 0 => (idx, arr(idx))
        case InsertionPoint(idx) => (idx, arr(idx - 1))
      }
    } else {
      (0, 0)
    }
}
