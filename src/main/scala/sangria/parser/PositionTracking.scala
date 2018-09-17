package sangria.parser

import org.parboiled2._
import sangria.ast.AstLocation

import scala.annotation.tailrec

trait PositionTracking { this: Parser ⇒
  private var lineIdx = Array(0)

  def parseLocations: Boolean
  def sourceId: String

  def trackNewLine: Rule0 = rule {
    test(parseLocations) ~ run {
      if (!contains(lineIdx, cursor)) lineIdx = lineIdx :+ cursor
    } | MATCH
  }

  def trackPos: Rule1[Option[AstLocation]] = rule {
    test(parseLocations) ~ push {
      val (size, lastCursor) = findLastItem(lineIdx, cursor)

      Some(AstLocation(sourceId, cursor, size, cursor - lastCursor + 1))
    } | push(None)
  }

  /**
    * Returns true if the array contains the given item.
    *
    * Assumes that the array is sorted in ascending order.
    */
  private def contains(arr: Array[Int], item: Int) = {
    @tailrec def go(i: Int): Boolean = {
      if (i < 0) false
      else if (arr(i) < item) false // no remaining values will match as the array is sorted
      else if (arr(i) == item) true
      else go(i - 1)
    }

    go(arr.length - 1)
  }

  /**
    * Returns the first item that is less than or equal to the given item
    * along with the number of elements that come before it.
    *
    * Assumes that the array is sorted in ascending order.
    */
  private def findLastItem(arr: Array[Int], item: Int) = {
    @tailrec def go(i: Int, last: Int): (Int, Int) = {
      if (i < 0) (i + 1, last)
      else if (arr(i) <= item) (i + 1, arr(i))
      else go(i - 1, arr(i))
    }

    go(arr.length - 1, 0)
  }
}
