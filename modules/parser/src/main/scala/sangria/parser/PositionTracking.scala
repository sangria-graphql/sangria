package sangria.parser

import org.parboiled2._
import sangria.ast.AstLocation

private[parser] trait PositionTracking { this: Parser =>

  /** The indices (offsets into the source code) of the first characters of each line, ascending. */
  private[this] var lineIdx = new Array[Int](64)
  private[this] var lineCount = 1 // lineIdx(0) == 0

  /** The line (1-based) found by the last lookup. The cursor mostly moves forward, so the next
    * lookup is usually on the same or the following line.
    */
  private[this] var lastLine = 1

  def parseLocations: Boolean
  def sourceId: String

  def trackNewLine: Rule0 = rule {
    test(parseLocations) ~ run {
      // the cursor only moves backwards on backtracking, so a new line start is beyond the last
      if (cursor > lineIdx(lineCount - 1)) {
        if (lineCount == lineIdx.length) lineIdx = java.util.Arrays.copyOf(lineIdx, lineCount * 2)
        lineIdx(lineCount) = cursor
        lineCount += 1
      }
    } | MATCH
  }

  def trackPos: Rule1[Option[AstLocation]] = rule {
    test(parseLocations) ~ push {
      val c = cursor
      // number of line starts <= cursor; the last of them is the start of the current line
      var lo = lastLine
      if (lineIdx(lo - 1) <= c && (lo == lineCount || lineIdx(lo) > c)) {
        // same line as the last lookup
      } else if (lo < lineCount && lineIdx(lo) <= c && (lo + 1 == lineCount || lineIdx(
          lo + 1) > c)) {
        lo += 1
      } else {
        lo = 0
        var hi = lineCount
        while (lo < hi) {
          val mid = (lo + hi) >>> 1
          if (lineIdx(mid) <= c) lo = mid + 1 else hi = mid
        }
      }
      lastLine = lo

      Some(AstLocation(sourceId, c, lo, c - lineIdx(lo - 1) + 1))
    } | push(None)
  }
}
