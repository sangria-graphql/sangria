package sangria.parser

import org.parboiled2._

trait PositionTracking { this: Parser ⇒
  private var lineIdx = Vector(0)

  def trackNewLine: Rule0 = rule { run(if (!(lineIdx contains cursor)) lineIdx = lineIdx :+ cursor) }

  def trackPos = rule {
    push {
      val currLines = lineIdx.takeWhile(_ <= cursor)
      Position(cursor, currLines.size, cursor - currLines.last + 1)
    }
  }
}
