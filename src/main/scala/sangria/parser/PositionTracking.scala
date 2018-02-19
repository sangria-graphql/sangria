package sangria.parser

import org.parboiled2._
import sangria.ast.AstLocation

trait PositionTracking { this: Parser ⇒
  private var lineIdx = Vector(0)

  def sourceId: String

  def trackNewLine: Rule0 = rule { run(if (!(lineIdx contains cursor)) lineIdx = lineIdx :+ cursor) }

  def trackPos = rule {
    push {
      val currLines = lineIdx.takeWhile(_ <= cursor)

      AstLocation(sourceId, cursor, currLines.size, cursor - currLines.last + 1)
    }
  }
}
