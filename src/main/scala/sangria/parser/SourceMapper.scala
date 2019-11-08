package sangria.parser

import org.parboiled2.ParserInput
import sangria.ast.AstLocation

trait SourceMapper {
  def id: String
  def source: String
  def renderLocation(location: AstLocation): String
  def renderLinePosition(location: AstLocation, prefix: String = ""): String
}

class DefaultSourceMapper(val id: String, val parserInput: ParserInput) extends SourceMapper {
  lazy val source = parserInput.sliceString(0, parserInput.length)

  def renderLocation(location: AstLocation) =
    s"(line ${location.line}, column ${location.column})"

  def renderLinePosition(location: AstLocation, prefix: String = "") =
    parserInput.getLine(location.line).replace("\r", "") + "\n" + prefix + (" " * (location.column - 1)) + "^"
}

class AggregateSourceMapper(val id: String, val delegates: Vector[SourceMapper]) extends SourceMapper {
  lazy val delegateById: Map[String, SourceMapper] = delegates.iterator.map(d => d.id -> d).toMap

  lazy val source = delegates.map(_.source.trim) mkString "\n\n"

  def renderLocation(location: AstLocation) =
    delegateById.get(location.sourceId).fold("")(sm => sm.renderLocation(location))

  def renderLinePosition(location: AstLocation, prefix: String = "") =
    delegateById.get(location.sourceId).fold("")(sm => sm.renderLinePosition(location, prefix))
}

object AggregateSourceMapper {
  def merge(mappers: Vector[SourceMapper]) = {
    def expand(sm: SourceMapper): Vector[SourceMapper] =
      sm match {
        case agg: AggregateSourceMapper => agg.delegates.flatMap(expand)
        case m => Vector(m)
      }

    new AggregateSourceMapper("merged", mappers.flatMap(expand))
  }
}
