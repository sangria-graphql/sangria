package sangria.parser

import org.parboiled2.ParserInput
import sangria.ast.AstLocation

/** Set of functions that convert a [[AstLocation GraphQL source code location]] to human-readable
  * strings.
  *
  * When rendering the results of a GraphQL document parse, it's helpful to describe where parsing
  * failed. This is the interface to that facility.
  */
trait SourceMapper {

  /** Identifier for the GraphQL document being parsed. Should be unique. */
  def id: String

  /** The GraphQL source code mapped by this object. */
  def source: String

  /** Return a description of the given location. */
  def renderLocation(location: AstLocation): String

  /** Return an indication of the line position of the given location.
    *
    * Useful for pointing to the location of a parsing error.
    *
    * @param prefix
    *   prefix to attach to the returned string
    */
  def renderLinePosition(location: AstLocation, prefix: String = ""): String
}

/** [[SourceMapper]] for a single GraphQL document. */
class DefaultSourceMapper(val id: String, val parserInput: ParserInput) extends SourceMapper {
  override lazy val source: String = parserInput.sliceString(0, parserInput.length)

  override def renderLocation(location: AstLocation) =
    s"(line ${location.line}, column ${location.column})"

  override def renderLinePosition(location: AstLocation, prefix: String = ""): String =
    parserInput
      .getLine(location.line)
      .replace("\r", "") + "\n" + prefix + (" " * (location.column - 1)) + "^"
}

/** [[SourceMapper]] for potentially multiple GraphQL documents.
  *
  * Sometimes it's necessary to compose a GraphQL document from multiple component documents; this
  * class provides the corresponding `SourceMapper` to support that.
  *
  * @param id
  *   Identifier for the combined document.
  * @param delegates
  *   The component documents.
  */
class AggregateSourceMapper(val id: String, val delegates: Vector[SourceMapper])
    extends SourceMapper {
  lazy val delegateById: Map[String, SourceMapper] = delegates.iterator.map(d => d.id -> d).toMap

  override lazy val source: String = delegates.map(_.source.trim).mkString("\n\n")

  override def renderLocation(location: AstLocation): String =
    delegateById.get(location.sourceId).fold("")(sm => sm.renderLocation(location))

  override def renderLinePosition(location: AstLocation, prefix: String = ""): String =
    delegateById.get(location.sourceId).fold("")(sm => sm.renderLinePosition(location, prefix))
}

object AggregateSourceMapper {
  private[this] def expand(sm: SourceMapper): Vector[SourceMapper] = sm match {
    case agg: AggregateSourceMapper => agg.delegates.flatMap(expand)
    case m => Vector(m)
  }

  def merge(mappers: Vector[SourceMapper]): AggregateSourceMapper =
    new AggregateSourceMapper("merged", mappers.flatMap(expand))
}
