package sangria.parser

import org.parboiled2.ParserInput
import sangria.ast.AstLocation

/** [[sangria.ast.SourceMapper]] for a single GraphQL document. */
class DefaultSourceMapper(val id: String, val parserInput: ParserInput)
    extends sangria.ast.SourceMapper {
  override lazy val source: String = parserInput.sliceString(0, parserInput.length)

  override def renderLocation(location: AstLocation) =
    s"(line ${location.line}, column ${location.column})"

  override def renderLinePosition(location: AstLocation, prefix: String = ""): String =
    parserInput
      .getLine(location.line)
      .replace("\r", "") + "\n" + prefix + (" " * (location.column - 1)) + "^"
}
