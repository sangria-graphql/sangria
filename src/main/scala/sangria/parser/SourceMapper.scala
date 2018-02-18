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
  override def source = parserInput.sliceString(0, parserInput.length)

  override def renderLocation(location: AstLocation) =
    s"(line ${location.line}, column ${location.column})"

  override def renderLinePosition(location: AstLocation, prefix: String = "") =
    parserInput.getLine(location.line).replace("\r", "") + "\n" + prefix + (" " * (location.column - 1)) + "^"
}


