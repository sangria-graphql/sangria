package sangria.parser

import org.parboiled2.{ParserInput, Position}

trait SourceMapper {
  def source: String
  def renderLocation(position: Position): String
  def renderLinePosition(position: Position): String
}

class Parboiled2SourceMapper(parserInput: ParserInput) extends SourceMapper {
  override def source = parserInput.sliceString(0, parserInput.length)

  override def renderLocation(position: Position) =
    s"(line ${position.line}, column ${position.column})"

  override def renderLinePosition(position: Position) =
    parserInput.getLine(position.line).replace("\r", "") + "\n" + (" " * (position.column - 1)) + "^"
}


