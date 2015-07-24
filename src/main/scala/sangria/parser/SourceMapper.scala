package sangria.parser

import org.parboiled2.{ParserInput, Position}

trait SourceMapper {
  def renderPosition(position: Position): String
}

class Parboiled2SourceMapper(parserInput: ParserInput) extends SourceMapper {
  override def renderPosition(position: Position) = {
    parserInput.getLine(position.line).replace("\r", "") + "\n" + (" " * (position.column - 1)) + "^"
  }
}


