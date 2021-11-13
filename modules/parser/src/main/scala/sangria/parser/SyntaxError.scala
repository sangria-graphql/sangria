package sangria.parser

import org.parboiled2.{ErrorFormatter, ParseError, Parser, ParserInput}

case class SyntaxError(parser: Parser, input: ParserInput, originalError: ParseError)
    extends Exception(originalError) {
  lazy val formattedError: String = formattedError()

  def formattedError(showPosition: Boolean = true): String =
    if (showPosition)
      parser.formatError(originalError)
    else
      parser.formatError(
        originalError,
        formatter = new ErrorFormatter(showPosition = false, showLine = false))

  override def getMessage = s"Syntax error while parsing GraphQL query. $formattedError"
}
