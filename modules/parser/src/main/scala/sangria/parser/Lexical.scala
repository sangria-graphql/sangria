package sangria.parser

/** Lexical analysis tools described in the GraphQL specification. */
private[parser] object Lexical {

  /** Regex that matches line termination sequences. */
  private[this] val newlineRegex = """\r\n|[\n\r]""".r

  /** Produces the value of a block string from its parsed raw value, similar to Coffeescript's
    * block string, Python's docstring trim or Ruby's strip_heredoc.
    *
    * This implements the GraphQL spec's BlockStringValue() static algorithm.
    *
    * @see
    *   [[https://spec.graphql.org/October2021/#BlockStringValue()]]
    */
  def blockStringValue(rawValue: String): String = {
    val lines = newlineRegex.split(rawValue)
    val lineSizes = lines.map(l => l -> leadingWhitespace(l))
    val commonIndentLines =
      lineSizes.drop(1).collect { case (line, size) if size != line.length => size }
    val strippedLines = if (commonIndentLines.nonEmpty) {
      val commonIndent = commonIndentLines.min
      lines.take(1) ++ lines.drop(1).map(_.drop(commonIndent))
    } else lines

    val trimmedLines = strippedLines.reverse.dropWhile(isBlank).reverse.dropWhile(isBlank)
    trimmedLines.mkString("\n")
  }

  private[this] def leadingWhitespace(str: String) = {
    var i = 0
    while (i < str.length && (str.charAt(i) == ' ' || str.charAt(i) == '\t')) i += 1
    i
  }

  private[this] def isBlank(str: String) = leadingWhitespace(str) == str.length
}
