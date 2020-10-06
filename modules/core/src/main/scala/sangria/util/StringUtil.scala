package sangria.util

import java.util.Locale

import scala.collection.mutable.ListBuffer

object StringUtil {
  private val camelToUpper = "_*([A-Z][a-z\\d]+)".r

  def camelCaseToUnderscore(name: String) =
    camelToUpper findAllMatchIn name map (_.group(1).toLowerCase) mkString "_"

  def orList(items: Seq[String], limit: Int = 5) =
    if (items.isEmpty)
      throw new IllegalArgumentException("List is empty")
    else {
      val limited = items take limit
      val start = limited dropRight 1
      val last = limited.last

      if (start.nonEmpty) s"${start mkString ", "} or $last"
      else last
    }

  /**
    * Given [ A, B, C ] return '"A", "B" or "C"'.
    */
  def quotedOrList(items: Seq[String], limit: Int = 5): String =
    orList(items map ("'" + _ + "'"))

  /**
    * Given an invalid input string and a list of valid options, returns a filtered
    * list of valid options sorted based on their similarity with the input.
    */
  def suggestionList(input: String, options: Seq[String]): Seq[String] = {
    val inputLength = input.length
    val inputThreshold = math.max(inputLength / 2, 1)

    options
      .iterator
      // filter out options where the lenght is too different to avoid computing the lexical distance
      .filter { opt =>
        val length = opt.length
        length >= inputLength - inputThreshold && length <= inputLength + inputThreshold
      }
      .map (opt => opt -> lexicalDistance(input, opt))
      .filter (opt => opt._2 <= math.max(inputThreshold, opt._1.length / 2))
      .toList
      .sortBy (_._2)
      .map (_._1)
  }

  /**
    * Computes the lexical distance between strings A and B.
    *
    * The "distance" between two strings is given by counting the minimum number
    * of edits needed to transform string A into string B. An edit can be an
    * insertion, deletion, or substitution of a single character, or a swap of two
    * adjacent characters.
    *
    * This distance can be useful for detecting typos in input or sorting
    *
    * @return distance in number of edits
    */
  def lexicalDistance(a: String, b: String): Int = {
    val d = for (i <- 0 to a.length) yield ListBuffer.fill(b.length + 1)(i)

    for (j <- 1 to b.length) {
      d(0)(j) = j
    }

    for (i <- 1 to a.length; j <- 1 to b.length) {
      val cost = if (a(i - 1) == b(j - 1)) 0 else 1

      d(i)(j) = math.min(math.min(d(i - 1)(j) + 1, d(i)(j - 1) + 1), d(i - 1)(j - 1) + cost)

      if (i > 1 && j > 1 && a(i - 1) == b(j - 2) && a(i - 2) == b(j - 1)) {
        d(i)(j) = math.min(d(i)(j), d(i - 2)(j - 2) + cost)
      }
    }

    d(a.length)(b.length)
  }

  def escapeBlockString(str: String) = str.replaceAll("\"\"\"", "\\\\\"\"\"")

  def escapeString(str: String) =
    str flatMap {
      case ch if ch > 0xfff => "\\u" + charHex(ch)
      case ch if ch > 0xff => "\\u0" + charHex(ch)
      case ch if ch > 0x7f => "\\u00" + charHex(ch)
      case ch if ch < 32 =>
        ch match {
          case '\b' => "\\b"
          case '\n' => "\\n"
          case '\t' => "\\t"
          case '\f' => "\\f"
          case '\r' => "\\r"
          case ch if ch > 0xf => "\\u00" + charHex(ch)
          case ch => "\\u000" + charHex(ch)
        }
      case '"' => "\\\""
      case '\\' => "\\\\"
      case ch => ch.toString
    }

  def charHex(ch: Char): String =
    Integer.toHexString(ch).toUpperCase(Locale.ENGLISH)

  /**
    * Produces the value of a block string from its parsed raw value, similar to
    * Coffeescript's block string, Python's docstring trim or Ruby's strip_heredoc.
    *
    * This implements the GraphQL spec's BlockStringValue() static algorithm.
    */
  def blockStringValue(rawString: String): String = {
    val lines = rawString.split("""\r\n|[\n\r]""")
    val lineSizes = lines.map(l => l -> leadingWhitespace(l))
    val commonIndentLines = lineSizes.drop(1).collect {case (line, size) if size != line.length => size}
    val strippedLines =
      if (commonIndentLines.nonEmpty) {
        val commonIndent = commonIndentLines.min

        lines.take(1) ++ lines.drop(1).map(_.drop(commonIndent))
      } else lines
    val trimmedLines = strippedLines.reverse.dropWhile(isBlank).reverse.dropWhile(isBlank)

    trimmedLines.mkString("\n")
  }

  private def leadingWhitespace(str: String) =  {
    var i = 0

    while (i < str.length && (str.charAt(i) == ' ' || str.charAt(i) == '\t')) i += 1

    i
  }

  private def isBlank(str: String) = leadingWhitespace(str) == str.length
}
