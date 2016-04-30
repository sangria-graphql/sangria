package sangria.util

import scala.collection.mutable.ListBuffer

object StringUtil {
  private val camelToUpper = "_*([A-Z][a-z\\d]+)".r

  def camelCaseToUnderscore(name: String) =
    camelToUpper findAllMatchIn name map (_.group(1).toLowerCase) mkString "_"

  /**
    * Given [ A, B, C ] return '"A", "B" or "C"'.
    */
  def quotedOrList(items: Seq[String], limit: Int = 5): String =
    if (items.isEmpty)
      throw new IllegalArgumentException("List is empty")
    else {
      val quoted = items map ("'" + _ + "'") take limit
      val start = quoted dropRight 1
      val last = quoted.last

      if (start.nonEmpty) s"${start mkString ", "} or $last"
      else last
    }

  /**
    * Given an invalid input string and a list of valid options, returns a filtered
    * list of valid options sorted based on their similarity with the input.
    */
  def suggestionList(input: String, options: Seq[String]): Seq[String] = {
    val inputThreshold = input.length / 2

    options
      .map (opt ⇒ opt → lexicalDistance(input, opt))
      .filter (opt ⇒ opt._2 <= math.max(math.max(inputThreshold, opt._1.length / 2), 1))
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
    val d = for (i ← 0 to a.length) yield ListBuffer.fill(b.length + 1)(i)

    for (j ← 1 to b.length) {
      d(0)(j) = j
    }

    for (i ← 1 to a.length; j ← 1 to b.length) {
      val cost = if (a(i - 1) == b(j - 1)) 0 else 1

      d(i)(j) = math.min(math.min(d(i - 1)(j) + 1, d(i)(j - 1) + 1), d(i - 1)(j - 1) + cost)

      if (i > 1 && j > 1 && a(i - 1) == b(j - 2) && a(i - 2) == b(j - 1)) {
        d(i)(j) = math.min(d(i)(j), d(i - 2)(j - 2) + cost)
      }
    }

    d(a.length)(b.length)
  }
}
