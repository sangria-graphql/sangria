package sangria.util

import org.scalactic.AbstractStringUniformity

trait StringMatchers {
  def strippedOfCarriageReturns = new AbstractStringUniformity {
    def normalized(s: String): String = stripCarriageReturns(s)
  }

  def stripCarriageReturns(s: String) = s.replaceAll("\\r", "")

  val whitespace = "\\s+".r

  def normalizeAllWhitespace(str: String): String =
    whitespace.replaceAllIn(str, " ").trim

  implicit class StringHelpers(s: String) {
    def stripCR: String = stripCarriageReturns(s)
    def normalizeAllWS: String = normalizeAllWhitespace(s)
  }
}
