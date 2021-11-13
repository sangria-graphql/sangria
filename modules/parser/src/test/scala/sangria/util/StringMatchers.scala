package sangria.util

import org.scalactic.AbstractStringUniformity

trait StringMatchers {
  protected def strippedOfCarriageReturns: AbstractStringUniformity = new AbstractStringUniformity {
    def normalized(s: String): String = stripCarriageReturns(s)
  }

  protected def stripCarriageReturns(s: String): String = s.replaceAll("\\r", "")

  protected implicit class StringHelpers(s: String) {
    def stripCR: String = stripCarriageReturns(s)
  }
}
