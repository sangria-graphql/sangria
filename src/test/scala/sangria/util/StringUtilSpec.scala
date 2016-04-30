package sangria.util

import org.scalatest.{Matchers, WordSpec}
import sangria.util.StringUtil._

class StringUtilSpec extends WordSpec with Matchers {
  "camelCaseToUnderscore" should {
    "convert camel-case identifiers to underscore-based one" in {
      camelCaseToUnderscore("FooBar") should be ("foo_bar")
      camelCaseToUnderscore("Foo1Bar") should be ("foo1_bar")
      camelCaseToUnderscore("FooBar_Baz") should be ("foo_bar_baz")
      camelCaseToUnderscore("_Foo_Bar_") should be ("foo_bar")
    }
  }

  "quotedOrList" should {
    "Does not accept an empty list" in {
      an [IllegalArgumentException] should be thrownBy quotedOrList(Nil)
    }

    "Returns single quoted item" in {
      quotedOrList(Seq("A")) should be ("'A'")
    }

    "Returns two item list" in {
      quotedOrList(Seq("A", "B")) should be ("'A' or 'B'")
    }

    "Returns comma separated many item list" in {
      quotedOrList(Seq("A", "B", "C")) should be ("'A', 'B' or 'C'")
    }

    "Limits to five items" in {
      quotedOrList(Seq("A", "B", "C", "D", "E", "F")) should be ("'A', 'B', 'C', 'D' or 'E'")
    }
  }

  "suggestionList" should {
    "Returns results when input is empty" in {
      suggestionList("", Seq("a")) should be (Seq("a"))
    }

    "Returns empty array when there are no options" in {
      suggestionList("input", Seq.empty) should be (Seq.empty)
    }
    
    "Returns options sorted based on similarity" in {
      suggestionList("abc", Seq("a", "ab", "abc")) should be (Seq("abc", "ab"))
    }
  }
}
