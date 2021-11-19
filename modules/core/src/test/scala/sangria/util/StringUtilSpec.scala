package sangria.util

import sangria.util.StringUtil._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class StringUtilSpec extends AnyWordSpec with Matchers with StringMatchers {
  "camelCaseToUnderscore" should {
    "convert camel-case identifiers to underscore-based one" in {
      camelCaseToUnderscore("FooBar") should be("foo_bar")
      camelCaseToUnderscore("Foo1Bar") should be("foo1_bar")
      camelCaseToUnderscore("FooBar_Baz") should be("foo_bar_baz")
      camelCaseToUnderscore("_Foo_Bar_") should be("foo_bar")
    }
  }

  "quotedOrList" should {
    "Does not accept an empty list" in {
      an[IllegalArgumentException] should be thrownBy quotedOrList(Nil)
    }

    "Returns single quoted item" in {
      quotedOrList(Seq("A")) should be("'A'")
    }

    "Returns two item list" in {
      quotedOrList(Seq("A", "B")) should be("'A' or 'B'")
    }

    "Returns comma separated many item list" in {
      quotedOrList(Seq("A", "B", "C")) should be("'A', 'B' or 'C'")
    }

    "Limits to five items" in {
      quotedOrList(Seq("A", "B", "C", "D", "E", "F")) should be("'A', 'B', 'C', 'D' or 'E'")
    }
  }

  "suggestionList" should {
    "Returns results with only one character when input is empty" in {
      suggestionList("", Seq("a")) should be(Seq("a"))
      suggestionList("", Seq("ab", "b")) should be(Seq("b"))
    }

    "Returns empty array when there are no options" in {
      suggestionList("input", Seq.empty) should be(Seq.empty)
    }

    "Returns options sorted based on similarity" in {
      suggestionList("abc", Seq("a", "ab", "abc")) should be(Seq("abc", "ab"))
      suggestionList("descritpion", Seq("description", "descriptionn", "descriptionnn")) should be(
        Seq("description", "descriptionn", "descriptionnn"))

      val options = "description" :: (1 to 50).map(i => s"name_$i").toList
      suggestionList("descritpion", options) should be(Seq("description"))
    }
  }
}
