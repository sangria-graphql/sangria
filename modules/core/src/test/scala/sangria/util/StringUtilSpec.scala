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

  "blockStringValue" should {
    "removes uniform indentation from a string" in {
      blockStringValue(
        """
          |    Hello,
          |      World!
          |
          |    Yours,
          |      GraphQL.
          |""".stripMargin
      ) should equal(
        """Hello,
          |  World!
          |
          |Yours,
          |  GraphQL.""".stripMargin
      )(after.being(strippedOfCarriageReturns))
    }

    "removes empty leading and trailing lines" in {
      blockStringValue(
        """
          |
          |    Hello,
          |      World!
          |
          |    Yours,
          |      GraphQL.
          |
          |""".stripMargin
      ) should equal(
        """Hello,
          |  World!
          |
          |Yours,
          |  GraphQL.""".stripMargin
      )(after.being(strippedOfCarriageReturns))
    }

    "removes blank leading and trailing lines" in {
      val spaces = " " * 10

      blockStringValue(
        s"""
           |$spaces
           |    Hello,
           |      World!
           |
           |    Yours,
           |      GraphQL.
           |$spaces
           |""".stripMargin
      ) should equal(
        """Hello,
          |  World!
          |
          |Yours,
          |  GraphQL.""".stripMargin
      )(after.being(strippedOfCarriageReturns))
    }

    "retains indentation from first line" in {
      blockStringValue(
        """    Hello,
           |      World!
           |
           |    Yours,
           |      GraphQL.
           |""".stripMargin
      ) should equal(
        """    Hello,
          |  World!
          |
          |Yours,
          |  GraphQL.""".stripMargin
      )(after.being(strippedOfCarriageReturns))
    }

    "does not alter trailing spaces" in {
      val spaces = " " * 10

      blockStringValue(
        s"""
           |    Hello,$spaces
           |      World!$spaces
           |    $spaces
           |    Yours,$spaces
           |      GraphQL.$spaces
           |""".stripMargin
      ) should equal(
        s"""Hello,$spaces
           |  World!$spaces
           |$spaces
           |Yours,$spaces
           |  GraphQL.$spaces""".stripMargin
      )(after.being(strippedOfCarriageReturns))
    }

    "handles empty strings" in {
      val spaces = " " * 10

      blockStringValue(
        s"""
           |$spaces
           |
           |$spaces
           |""".stripMargin
      ) should equal(
        "".stripMargin
      )(after.being(strippedOfCarriageReturns))

      blockStringValue("") should equal("")
    }
  }
}
