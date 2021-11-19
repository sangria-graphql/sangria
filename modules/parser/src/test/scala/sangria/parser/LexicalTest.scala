package sangria.parser

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.util.StringMatchers

class LexicalTest extends AnyWordSpec with Matchers with StringMatchers {
  "blockStringValue" should {
    "remove uniform indentation from a string" in {
      Lexical.blockStringValue(
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

    "remove empty leading and trailing lines" in {
      Lexical.blockStringValue(
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

    "remove blank leading and trailing lines" in {
      val spaces = " " * 10

      Lexical.blockStringValue(
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

    "retain indentation from first line" in {
      Lexical.blockStringValue(
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

    "not alter trailing spaces" in {
      val spaces = " " * 10

      Lexical.blockStringValue(
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

    "handle empty strings" in {
      val spaces = " " * 10

      Lexical.blockStringValue(
        s"""
           |$spaces
           |
           |$spaces
           |""".stripMargin
      ) should equal(
        "".stripMargin
      )(after.being(strippedOfCarriageReturns))

      Lexical.blockStringValue("") should equal("")
    }
  }
}
