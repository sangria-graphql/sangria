package sangria.parser

import org.parboiled2.{ParserInput, Position}
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._

import scala.util.{Failure, Success}

class QueryParserSpec extends WordSpec with Matchers {

  "QueryParser" should {
    "parse complex query" in {
      val query =
      """
      query FetchLukeAndLeiaAliased($someVar: Int = 1.23,$anotherVar: Int = 123)@include(if: true) @include(if: false){
        luke: human(id: "1000")@include(if: true){
          friends(sort: NAME)
        }
        leia: human(id: "10103\n \u00F6 ö") {
          name
        }

        ... on User {
          birth{day}
        }

        ...Foo
      }

      fragment Foo on User @foo(bar: 1){
        baz
      }
      """

      val expectedAst = 
        Document(List(
          OperationDefinition(
            OperationType.Query,
            Some("FetchLukeAndLeiaAliased"),
            List(
              VariableDefinition(
                "someVar", 
                Type("Int", false, false, Some(Position(47, 2, 47))),
                Some(FloatValue(1.23, Some(Position(53, 2, 53)))),
                Some(Position(37, 2, 37))),
              VariableDefinition(
                "anotherVar",
                Type("Int", false, false, Some(Position(71, 2, 71))),
                Some(IntValue(123, Some(Position(77, 2, 77)))),
                Some(Position(58, 2, 58)))),
            List(
              Directive(
                "include",
                List(Argument("if", BooleanValue(true, Some(Position(94, 2, 94))), Some(Position(90, 2, 90)))),
                Some(Position(81, 2, 81))),
              Directive(
                "include",
                List(Argument("if", BooleanValue(false, Some(Position(113, 2, 113))), Some(Position(109, 2, 109)))),
                Some(Position(100, 2, 100)))),
            List(
              Field(
                Some("luke"), 
                "human", 
                List(Argument("id", StringValue("1000", Some(Position(145, 3, 25))), Some(Position(141, 3, 21)))),
                List(Directive("include", List(Argument("if", BooleanValue(true, Some(Position(165, 3, 45))), Some(Position(161, 3, 41)))), Some(Position(152, 3, 32)))),
                List(
                  Field(None, "friends", List(Argument("sort", EnumValue("NAME", Some(Position(196, 4, 25))), Some(Position(190, 4, 19)))), Nil, Nil, Some(Position(182, 4, 11)))),
                Some(Position(129, 3, 9))),
              Field(
                Some("leia"),
                "human",
                List(Argument("id",StringValue("10103\n ö ö", Some(Position(236, 6, 25))),Some(Position(232, 6, 21)))),
                Nil,
                List(
                  Field(None, "name", Nil, Nil, Nil, Some(Position(263, 7, 11)))),
                Some(Position(220, 6, 9))),
              InlineFragment(
                "User", 
                Nil,
                List(Field(None, "birth", Nil, Nil, List(Field(None, "day", Nil, Nil, Nil, Some(Position(317, 11, 17)))), Some(Position(311, 11, 11)))),
                Some(Position(287, 10, 9))),
              FragmentSpread("Foo", Nil, Some(Position(341, 14, 9)))),
            Some(Position(7, 2, 7))),
          FragmentDefinition(
            "Foo",
            "User",
            List(Directive("foo", List(Argument("bar", IntValue(1, Some(Position(394, 17, 38))), Some(Position(389, 17, 33)))), Some(Position(384, 17, 28)))),
            List(Field(None, "baz", Nil, Nil, Nil, Some(Position(406, 18, 9)))), Some(Position(363, 17, 7)))), Some(Position(7, 2, 7)))

      QueryParser.parse(query) should be (Success(expectedAst))
    }

    "provide useful error messages" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        """
          { ...MissingOn }
          fragment MissingOn Type
        """)

      error.formattedError should be (
        """Invalid input 'T', expected On (line 3, column 30):
          |          fragment MissingOn Type
          |                             ^""".stripMargin
      )
    }
  }

}
