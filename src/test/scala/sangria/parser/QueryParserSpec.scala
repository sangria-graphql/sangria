package sangria.parser

import language.postfixOps

import org.parboiled2.{ParserInput, Position}
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._
import sangria.util.{StringMatchers, FileUtil}

import scala.reflect.ClassTag
import scala.util.{Failure, Success}

class QueryParserSpec extends WordSpec with Matchers with StringMatchers {

  "QueryParser" should {
    "parse complex query" in {
      val query = FileUtil loadQuery "complex-query.graphql"

      val expectedAst =
        Document(
          List(
            OperationDefinition(
              OperationType.Query, 
              Some("FetchLukeAndLeiaAliased"),
              List(
                VariableDefinition(
                  "someVar",
                  NamedType("Int", Some(Position(53, 2, 41))),
                  Some(BigDecimalValue(BigDecimal("1.23"), Some(Position(59, 2, 47)))), Some(Position(43, 2, 31))),
                VariableDefinition(
                  "anotherVar",
                  NamedType("Int", Some(Position(77, 2, 65))),
                  Some(BigIntValue(BigInt(123), Some(Position(83, 2, 71)))), Some(Position(64, 2, 52)))),
              List(
                Directive(
                  "include",
                  List(Argument("if", BooleanValue(true, Some(Position(100, 2, 88))), Some(Position(96, 2, 84)))),
                  Some(Position(87, 2, 75))),
                Directive(
                  "include",
                  List(Argument("if", BooleanValue(false, Some(Position(119, 2, 107))), Some(Position(115, 2, 103)))),
                  Some(Position(106, 2, 94)))),
              List(
                Field(
                  Some("luke"),
                  "human",
                  List(Argument("id", StringValue("1000", Some(Position(145, 3, 19))), Some(Position(141, 3, 15)))),
                  List(Directive("include", List(Argument("if", BooleanValue(true, Some(Position(165, 3, 39))), Some(Position(161, 3, 35)))), Some(Position(152, 3, 26)))),
                  List(Field(None, "friends", List(Argument("sort", EnumValue("NAME", Some(Position(190, 4, 19))), Some(Position(184, 4, 13)))), Nil, Nil, Some(Position(176, 4, 5)))),
                  Some(Position(129, 3, 3))),
                Field(
                  Some("leia"), "human",
                  List(Argument("id", StringValue("10103\n ö ö", Some(Position(223, 6, 24))), Some(Position(214, 6, 15)))),
                  Nil,
                  List(Field(None, "name", Nil, Nil, Nil, Some(Position(249, 7, 5)))),
                  Some(Position(202, 6, 3))),
                InlineFragment(
                  Some(NamedType("User", Some(Position(280, 10, 10)))),
                  Nil,
                  List(Field(None, "birth", Nil, Nil, List(Field(None, "day", Nil, Nil, Nil, Some(Position(297, 11, 11)))), Some(Position(291, 11, 5)))),
                  Some(Position(273, 10, 3))),
                FragmentSpread("Foo", Nil, Some(Position(309, 14, 3)))), Some(Position(13, 2, 1))),
            FragmentDefinition(
              "Foo",
              NamedType("User", Some(Position(335, 17, 17))),
              List(
                Directive(
                  "foo",
                  List(Argument("bar", BigIntValue(BigInt(1), Some(Position(350, 17, 32))), Some(Position(345, 17, 27)))),
                  Some(Position(340, 17, 22)))),
              List(Field(None, "baz", Nil, Nil, Nil, Some(Position(356, 18, 3)))),
              Some(Position(319, 17, 1)))),
          Some(Position(13, 2, 1)))

      QueryParser.parse(query) map (_.copy(sourceMapper = None)) should be (Success(expectedAst))
    }

    "parse kitchen sink" in {
      val query = FileUtil loadQuery "kitchen-sink.graphql"

      val expectedAst =
        Document(
          List(
            OperationDefinition(OperationType.Query, Some("queryName"),
              List(
                VariableDefinition("foo", NamedType("ComplexType", Some(Position(310, 8, 23))), None, Some(Position(304, 8, 17))),
                VariableDefinition("site", NamedType("Site", Some(Position(330, 8, 43))),
                  Some(EnumValue("MOBILE", Some(Position(337, 8, 50)))),
                  Some(Position(323, 8, 36)))),
              Nil,
              List(
                Field(Some("whoever123is"), "node",
                  List(
                    Argument("id",
                      ListValue(List(BigIntValue(123, Some(Position(373, 9, 27))), BigIntValue(456, Some(Position(378, 9, 32)))), Some(Position(372, 9, 26))),
                      Some(Position(368, 9, 22)))),
                  Nil,
                  List(
                    Field(None, "id", Nil, Nil, Nil, Some(Position(390, 10, 5))),
                    InlineFragment(
                      Some(NamedType("User", Some(Position(406, 11, 12)))),
                      List(Directive("defer", Nil, Some(Position(411, 11, 17)))),
                      List(Field(None, "field2", Nil, Nil, List(
                        Field(None, "id", Nil, Nil, Nil, Some(Position(443, 13, 9))),
                        Field(Some("alias"), "field1",
                          List(
                            Argument("first", BigIntValue(10, Some(Position(476, 14, 29))), Some(Position(470, 14, 23))),
                            Argument("after", VariableValue("foo", Some(Position(486, 14, 39))), Some(Position(480, 14, 33)))),
                          List(
                            Directive("include", List(
                              Argument("if", VariableValue("foo", Some(Position(506, 14, 59))), Some(Position(502, 14, 55)))),
                              Some(Position(493, 14, 46)))),
                          List(
                            Field(None, "id", Nil, Nil, Nil, Some(Position(524, 15, 11))),
                            FragmentSpread("frag", Nil, Some(Position(538, 16, 11)))),
                          Some(Position(456, 14, 9)))),
                        Some(Position(426, 12, 7)))),
                      Some(Position(399, 11, 5)))),
                  Some(Position(349, 9, 3)))),
              Some(Position(288, 8, 1))),
            OperationDefinition(OperationType.Mutation, Some("likeStory"), Nil, Nil,
              List(
                Field(None, "like",
                  List(Argument("story", BigIntValue(123, Some(Position(612, 24, 15))), Some(Position(605, 24, 8)))),
                  List(Directive("defer", Nil, Some(Position(617, 24, 20)))),
                  List(
                    Field(None, "story", Nil, Nil, List(Field(None, "id", Nil, Nil, Nil, Some(Position(644, 26, 7)))), Some(Position(630, 25, 5)))),
                  Some(Position(600, 24, 3)))),
              Some(Position(577, 23, 1))),
            OperationDefinition(OperationType.Subscription, Some("StoryLikeSubscription"),
              List(VariableDefinition("input", NamedType("StoryLikeSubscribeInput", Some(Position(703, 31, 44))), None, Some(Position(695, 31, 36)))),
              Nil,
              List(
                Field(None, "storyLikeSubscribe",
                  List(Argument("input", VariableValue("input", Some(Position(758, 32, 29))), Some(Position(751, 32, 22)))),
                  Nil,
                  List(
                    Field(None, "story", Nil, Nil,
                      List(
                        Field(None, "likers", Nil, Nil,
                          List(Field(None, "count", Nil, Nil, Nil, Some(Position(803, 35, 9)))),
                          Some(Position(786, 34, 7))),
                        Field(None, "likeSentence", Nil, Nil,
                          List(Field(None, "text", Nil, Nil, Nil, Some(Position(846, 38, 9)))),
                          Some(Position(823, 37, 7)))),
                      Some(Position(772, 33, 5)))),
                  Some(Position(732, 32, 3)))),
              Some(Position(660, 31, 1))),
            FragmentDefinition("frag", NamedType("Friend", Some(Position(889, 44, 18))), Nil,
              List(
                Field(None, "foo",
                  List(
                    Argument("size", VariableValue("size", Some(Position(910, 45, 13))), Some(Position(904, 45, 7))),
                    Argument("bar", VariableValue("b", Some(Position(922, 45, 25))), Some(Position(917, 45, 20))),
                    Argument("obj", ObjectValue(
                      List(ObjectField("key", StringValue("value", Some(Position(937, 45, 40))), Some(Position(932, 45, 35)))), Some(Position(931, 45, 34))),
                      Some(Position(926, 45, 29)))),
                  Nil,
                  Nil,
                  Some(Position(900, 45, 3)))),
              Some(Position(872, 44, 1))),
            OperationDefinition(OperationType.Query, None, Nil, Nil,
              List(
                Field(None, "unnamed",
                  List(
                    Argument("truthy", BooleanValue(true, Some(Position(970, 49, 19))), Some(Position(962, 49, 11))),
                    Argument("falsey", BooleanValue(false, Some(Position(984, 49, 33))), Some(Position(976, 49, 25)))),
                  Nil,
                  Nil,
                  Some(Position(954, 49, 3))),
                Field(None, "query", Nil, Nil, Nil, Some(Position(994, 50, 3))),
                InlineFragment(None,
                  List(
                    Directive("skip",
                      List(Argument("unless", VariableValue("foo", Some(Position(1021, 52, 21))), Some(Position(1013, 52, 13)))),
                      Some(Position(1007, 52, 7)))),
                  List(
                    Field(None, "id", Nil, Nil, Nil, Some(Position(1033, 53, 5)))),
                  Some(Position(1003, 52, 3))),
                InlineFragment(None, Nil,
                  List(Field(None, "id", Nil, Nil, Nil, Some(Position(1052, 56, 5)))),
                  Some(Position(1042, 55, 3)))),
              Some(Position(950, 48, 1)))),
          Some(Position(288, 8, 1)), None)
      
      QueryParser.parse(query) map (_.copy(sourceMapper = None)) should be (Success(expectedAst))
    }

    "parse anonymous query" in {
      val query =
        """
          query {
            foo bar,
            baz
          }
        """

      val expectedAst =
        Document(List(
          OperationDefinition(
            OperationType.Query,
            None,
            Nil,
            Nil,
            List(
              Field(None, "foo", Nil, Nil, Nil, Some(Position(31, 3, 13))),
              Field(None, "bar", Nil, Nil, Nil, Some(Position(35, 3, 17))),
              Field(None, "baz", Nil, Nil, Nil, Some(Position(52, 4, 13)))),
            Some(Position(11, 2, 11)))),
          Some(Position(11, 2, 11)),
          None)

      QueryParser.parse(stripCarriageReturns(query)) map (_.copy(sourceMapper = None)) should be (Success(expectedAst))
    }

    "parse inline fragments without type condition" in {
      val query =
        """
          query {
            ... {
              foo bar
            }

            ... @include(if: true) {
              baz
            }
          }
        """

      val expectedAst =
        Document(List(
          OperationDefinition(OperationType.Query, None, Nil, Nil, List(
            InlineFragment(None, Nil, List(
              Field(None, "foo", Nil, Nil, Nil, Some(Position(51, 4, 15))),
              Field(None, "bar", Nil, Nil, Nil, Some(Position(55, 4, 19)))),
              Some(Position(31, 3, 13))),
            InlineFragment(None,
              List(Directive("include", List(
                Argument("if", BooleanValue(true, Some(Position(103, 7, 30))),
                  Some(Position(99, 7, 26)))),
                Some(Position(90, 7, 17)))),
              List(Field(None, "baz", Nil, Nil, Nil, Some(Position(125, 8, 15)))),
              Some(Position(86, 7, 13)))),
            Some(Position(11, 2, 11)))),
          Some(Position(11, 2, 11)), None)

      QueryParser.parse(stripCarriageReturns(query)) map (_.copy(sourceMapper = None)) should be (Success(expectedAst))
    }

    "parse anonymous mutation" in {
      val query =
        """
          mutation {
            foo bar,
            baz
          }
        """

      val expectedAst =
        Document(List(
          OperationDefinition(
            OperationType.Mutation,
            None,
            Nil,
            Nil,
            List(
              Field(None, "foo", Nil, Nil, Nil, Some(Position(34, 3, 13))),
              Field(None, "bar", Nil, Nil, Nil, Some(Position(38, 3, 17))),
              Field(None, "baz", Nil, Nil, Nil, Some(Position(55, 4, 13)))),
            Some(Position(11, 2, 11)))),
          Some(Position(11, 2, 11)),
          None)

      QueryParser.parse(stripCarriageReturns(query)) map (_.copy(sourceMapper = None)) should be (Success(expectedAst))
    }

    "provide useful error message (fragment `on`)" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        """
          { ...MissingOn }
          fragment MissingOn Type
        """)

      error.formattedError should equal (
        """Invalid input 'T', expected TypeCondition (line 3, column 30):
          |          fragment MissingOn Type
          |                             ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "provide useful error message (braces)" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "{ field: {} }")

      error.formattedError should equal (
        """Invalid input '{', expected Name (line 1, column 10):
          |{ field: {} }
          |         ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "provide useful error message (operation def)" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "notanoperation Foo { field }")

      error.formattedError should equal (
        """Invalid input 'n', expected OperationDefinition or FragmentDefinition (line 1, column 1):
          |notanoperation Foo { field }
          |^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "provide useful error message (ellipsis)" in {
      val Failure(error: SyntaxError) = QueryParser.parse("...")

      error.formattedError should equal (
        """Invalid input '.', expected OperationDefinition or FragmentDefinition (line 1, column 1):
          |...
          |^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "parses constant default values" in {
      QueryParser.parse("{ field(complex: { a: { b: [ $var ] } }) }").isSuccess should be (true)
    }

    "parses variable inline values" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = { a: { b: [ $var ] } }) { field }")

      error.getMessage should equal (
        """Syntax error while parsing GraphQL query. Invalid input '$', expected ValueConst or ws (line 1, column 37):
          |query Foo($x: Complex = { a: { b: [ $var ] } }) { field }
          |                                    ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "produce parse error for `1.`" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = 1.) { field }")

      error.formattedError should equal (
        """Invalid input ')', expected '0' or NonZeroDigit (line 1, column 27):
          |query Foo($x: Complex = 1.) { field }
          |                          ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "produce parse error for `.123`" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = .123) { field }")

      error.formattedError should equal (
        """Invalid input '.', expected StringValue, BooleanValue, ObjectValueConst, NullValue, ListValueConst, EnumValue or NumberValue (line 1, column 25):
          |query Foo($x: Complex = .123) { field }
          |                        ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "produce parse error for `1.0e`" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = 1.0e) { field }")

      error.formattedError should equal (
        """Invalid input ')', expected Sign or Digit (line 1, column 29):
          |query Foo($x: Complex = 1.0e) { field }
          |                            ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "produce parse error for `1.A`" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = 1.A) { field }")

      error.formattedError should equal (
        """Invalid input 'A', expected '0' or NonZeroDigit (line 1, column 27):
          |query Foo($x: Complex = 1.A) { field }
          |                          ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "produce parse error for `+1`" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = +1) { field }")

      error.formattedError should equal (
        """Invalid input '+', expected StringValue, BooleanValue, ObjectValueConst, NullValue, ListValueConst, EnumValue or NumberValue (line 1, column 25):
          |query Foo($x: Complex = +1) { field }
          |                        ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "produce parse error for `1.0eA`" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = 1.0eA) { field }")

      error.formattedError should equal (
        """Invalid input 'A', expected Sign or Digit (line 1, column 29):
          |query Foo($x: Complex = 1.0eA) { field }
          |                            ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "disallows uncommon control characters" in {
      QueryParser.parse("{ field\u0007 }").isSuccess should be (false)
      QueryParser.parse("{ field } \u0007").isSuccess should be (false)
    }

    "accepts BOM header" in {
      QueryParser.parse("\uFEFF{ field }").isSuccess should be (true)
    }

    "accepts new lines header" in {
      QueryParser.parse("{ field \n another }").isSuccess should be (true)
      QueryParser.parse("{ field \r\n another }").isSuccess should be (true)
    }

    "accepts escape sequences" in {
      QueryParser.parse("{ field(id: \"\\u000A\") }").isSuccess should be (true)
      QueryParser.parse("{ field(id: \"\\uXXXX\") }").isSuccess should be (false)
      QueryParser.parse("{ field(id: \"\\x\") }").isSuccess should be (false)
    }

    "allow `null` to be the prefix of an enum value" in {
      QueryParser.parse("query Foo($x: Complex = null111) { field }").isSuccess should be (true)
      QueryParser.parse("query Foo($x: Complex = null_foo) { field }").isSuccess should be (true)
      QueryParser.parse("query Foo($x: Complex = nullFoo) { field }").isSuccess should be (true)
    }

    def findAst[T <: AstNode : ClassTag](ast: AstNode): Option[T] =
      ast match {
        case node if implicitly[ClassTag[T]].runtimeClass.isAssignableFrom(node.getClass) ⇒ Some(node.asInstanceOf[T])
        case Document(defs, _, _) ⇒ defs map findAst[T] find (_.isDefined) flatten
        case OperationDefinition(_, _, vars, _, _, _) ⇒ vars map findAst[T] find (_.isDefined) flatten
        case VariableDefinition(_, _, default, _) ⇒ default flatMap findAst[T]
        case _ ⇒ None
      }

    "parse int values" in {
      val expectedTable = List(
        "4" → BigInt("4"),
        "-4" → BigInt("-4"),
        "9" → BigInt("9"),
        "0" → BigInt("0"),
        "784236564875237645762347623147574756321" → BigInt("784236564875237645762347623147574756321")
      )

      expectedTable foreach { expected ⇒
        findAst[BigIntValue](QueryParser.parse(s"query Foo($$x: Complex = ${expected._1}) { field }").get) should be (
          Some(BigIntValue(expected._2, Some(Position(24, 1, 25)))))
      }
    }

    "parse float values" in {
      val expectedTable = List(
        "4.123" → BigDecimal("4.123"),
        "-4.123" → BigDecimal("-4.123"),
        "0.123" → BigDecimal("0.123"),
        "123E4" → BigDecimal("123E4"),
        "123e-4" → BigDecimal("123e-4"),
        "-1.123e4" → BigDecimal("-1.123e4"),
        "-1.123E4" → BigDecimal("-1.123E4"),
        "-1.123e+4" → BigDecimal("-1.123e+4"),
        "-1.123e4567" → BigDecimal("-1.123e4567")
      )

      expectedTable foreach { expected ⇒
        withClue(s"Parsing ${expected._1}.") {
          findAst[BigDecimalValue](QueryParser.parse(s"query Foo($$x: Complex = ${expected._1}) { field }").get) should be(
            Some(BigDecimalValue(expected._2, Some(Position(24, 1, 25)))))
        }
      }
    }

    "parse input values independently" in {
      val expectedTable = List(
        "null" → NullValue(Some(Position(0, 1, 1))),
        "1.234" → BigDecimalValue(BigDecimal("1.234"), Some(Position(0, 1, 1))),
        "HELLO_WORLD" → EnumValue("HELLO_WORLD", Some(Position(0, 1, 1))),
        "[1, 2 \"test\"]" → ListValue(
          List(
            BigIntValue(1, Some(Position(1, 1, 2))),
            BigIntValue(2, Some(Position(4, 1, 5))),
            StringValue("test", Some(Position(6, 1, 7)))),
          Some(Position(0, 1, 1))),
        "{a: 1, b: \"foo\" c: {nest: true, oops: null, e: FOO_BAR}}" →
          ObjectValue(
            List(
              ObjectField("a", BigIntValue(1, Some(Position(4, 1, 5))), Some(Position(1, 1, 2))),
              ObjectField("b", StringValue("foo", Some(Position(10, 1, 11))), Some(Position(7, 1, 8))),
              ObjectField("c",
                ObjectValue(
                  List(
                    ObjectField("nest", BooleanValue(true, Some(Position(26, 1, 27))), Some(Position(20, 1, 21))),
                    ObjectField("oops", NullValue(Some(Position(38, 1, 39))), Some(Position(32, 1, 33))),
                    ObjectField("e", EnumValue("FOO_BAR", Some(Position(47, 1, 48))), Some(Position(44, 1, 45)))),
                  Some(Position(19, 1, 20))),
                Some(Position(16, 1, 17)))),
            Some(Position(0, 1, 1))),
        """
         {
           a: 1

           # This is a test comment!
           b: "foo"
         }
        """ →
          ObjectValue(
            List(
              ObjectField("a", BigIntValue(1, Some(Position(26, 3, 15))), Some(Position(23, 3, 12))),
              ObjectField("b", StringValue("foo", Some(Position(80, 6, 15))), Some(Position(77, 6, 12)))),
            Some(Position(10, 2, 10)))

      )

      expectedTable foreach { expected ⇒
        withClue(s"Parsing ${expected._1}.") {
          QueryParser.parseInput(stripCarriageReturns(expected._1)) should equal (Success(expected._2))
        }
      }
    }
  }

  "Ast" should {
    "be equal for the same queries" in {
      val query =
        """
          {
            id
            name
            friends {
              name
            }
          }
        """

      (QueryParser.parse(query) == QueryParser.parse(query)) should be (true)
    }

    "not be equal for the same queries with different AST node positions" in {
      val query1 =
        """
          {
            id
            name
            friends {
              name
            }
          }
        """

      val query2 =
        """
          {
            id
            name
            friends {name}
          }
        """

      (QueryParser.parse(query1) == QueryParser.parse(query2)) should be (false)
    }
  }
}
