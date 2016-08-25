package sangria.parser

import language.postfixOps
import org.parboiled2.Position
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._
import sangria.util.{DebugUtil, FileUtil, StringMatchers}

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
                  Some(BigDecimalValue(1.23, Nil, Some(Position(59, 2, 47)))),
                  Nil,
                  Some(Position(43, 2, 31))
                ),
                VariableDefinition(
                  "anotherVar",
                  NamedType("Int", Some(Position(77, 2, 65))),
                  Some(BigIntValue(123, Nil, Some(Position(83, 2, 71)))),
                  Nil,
                  Some(Position(64, 2, 52))
                )),
              List(
                Directive(
                  "include",
                  List(
                    Argument(
                      "if",
                      BooleanValue(true, Nil, Some(Position(100, 2, 88))),
                      Nil,
                      Some(Position(96, 2, 84))
                    )),
                  Nil,
                  Some(Position(87, 2, 75))
                ),
                Directive(
                  "include",
                  List(
                    Argument(
                      "if",
                      BooleanValue(false, Nil, Some(Position(119, 2, 107))),
                      Nil,
                      Some(Position(115, 2, 103))
                    )),
                  Nil,
                  Some(Position(106, 2, 94))
                )),
              List(
                Field(
                  Some("luke"),
                  "human",
                  List(
                    Argument(
                      "id",
                      StringValue("1000", Nil, Some(Position(145, 3, 19))),
                      Nil,
                      Some(Position(141, 3, 15))
                    )),
                  List(
                    Directive(
                      "include",
                      List(
                        Argument(
                          "if",
                          BooleanValue(true, Nil, Some(Position(165, 3, 39))),
                          Nil,
                          Some(Position(161, 3, 35))
                        )),
                      Nil,
                      Some(Position(152, 3, 26))
                    )),
                  List(
                    Field(
                      None,
                      "friends",
                      List(
                        Argument(
                          "sort",
                          EnumValue("NAME", Nil, Some(Position(190, 4, 19))),
                          Nil,
                          Some(Position(184, 4, 13))
                        )),
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Some(Position(176, 4, 5))
                    )),
                  Nil,
                  Nil,
                  Some(Position(129, 3, 3))
                ),
                Field(
                  Some("leia"),
                  "human",
                  List(
                    Argument(
                      "id",
                      StringValue("10103\n \u00F6 \u00F6", Nil, Some(Position(223, 6, 24))),
                      Nil,
                      Some(Position(214, 6, 15))
                    )),
                  Nil,
                  List(
                    Field(
                      None,
                      "name",
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Some(Position(249, 7, 5))
                    )),
                  Nil,
                  List(
                    Comment(" some name", Some(Position(254, 7, 10)))),
                  Some(Position(202, 6, 3))
                ),
                InlineFragment(
                  Some(NamedType("User", Some(Position(280, 10, 10)))),
                  Nil,
                  List(
                    Field(
                      None,
                      "birth",
                      Nil,
                      Nil,
                      List(
                        Field(
                          None,
                          "day",
                          Nil,
                          Nil,
                          Nil,
                          Nil,
                          Nil,
                          Some(Position(297, 11, 11))
                        )),
                      Nil,
                      Nil,
                      Some(Position(291, 11, 5))
                    )),
                  Nil,
                  Nil,
                  Some(Position(273, 10, 3))
                ),
                FragmentSpread("Foo", Nil, Nil, Some(Position(309, 14, 3)))),
              List(
                Comment(" test query", Some(Position(0, 1, 1)))),
              Nil,
              Some(Position(13, 2, 1))
            ),
            FragmentDefinition(
              "Foo",
              NamedType("User", Some(Position(335, 17, 17))),
              List(
                Directive(
                  "foo",
                  List(
                    Argument(
                      "bar",
                      BigIntValue(1, Nil, Some(Position(350, 17, 32))),
                      Nil,
                      Some(Position(345, 17, 27))
                    )),
                  Nil,
                  Some(Position(340, 17, 22))
                )),
              List(
                Field(
                  None,
                  "baz",
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Some(Position(356, 18, 3))
                )),
              Nil,
              List(
                Comment(" field in fragment!", Some(Position(360, 18, 7)))),
              Some(Position(319, 17, 1))
            )),
          Nil,
          Some(Position(0, 1, 1)),
          None
        )

      QueryParser.parse(query) map (_.withoutSourceMapper) should be (Success(expectedAst))
    }

    "parse kitchen sink" in {
      val query = FileUtil loadQuery "kitchen-sink.graphql"

      val expectedAst =
        Document(
          List(
            OperationDefinition(
              OperationType.Query,
              Some("queryName"),
              List(
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", Some(Position(310, 8, 23))),
                  None,
                  Nil,
                  Some(Position(304, 8, 17))
                ),
                VariableDefinition(
                  "site",
                  NamedType("Site", Some(Position(330, 8, 43))),
                  Some(EnumValue("MOBILE", Nil, Some(Position(337, 8, 50)))),
                  Nil,
                  Some(Position(323, 8, 36))
                )),
              Nil,
              List(
                Field(
                  Some("whoever123is"),
                  "node",
                  List(
                    Argument(
                      "id",
                      ListValue(
                        List(
                          BigIntValue(123, Nil, Some(Position(373, 9, 27))),
                          BigIntValue(456, Nil, Some(Position(378, 9, 32)))),
                        Nil,
                        Some(Position(372, 9, 26))
                      ),
                      Nil,
                      Some(Position(368, 9, 22))
                    )),
                  Nil,
                  List(
                    Field(
                      None,
                      "id",
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Some(Position(390, 10, 5))
                    ),
                    InlineFragment(
                      Some(NamedType("User", Some(Position(406, 11, 12)))),
                      List(
                        Directive(
                          "defer",
                          Nil,
                          Nil,
                          Some(Position(411, 11, 17))
                        )),
                      List(
                        Field(
                          None,
                          "field2",
                          Nil,
                          Nil,
                          List(
                            Field(
                              None,
                              "id",
                              Nil,
                              Nil,
                              Nil,
                              Nil,
                              Nil,
                              Some(Position(443, 13, 9))
                            ),
                            Field(
                              Some("alias"),
                              "field1",
                              List(
                                Argument(
                                  "first",
                                  BigIntValue(10, Nil, Some(Position(476, 14, 29))),
                                  Nil,
                                  Some(Position(470, 14, 23))
                                ),
                                Argument(
                                  "after",
                                  VariableValue("foo", Nil, Some(Position(486, 14, 39))),
                                  Nil,
                                  Some(Position(480, 14, 33))
                                )),
                              List(
                                Directive(
                                  "include",
                                  List(
                                    Argument(
                                      "if",
                                      VariableValue("foo", Nil, Some(Position(506, 14, 59))),
                                      Nil,
                                      Some(Position(502, 14, 55))
                                    )),
                                  Nil,
                                  Some(Position(493, 14, 46))
                                )),
                              List(
                                Field(
                                  None,
                                  "id",
                                  Nil,
                                  Nil,
                                  Nil,
                                  Nil,
                                  Nil,
                                  Some(Position(524, 15, 11))
                                ),
                                FragmentSpread("frag", Nil, Nil, Some(Position(538, 16, 11)))),
                              Nil,
                              Nil,
                              Some(Position(456, 14, 9))
                            )),
                          Nil,
                          Nil,
                          Some(Position(426, 12, 7))
                        )),
                      Nil,
                      Nil,
                      Some(Position(399, 11, 5))
                    )),
                  Nil,
                  Nil,
                  Some(Position(349, 9, 3))
                )),
              List(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(Position(0, 1, 1))),
                Comment(" All rights reserved.", Some(Position(37, 2, 1))),
                Comment("", Some(Position(60, 3, 1))),
                Comment(" This source code is licensed under the BSD-style license found in the", Some(Position(62, 4, 1))),
                Comment(" LICENSE file in the root directory of this source tree. An additional grant", Some(Position(134, 5, 1))),
                Comment(" of patent rights can be found in the PATENTS file in the same directory.", Some(Position(212, 6, 1)))),
              Nil,
              Some(Position(288, 8, 1))
            ),
            OperationDefinition(
              OperationType.Mutation,
              Some("likeStory"),
              Nil,
              Nil,
              List(
                Field(
                  None,
                  "like",
                  List(
                    Argument(
                      "story",
                      BigIntValue(123, Nil, Some(Position(612, 24, 15))),
                      Nil,
                      Some(Position(605, 24, 8))
                    )),
                  List(
                    Directive(
                      "defer",
                      Nil,
                      Nil,
                      Some(Position(617, 24, 20))
                    )),
                  List(
                    Field(
                      None,
                      "story",
                      Nil,
                      Nil,
                      List(
                        Field(
                          None,
                          "id",
                          Nil,
                          Nil,
                          Nil,
                          Nil,
                          Nil,
                          Some(Position(644, 26, 7))
                        )),
                      Nil,
                      Nil,
                      Some(Position(630, 25, 5))
                    )),
                  Nil,
                  Nil,
                  Some(Position(600, 24, 3))
                )),
              Nil,
              Nil,
              Some(Position(577, 23, 1))
            ),
            OperationDefinition(
              OperationType.Subscription,
              Some("StoryLikeSubscription"),
              List(
                VariableDefinition(
                  "input",
                  NamedType("StoryLikeSubscribeInput", Some(Position(703, 31, 44))),
                  None,
                  Nil,
                  Some(Position(695, 31, 36))
                )),
              Nil,
              List(
                Field(
                  None,
                  "storyLikeSubscribe",
                  List(
                    Argument(
                      "input",
                      VariableValue("input", Nil, Some(Position(758, 32, 29))),
                      Nil,
                      Some(Position(751, 32, 22))
                    )),
                  Nil,
                  List(
                    Field(
                      None,
                      "story",
                      Nil,
                      Nil,
                      List(
                        Field(
                          None,
                          "likers",
                          Nil,
                          Nil,
                          List(
                            Field(
                              None,
                              "count",
                              Nil,
                              Nil,
                              Nil,
                              Nil,
                              Nil,
                              Some(Position(803, 35, 9))
                            )),
                          Nil,
                          Nil,
                          Some(Position(786, 34, 7))
                        ),
                        Field(
                          None,
                          "likeSentence",
                          Nil,
                          Nil,
                          List(
                            Field(
                              None,
                              "text",
                              Nil,
                              Nil,
                              Nil,
                              Nil,
                              Nil,
                              Some(Position(846, 38, 9))
                            )),
                          Nil,
                          Nil,
                          Some(Position(823, 37, 7))
                        )),
                      Nil,
                      Nil,
                      Some(Position(772, 33, 5))
                    )),
                  Nil,
                  Nil,
                  Some(Position(732, 32, 3))
                )),
              Nil,
              Nil,
              Some(Position(660, 31, 1))
            ),
            FragmentDefinition(
              "frag",
              NamedType("Friend", Some(Position(889, 44, 18))),
              Nil,
              List(
                Field(
                  None,
                  "foo",
                  List(
                    Argument(
                      "size",
                      VariableValue("size", Nil, Some(Position(910, 45, 13))),
                      Nil,
                      Some(Position(904, 45, 7))
                    ),
                    Argument(
                      "bar",
                      VariableValue("b", Nil, Some(Position(922, 45, 25))),
                      Nil,
                      Some(Position(917, 45, 20))
                    ),
                    Argument(
                      "obj",
                      ObjectValue(
                        List(
                          ObjectField(
                            "key",
                            StringValue("value", Nil, Some(Position(937, 45, 40))),
                            Nil,
                            Some(Position(932, 45, 35))
                          )),
                        Nil,
                        Some(Position(931, 45, 34))
                      ),
                      Nil,
                      Some(Position(926, 45, 29))
                    )),
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Some(Position(900, 45, 3))
                )),
              Nil,
              Nil,
              Some(Position(872, 44, 1))
            ),
            OperationDefinition(
              OperationType.Query,
              None,
              Nil,
              Nil,
              List(
                Field(
                  None,
                  "unnamed",
                  List(
                    Argument(
                      "truthy",
                      BooleanValue(true, Nil, Some(Position(970, 49, 19))),
                      Nil,
                      Some(Position(962, 49, 11))
                    ),
                    Argument(
                      "falsey",
                      BooleanValue(false, Nil, Some(Position(984, 49, 33))),
                      Nil,
                      Some(Position(976, 49, 25))
                    )),
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Some(Position(954, 49, 3))
                ),
                Field(
                  None,
                  "query",
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Some(Position(994, 50, 3))
                ),
                InlineFragment(
                  None,
                  List(
                    Directive(
                      "skip",
                      List(
                        Argument(
                          "unless",
                          VariableValue("foo", Nil, Some(Position(1021, 52, 21))),
                          Nil,
                          Some(Position(1013, 52, 13))
                        )),
                      Nil,
                      Some(Position(1007, 52, 7))
                    )),
                  List(
                    Field(
                      None,
                      "id",
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Some(Position(1033, 53, 5))
                    )),
                  Nil,
                  Nil,
                  Some(Position(1003, 52, 3))
                ),
                InlineFragment(
                  None,
                  Nil,
                  List(
                    Field(
                      None,
                      "id",
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Some(Position(1052, 56, 5))
                    )),
                  Nil,
                  Nil,
                  Some(Position(1042, 55, 3))
                )),
              Nil,
              Nil,
              Some(Position(950, 48, 1))
            )),
          Nil,
          Some(Position(0, 1, 1)),
          None
        )

      QueryParser.parse(query) map (_.withoutSourceMapper) should be (Success(expectedAst))
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
              Field(None, "foo", Nil, Nil, Nil, Nil, Nil, Some(Position(31, 3, 13))),
              Field(None, "bar", Nil, Nil, Nil, Nil, Nil, Some(Position(35, 3, 17))),
              Field(None, "baz", Nil, Nil, Nil, Nil, Nil, Some(Position(52, 4, 13)))),
            Nil,
            Nil,
            Some(Position(11, 2, 11)))),
          Nil,
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
              Field(None, "foo", Nil, Nil, Nil, Nil, Nil, Some(Position(51, 4, 15))),
              Field(None, "bar", Nil, Nil, Nil, Nil, Nil, Some(Position(55, 4, 19)))),
              Nil,
              Nil,
              Some(Position(31, 3, 13))),
            InlineFragment(None,
              List(Directive("include", List(
                Argument("if", BooleanValue(true, Nil, Some(Position(103, 7, 30))),
                  Nil, Some(Position(99, 7, 26)))),
                Nil,
                Some(Position(90, 7, 17)))),
              List(Field(None, "baz", Nil, Nil, Nil, Nil, Nil, Some(Position(125, 8, 15)))),
              Nil,
              Nil,
              Some(Position(86, 7, 13)))),
            Nil,
            Nil,
            Some(Position(11, 2, 11)))),
          Nil,
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
              Field(None, "foo", Nil, Nil, Nil, Nil, Nil, Some(Position(34, 3, 13))),
              Field(None, "bar", Nil, Nil, Nil, Nil, Nil, Some(Position(38, 3, 17))),
              Field(None, "baz", Nil, Nil, Nil, Nil, Nil, Some(Position(55, 4, 13)))),
            Nil,
            Nil,
            Some(Position(11, 2, 11)))),
          Nil,
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
        """Invalid input "{ field: {", expected OperationDefinition, FragmentDefinition or TypeSystemDefinition (line 1, column 1):
          |{ field: {} }
          |^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "provide useful error message (operation def)" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "notanoperation Foo { field }")

      error.formattedError should equal (
        """Invalid input 'n', expected OperationDefinition, FragmentDefinition or TypeSystemDefinition (line 1, column 1):
          |notanoperation Foo { field }
          |^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "provide useful error message (ellipsis)" in {
      val Failure(error: SyntaxError) = QueryParser.parse("...")

      error.formattedError should equal (
        """Invalid input '.', expected OperationDefinition, FragmentDefinition or TypeSystemDefinition (line 1, column 1):
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
        """Syntax error while parsing GraphQL query. Invalid input '$', expected StringValue, BooleanValue, ObjectValueConst, NullValue, ListValueConst, EnumValue or NumberValue (line 1, column 37):
          |query Foo($x: Complex = { a: { b: [ $var ] } }) { field }
          |                                    ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "produce parse error for `1.`" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = 1.) { field }")

      error.formattedError should equal (
        """Invalid input "1.)", expected ValueConst or VariableDefinition (line 1, column 25):
          |query Foo($x: Complex = 1.) { field }
          |                        ^""".stripMargin) (after being strippedOfCarriageReturns)
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
        """Invalid input "1.0e)", expected ValueConst or VariableDefinition (line 1, column 25):
          |query Foo($x: Complex = 1.0e) { field }
          |                        ^""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "produce parse error for `1.A`" in {
      val Failure(error: SyntaxError) = QueryParser.parse(
        "query Foo($x: Complex = 1.A) { field }")

      error.formattedError should equal (
        """Invalid input "1.A", expected ValueConst or VariableDefinition (line 1, column 25):
          |query Foo($x: Complex = 1.A) { field }
          |                        ^""".stripMargin) (after being strippedOfCarriageReturns)
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
        """Invalid input "1.0eA", expected ValueConst or VariableDefinition (line 1, column 25):
          |query Foo($x: Complex = 1.0eA) { field }
          |                        ^""".stripMargin) (after being strippedOfCarriageReturns)
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
        case Document(defs, _, _, _) ⇒ defs map findAst[T] find (_.isDefined) flatten
        case OperationDefinition(_, _, vars, _, _, _, _, _) ⇒ vars map findAst[T] find (_.isDefined) flatten
        case VariableDefinition(_, _, default, _, _) ⇒ default flatMap findAst[T]
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
          Some(BigIntValue(expected._2, Nil, Some(Position(24, 1, 25)))))
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
            Some(BigDecimalValue(expected._2, Nil, Some(Position(24, 1, 25)))))
        }
      }
    }

    "parse input values independently" in {
      val expectedTable = List(
        "null" → NullValue(Nil, Some(Position(0, 1, 1))),
        "1.234" → BigDecimalValue(BigDecimal("1.234"), Nil, Some(Position(0, 1, 1))),
        "HELLO_WORLD" → EnumValue("HELLO_WORLD", Nil, Some(Position(0, 1, 1))),
        "[1, 2 \"test\"]" → ListValue(
          List(
            BigIntValue(1, Nil, Some(Position(1, 1, 2))),
            BigIntValue(2, Nil, Some(Position(4, 1, 5))),
            StringValue("test", Nil, Some(Position(6, 1, 7)))),
          Nil,
          Some(Position(0, 1, 1))),
        "{a: 1, b: \"foo\" c: {nest: true, oops: null, e: FOO_BAR}}" →
          ObjectValue(
            List(
              ObjectField("a", BigIntValue(1, Nil, Some(Position(4, 1, 5))), Nil, Some(Position(1, 1, 2))),
              ObjectField("b", StringValue("foo", Nil, Some(Position(10, 1, 11))), Nil, Some(Position(7, 1, 8))),
              ObjectField("c",
                ObjectValue(
                  List(
                    ObjectField("nest", BooleanValue(true, Nil, Some(Position(26, 1, 27))), Nil, Some(Position(20, 1, 21))),
                    ObjectField("oops", NullValue(Nil, Some(Position(38, 1, 39))), Nil, Some(Position(32, 1, 33))),
                    ObjectField("e", EnumValue("FOO_BAR", Nil, Some(Position(47, 1, 48))), Nil, Some(Position(44, 1, 45)))),
                  Nil,
                  Some(Position(19, 1, 20))),
                Nil,
                Some(Position(16, 1, 17)))),
            Nil,
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
              ObjectField("a", BigIntValue(1, Nil, Some(Position(26, 3, 15))), Nil, Some(Position(23, 3, 12))),
              ObjectField("b", StringValue("foo", Nil, Some(Position(80, 6, 15))),
                List(Comment(" This is a test comment!", Some(Position(40, 5, 12)))),
                Some(Position(77, 6, 12)))),
              Nil,
            Some(Position(10, 2, 10)))

      )

      expectedTable foreach { expected ⇒
        withClue(s"Parsing ${expected._1}.") {
          QueryParser.parseInput(stripCarriageReturns(expected._1)) should equal (Success(expected._2))
        }
      }
    }

    "parse and collect comments in AST nodes" in {
      val query = FileUtil loadQuery "too-many-comments.graphql"

      val expected =
        Document(
          List(
            OperationDefinition(
              OperationType.Query,
              Some("queryName"),
              List(
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", Some(Position(434, 23, 1))),
                  None,
                  List(
                    Comment(" comment 5", Some(Position(354, 15, 1))),
                    Comment(" comment 6", Some(Position(366, 16, 1)))),
                  Some(Position(378, 17, 1))
                ),
                VariableDefinition(
                  "site",
                  NamedType("Site", Some(Position(565, 36, 1))),
                  Some(EnumValue("MOBILE", List(Comment(" comment 16.5", Some(Position(602, 40, 1))), Comment(" comment 16.6", Some(Position(617, 41, 1)))), Some(Position(632, 42, 1)))),
                  List(
                    Comment(" comment 11", Some(Position(446, 24, 1))),
                    Comment(" comment 12", Some(Position(459, 25, 1))),
                    Comment(" comment 13", Some(Position(475, 28, 1))),
                    Comment(" comment 14", Some(Position(488, 29, 1)))),
                  Some(Position(501, 30, 1))
                ),
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", Some(Position(703, 48, 7))),
                  Some(ObjectValue(
                    List(
                      ObjectField(
                        "field1",
                        StringValue("val", List(Comment(" comment 18.11", Some(Position(849, 61, 1))), Comment(" comment 18.12", Some(Position(865, 62, 1)))), Some(Position(881, 63, 1))),
                        List(
                          Comment(" comment 18.7", Some(Position(779, 55, 1))),
                          Comment(" comment 18.8", Some(Position(794, 56, 1)))),
                        Some(Position(809, 57, 1))
                      ),
                      ObjectField(
                        "list",
                        ListValue(
                          List(
                            BigIntValue(1, List(Comment(" comment 18.21", Some(Position(1026, 76, 1))), Comment(" comment 18.22", Some(Position(1042, 77, 1)))), Some(Position(1058, 78, 1))),
                            BigIntValue(2, List(Comment(" comment 18.23", Some(Position(1061, 79, 1))), Comment(" comment 18.24", Some(Position(1077, 80, 1)))), Some(Position(1093, 81, 1))),
                            BigIntValue(3, List(Comment(" comment 18.25", Some(Position(1096, 82, 1))), Comment(" comment 18.26", Some(Position(1112, 83, 1)))), Some(Position(1128, 84, 1)))),
                          List(
                            Comment(" comment 18.19", Some(Position(992, 73, 1))),
                            Comment(" comment 18.20", Some(Position(1008, 74, 1)))),
                          Some(Position(1024, 75, 1))
                        ),
                        List(
                          Comment(" comment 18.13", Some(Position(887, 64, 1))),
                          Comment(" comment 18.14", Some(Position(903, 65, 1))),
                          Comment(" comment 18.15", Some(Position(921, 67, 1))),
                          Comment(" comment 18.16", Some(Position(937, 68, 1)))),
                        Some(Position(953, 69, 1))
                      ),
                      ObjectField(
                        "field2",
                        BooleanValue(true, List(Comment(" comment 18.35", Some(Position(1271, 97, 1))), Comment(" comment 18.36", Some(Position(1287, 98, 1)))), Some(Position(1303, 99, 1))),
                        List(
                          Comment(" comment 18.29", Some(Position(1164, 88, 1))),
                          Comment(" comment 18.30", Some(Position(1180, 89, 1))),
                          Comment(" comment 18.31", Some(Position(1198, 91, 1))),
                          Comment(" comment 18.32", Some(Position(1214, 92, 1)))),
                        Some(Position(1230, 93, 1))
                      )),
                    List(
                      Comment(" comment 18.5", Some(Position(747, 52, 1))),
                      Comment(" comment 18.6", Some(Position(762, 53, 1)))),
                    Some(Position(777, 54, 1))
                  )),
                  List(
                    Comment(" comment 17", Some(Position(639, 43, 1))),
                    Comment(" comment 18", Some(Position(652, 44, 1))),
                    Comment(" comment 18.1", Some(Position(667, 46, 1))),
                    Comment(" comment 18.2", Some(Position(682, 47, 1)))),
                  Some(Position(697, 48, 1))
                )),
              Nil,
              List(
                Field(
                  Some("whoever123is"),
                  "node",
                  List(
                    Argument(
                      "id",
                      ListValue(
                        List(
                          BigIntValue(123, List(Comment(" comment 35", Some(Position(1660, 130, 3))), Comment(" comment 36", Some(Position(1675, 131, 3)))), Some(Position(1690, 132, 3))),
                          BigIntValue(456, List(Comment(" comment 37", Some(Position(1696, 133, 3))), Comment(" comment 38", Some(Position(1711, 134, 3)))), Some(Position(1726, 135, 3)))),
                        List(
                          Comment(" comment 33", Some(Position(1626, 127, 3))),
                          Comment(" comment 34", Some(Position(1641, 128, 3)))),
                        Some(Position(1656, 129, 3))
                      ),
                      List(
                        Comment(" comment 29", Some(Position(1557, 121, 3))),
                        Comment(" comment 30", Some(Position(1572, 122, 3)))),
                      Some(Position(1587, 123, 3))
                    )),
                  Nil,
                  List(
                    Field(
                      None,
                      "id",
                      Nil,
                      Nil,
                      Nil,
                      List(
                        Comment(" comment 44", Some(Position(1837, 145, 4))),
                        Comment(" comment 45", Some(Position(1853, 146, 4)))),
                      Nil,
                      Some(Position(1870, 147, 5))
                    ),
                    InlineFragment(
                      Some(NamedType("User", Some(Position(1996, 156, 5)))),
                      List(
                        Directive(
                          "defer",
                          Nil,
                          List(
                            Comment(" comment 52", Some(Position(2005, 157, 5))),
                            Comment(" comment 53", Some(Position(2022, 158, 5)))),
                          Some(Position(2039, 159, 5))
                        )),
                      List(
                        Field(
                          None,
                          "field2",
                          Nil,
                          Nil,
                          List(
                            Field(
                              Some("alias"),
                              "field1",
                              List(
                                Argument(
                                  "first",
                                  BigIntValue(10, List(Comment(" comment 70", Some(Position(2474, 185, 9))), Comment(" comment 71", Some(Position(2495, 186, 9)))), Some(Position(2516, 187, 9))),
                                  List(
                                    Comment(" comment 66", Some(Position(2366, 179, 9))),
                                    Comment(" comment 67", Some(Position(2387, 180, 9)))),
                                  Some(Position(2408, 181, 9))
                                ),
                                Argument(
                                  "after",
                                  VariableValue("foo", List(Comment(" comment 76", Some(Position(2636, 194, 9))), Comment(" comment 77", Some(Position(2657, 195, 9)))), Some(Position(2678, 196, 9))),
                                  List(
                                    Comment(" comment 72", Some(Position(2528, 188, 9))),
                                    Comment(" comment 73", Some(Position(2549, 189, 9)))),
                                  Some(Position(2570, 190, 9))
                                )),
                              List(
                                Directive(
                                  "include",
                                  List(
                                    Argument(
                                      "if",
                                      VariableValue("foo", List(Comment(" comment 88", Some(Position(2961, 212, 10))), Comment(" comment 89", Some(Position(2983, 213, 10)))), Some(Position(3005, 214, 10))),
                                      List(
                                        Comment(" comment 84", Some(Position(2855, 206, 9))),
                                        Comment(" comment 85", Some(Position(2876, 207, 9)))),
                                      Some(Position(2897, 208, 9))
                                    )),
                                  List(
                                    Comment(" comment 80", Some(Position(2744, 200, 9))),
                                    Comment(" comment 81", Some(Position(2765, 201, 9)))),
                                  Some(Position(2786, 202, 9))
                                )),
                              List(
                                Field(
                                  None,
                                  "id",
                                  Nil,
                                  Nil,
                                  Nil,
                                  List(
                                    Comment(" comment 94", Some(Position(3130, 221, 11))),
                                    Comment(" comment 95", Some(Position(3153, 222, 11)))),
                                  Nil,
                                  Some(Position(3176, 223, 11))
                                ),
                                FragmentSpread("frag", Nil, List(Comment(" comment 96", Some(Position(3190, 224, 11))), Comment(" comment 97", Some(Position(3213, 225, 11)))), Some(Position(3237, 227, 11)))),
                              List(
                                Comment(" comment 58", Some(Position(2151, 167, 7))),
                                Comment(" comment 59", Some(Position(2170, 168, 7)))),
                              List(
                                Comment(" comment 100", Some(Position(3312, 231, 11))),
                                Comment(" comment 101", Some(Position(3336, 232, 11)))),
                              Some(Position(2191, 169, 9))
                            )),
                          Nil,
                          List(
                            Comment(" comment 102", Some(Position(3368, 234, 9))),
                            Comment(" comment 103", Some(Position(3390, 235, 9)))),
                          Some(Position(2092, 163, 7))
                        )),
                      List(
                        Comment(" comment 46", Some(Position(1879, 148, 5))),
                        Comment(" comment 47", Some(Position(1896, 149, 5)))),
                      List(
                        Comment(" comment 104", Some(Position(3418, 237, 7))),
                        Comment(" comment 105", Some(Position(3438, 238, 7)))),
                      Some(Position(1913, 150, 5))
                    )),
                  List(
                    Comment(" comment 21", Some(Position(1408, 109, 2))),
                    Comment(" comment 22", Some(Position(1422, 110, 2)))),
                  List(
                    Comment(" comment 106", Some(Position(3462, 240, 5))),
                    Comment(" comment 107", Some(Position(3480, 241, 5)))),
                  Some(Position(1437, 111, 3))
                )),
              List(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(Position(0, 1, 1))),
                Comment(" All rights reserved.", Some(Position(37, 2, 1))),
                Comment("", Some(Position(60, 3, 1))),
                Comment(" This source code is licensed under the BSD-style license found in the", Some(Position(62, 4, 1))),
                Comment(" LICENSE file in the root directory of this source tree. An additional grant", Some(Position(134, 5, 1))),
                Comment(" of patent rights can be found in the PATENTS file in the same directory.", Some(Position(212, 6, 1)))),
              List(
                Comment(" comment 108", Some(Position(3500, 243, 3))),
                Comment(" comment 109", Some(Position(3516, 244, 3)))),
              Some(Position(288, 8, 1))
            ),
            OperationDefinition(
              OperationType.Mutation,
              Some("likeStory"),
              Nil,
              Nil,
              List(
                Field(
                  None,
                  "like",
                  List(
                    Argument(
                      "story",
                      BigIntValue(123, List(Comment(" comment 124", Some(Position(3793, 268, 3))), Comment(" comment 125", Some(Position(3809, 269, 3)))), Some(Position(3825, 270, 3))),
                      List(
                        Comment(" comment 120", Some(Position(3717, 262, 3))),
                        Comment(" comment 121", Some(Position(3733, 263, 3)))),
                      Some(Position(3749, 264, 3))
                    )),
                  List(
                    Directive(
                      "defer",
                      Nil,
                      List(
                        Comment(" comment 128", Some(Position(3867, 274, 3))),
                        Comment(" comment 129", Some(Position(3883, 275, 3)))),
                      Some(Position(3899, 276, 3))
                    )),
                  List(
                    Field(
                      None,
                      "story",
                      Nil,
                      Nil,
                      List(
                        Field(
                          None,
                          "id",
                          Nil,
                          Nil,
                          Nil,
                          List(
                            Comment(" comment 136", Some(Position(4030, 286, 5))),
                            Comment(" comment 137", Some(Position(4048, 287, 5))),
                            Comment(" comment 138", Some(Position(4067, 289, 5))),
                            Comment(" comment 139", Some(Position(4085, 290, 5)))),
                          Nil,
                          Some(Position(4105, 291, 7))
                        )),
                      List(
                        Comment(" comment 132", Some(Position(3944, 280, 3))),
                        Comment(" comment 133", Some(Position(3960, 281, 3)))),
                      List(
                        Comment(" comment 140", Some(Position(4114, 292, 7))),
                        Comment(" comment 141", Some(Position(4134, 293, 7)))),
                      Some(Position(3978, 282, 5))
                    )),
                  List(
                    Comment(" comment 116", Some(Position(3644, 256, 1))),
                    Comment(" comment 117", Some(Position(3658, 257, 1)))),
                  List(
                    Comment(" comment 142", Some(Position(4158, 295, 5))),
                    Comment(" comment 143", Some(Position(4176, 296, 5)))),
                  Some(Position(3674, 258, 3))
                )),
              List(
                Comment(" comment 110", Some(Position(3536, 247, 4))),
                Comment(" comment 111", Some(Position(3553, 248, 4)))),
              List(
                Comment(" comment 144", Some(Position(4196, 298, 3))),
                Comment(" comment 145", Some(Position(4212, 299, 3)))),
              Some(Position(3567, 249, 1))
            ),
            FragmentDefinition(
              "frag",
              NamedType("Friend", Some(Position(4358, 312, 1))),
              Nil,
              List(
                InlineFragment(
                  None,
                  List(
                    Directive(
                      "skip",
                      List(
                        Argument(
                          "unless",
                          VariableValue("foo", List(Comment(" comment 168", Some(Position(4613, 334, 3))), Comment(" comment 169", Some(Position(4629, 335, 3)))), Some(Position(4645, 336, 3))),
                          List(
                            Comment(" comment 164", Some(Position(4536, 328, 3))),
                            Comment(" comment 165", Some(Position(4552, 329, 3)))),
                          Some(Position(4568, 330, 3))
                        )),
                      List(
                        Comment(" comment 160", Some(Position(4460, 322, 3))),
                        Comment(" comment 161", Some(Position(4476, 323, 3)))),
                      Some(Position(4492, 324, 3))
                    )),
                  List(
                    Field(
                      None,
                      "id",
                      Nil,
                      Nil,
                      Nil,
                      List(
                        Comment(" comment 174", Some(Position(4724, 343, 3))),
                        Comment(" comment 175", Some(Position(4740, 344, 3)))),
                      Nil,
                      Some(Position(4758, 345, 5))
                    )),
                  List(
                    Comment(" comment 156", Some(Position(4395, 316, 1))),
                    Comment(" comment 157", Some(Position(4409, 317, 1))),
                    Comment(" comment 158", Some(Position(4424, 319, 1))),
                    Comment(" comment 159", Some(Position(4438, 320, 1)))),
                  List(
                    Comment(" comment 176", Some(Position(4765, 346, 5))),
                    Comment(" comment 177", Some(Position(4783, 347, 5)))),
                  Some(Position(4454, 321, 3))
                )),
              List(
                Comment(" comment 146", Some(Position(4228, 300, 3))),
                Comment(" comment 147", Some(Position(4242, 301, 1)))),
              List(
                Comment(" comment 178", Some(Position(4803, 349, 3))),
                Comment(" comment 179", Some(Position(4819, 350, 3)))),
              Some(Position(4257, 303, 1))
            )),
          List(
            Comment(" comment 180", Some(Position(4835, 352, 1))),
            Comment(" comment 181", Some(Position(4849, 353, 1)))),
          Some(Position(0, 1, 1)),
          None
        )

      QueryParser.parse(query) map (_.withoutSourceMapper) should be (Success(expected))
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