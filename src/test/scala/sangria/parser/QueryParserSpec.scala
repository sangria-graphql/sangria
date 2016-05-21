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
            OperationDefinition(OperationType.Query, Some("FetchLukeAndLeiaAliased"),
              List(
                VariableDefinition("someVar", NamedType("Int", Some(Position(53, 2, 41))),
                  Some(BigDecimalValue(1.23, None, Some(Position(59, 2, 47)))),
                  None,
                  Some(Position(43, 2, 31))),
                VariableDefinition("anotherVar", NamedType("Int", Some(Position(77, 2, 65))),
                  Some(BigIntValue(123, None, Some(Position(83, 2, 71)))),
                  None,
                  Some(Position(64, 2, 52)))),
              List(
                Directive("include",
                  List(
                    Argument("if",
                      BooleanValue(true, None, Some(Position(100, 2, 88))),
                      None,
                      Some(Position(96, 2, 84)))),
                  None,
                  Some(Position(87, 2, 75))),
                Directive("include",
                  List(
                    Argument("if",
                      BooleanValue(false, None, Some(Position(119, 2, 107))),
                      None,
                      Some(Position(115, 2, 103)))),
                  None,
                  Some(Position(106, 2, 94)))),
              List(
                Field(Some("luke"), "human",
                  List(
                    Argument("id",
                      StringValue("1000", None, Some(Position(145, 3, 19))),
                      None,
                      Some(Position(141, 3, 15)))),
                  List(
                    Directive("include",
                      List(
                        Argument("if",
                          BooleanValue(true, None, Some(Position(165, 3, 39))),
                          None,
                          Some(Position(161, 3, 35)))),
                      None,
                      Some(Position(152, 3, 26)))),
                  List(
                    Field(None, "friends",
                      List(Argument("sort", EnumValue("NAME", None, Some(Position(190, 4, 19))), None, Some(Position(184, 4, 13)))),
                      Nil,
                      Nil,
                      None,
                      Some(Position(176, 4, 5)))),
                  None,
                  Some(Position(129, 3, 3))),
                Field(Some("leia"), "human",
                  List(Argument("id", StringValue("10103\n ö ö", None, Some(Position(223, 6, 24))), None, Some(Position(214, 6, 15)))),
                  Nil,
                  List(Field(None, "name", Nil, Nil, Nil, None, Some(Position(249, 7, 5)))),
                  None,
                  Some(Position(202, 6, 3))),
                InlineFragment(Some(NamedType("User", Some(Position(280, 10, 10)))),
                  Nil,
                  List(
                    Field(None, "birth", Nil, Nil,
                      List(
                        Field(None, "day", Nil, Nil, Nil, None, Some(Position(297, 11, 11)))), None, Some(Position(291, 11, 5)))), None, Some(Position(273, 10, 3))),
                FragmentSpread("Foo", Nil, None, Some(Position(309, 14, 3)))),
              Some(Comment(Vector(" test query"), Some(Position(0, 1, 1)))),
              Some(Position(13, 2, 1))),
            FragmentDefinition("Foo", NamedType("User", Some(Position(335, 17, 17))),
              List(Directive("foo",
                List(Argument("bar", BigIntValue(1, None, Some(Position(350, 17, 32))), None, Some(Position(345, 17, 27)))),
                None,
                Some(Position(340, 17, 22)))),
              List(Field(None, "baz", Nil, Nil, Nil, None, Some(Position(356, 18, 3)))),
              None,
              Some(Position(319, 17, 1)))),
          Some(Position(0, 1, 1)),
          None)

      QueryParser.parse(query) map (_.copy(sourceMapper = None)) should be (Success(expectedAst))
    }

    "parse kitchen sink" in {
      val query = FileUtil loadQuery "kitchen-sink.graphql"
      
      val expectedAst =
        Document(
          List(
            OperationDefinition(OperationType.Query, Some("queryName"),
              List(
                VariableDefinition("foo", NamedType("ComplexType", Some(Position(310, 8, 23))), None, None, Some(Position(304, 8, 17))),
                VariableDefinition("site", NamedType("Site", Some(Position(330, 8, 43))),
                  Some(EnumValue("MOBILE", None, Some(Position(337, 8, 50)))), None, Some(Position(323, 8, 36)))),
              Nil,
              List(
                Field(Some("whoever123is"), "node",
                  List(
                    Argument("id", ListValue(
                      List(
                        BigIntValue(123, None, Some(Position(373, 9, 27))),
                        BigIntValue(456, None, Some(Position(378, 9, 32)))),
                      None, Some(Position(372, 9, 26))),
                      None, Some(Position(368, 9, 22)))),
                  Nil,
                  List(
                    Field(None, "id", Nil, Nil, Nil, None, Some(Position(390, 10, 5))),
                    InlineFragment(
                      Some(NamedType("User", Some(Position(406, 11, 12)))),
                      List(Directive("defer", Nil, None, Some(Position(411, 11, 17)))),
                      List(
                        Field(None, "field2", Nil, Nil,
                          List(
                            Field(None, "id", Nil, Nil, Nil, None, Some(Position(443, 13, 9))),
                            Field(Some("alias"), "field1",
                              List(
                                Argument("first", BigIntValue(10, None, Some(Position(476, 14, 29))), None, Some(Position(470, 14, 23))),
                                Argument("after", VariableValue("foo", None, Some(Position(486, 14, 39))), None, Some(Position(480, 14, 33)))),
                              List(
                                Directive("include",
                                  List(Argument("if", VariableValue("foo", None, Some(Position(506, 14, 59))), None, Some(Position(502, 14, 55)))),
                                  None, Some(Position(493, 14, 46)))),
                              List(
                                Field(None, "id", Nil, Nil, Nil, None, Some(Position(524, 15, 11))),
                                FragmentSpread("frag", Nil, None, Some(Position(538, 16, 11)))),
                              None, Some(Position(456, 14, 9)))),
                          None, Some(Position(426, 12, 7)))),
                      None, Some(Position(399, 11, 5)))),
                  None, Some(Position(349, 9, 3)))),
              Some(Comment(
                Vector(
                  " Copyright (c) 2015, Facebook, Inc.",
                  " All rights reserved.",
                  "",
                  " This source code is licensed under the BSD-style license found in the",
                  " LICENSE file in the root directory of this source tree. An additional grant",
                  " of patent rights can be found in the PATENTS file in the same directory."),
                Some(Position(0, 1, 1)))),
              Some(Position(288, 8, 1))),
            OperationDefinition(OperationType.Mutation, Some("likeStory"), Nil, Nil,
              List(
                Field(None, "like",
                  List(Argument("story", BigIntValue(123, None, Some(Position(612, 24, 15))), None, Some(Position(605, 24, 8)))),
                  List(Directive("defer", Nil, None, Some(Position(617, 24, 20)))),
                  List(Field(None, "story", Nil, Nil, List(Field(None, "id", Nil, Nil, Nil, None, Some(Position(644, 26, 7)))), None, Some(Position(630, 25, 5)))),
                  None, Some(Position(600, 24, 3)))),
              None, Some(Position(577, 23, 1))),
            OperationDefinition(OperationType.Subscription, Some("StoryLikeSubscription"),
              List(VariableDefinition("input", NamedType("StoryLikeSubscribeInput", Some(Position(703, 31, 44))), None, None, Some(Position(695, 31, 36)))),
              Nil,
              List(
                Field(None, "storyLikeSubscribe",
                  List(Argument("input", VariableValue("input", None, Some(Position(758, 32, 29))), None, Some(Position(751, 32, 22)))),
                  Nil,
                  List(
                    Field(None, "story", Nil, Nil,
                      List(
                        Field(None, "likers", Nil, Nil,
                          List(
                            Field(None, "count", Nil, Nil, Nil, None, Some(Position(803, 35, 9)))),
                          None, Some(Position(786, 34, 7))),
                        Field(None, "likeSentence", Nil, Nil,
                          List(Field(None, "text", Nil, Nil, Nil, None, Some(Position(846, 38, 9)))),
                          None, Some(Position(823, 37, 7)))),
                      None, Some(Position(772, 33, 5)))),
                  None, Some(Position(732, 32, 3)))),
              None, Some(Position(660, 31, 1))),
            FragmentDefinition("frag", NamedType("Friend", Some(Position(889, 44, 18))), Nil,
              List(
                Field(None, "foo",
                  List(
                    Argument("size", VariableValue("size", None, Some(Position(910, 45, 13))), None, Some(Position(904, 45, 7))),
                    Argument("bar", VariableValue("b", None, Some(Position(922, 45, 25))), None, Some(Position(917, 45, 20))),
                    Argument("obj",
                      ObjectValue(
                        List(ObjectField("key", StringValue("value", None, Some(Position(937, 45, 40))), None, Some(Position(932, 45, 35)))),
                        None, Some(Position(931, 45, 34))),
                      None, Some(Position(926, 45, 29)))),
                  Nil,
                  Nil,
                  None, Some(Position(900, 45, 3)))),
              None, Some(Position(872, 44, 1))),
            OperationDefinition(OperationType.Query, None, Nil, Nil,
              List(
                Field(None, "unnamed",
                  List(
                    Argument("truthy", BooleanValue(true, None, Some(Position(970, 49, 19))), None, Some(Position(962, 49, 11))),
                    Argument("falsey", BooleanValue(false, None, Some(Position(984, 49, 33))), None, Some(Position(976, 49, 25)))),
                  Nil,
                  Nil,
                  None, Some(Position(954, 49, 3))),
                Field(None, "query", Nil, Nil, Nil, None, Some(Position(994, 50, 3))),
                InlineFragment(None,
                  List(
                    Directive("skip",
                      List(Argument("unless", VariableValue("foo", None, Some(Position(1021, 52, 21))), None, Some(Position(1013, 52, 13)))),
                      None, Some(Position(1007, 52, 7)))),
                  List(Field(None, "id", Nil, Nil, Nil, None, Some(Position(1033, 53, 5)))),
                  None, Some(Position(1003, 52, 3))),
                InlineFragment(None, Nil, List(Field(None, "id", Nil, Nil, Nil, None, Some(Position(1052, 56, 5)))), None, Some(Position(1042, 55, 3)))),
              None, Some(Position(950, 48, 1)))),
          Some(Position(0, 1, 1)),
          None)

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
              Field(None, "foo", Nil, Nil, Nil, None, Some(Position(31, 3, 13))),
              Field(None, "bar", Nil, Nil, Nil, None, Some(Position(35, 3, 17))),
              Field(None, "baz", Nil, Nil, Nil, None, Some(Position(52, 4, 13)))),
            None,
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
              Field(None, "foo", Nil, Nil, Nil, None, Some(Position(51, 4, 15))),
              Field(None, "bar", Nil, Nil, Nil, None, Some(Position(55, 4, 19)))),
              None,
              Some(Position(31, 3, 13))),
            InlineFragment(None,
              List(Directive("include", List(
                Argument("if", BooleanValue(true, None, Some(Position(103, 7, 30))),
                  None, Some(Position(99, 7, 26)))),
                None,
                Some(Position(90, 7, 17)))),
              List(Field(None, "baz", Nil, Nil, Nil, None, Some(Position(125, 8, 15)))),
              None,
              Some(Position(86, 7, 13)))),
            None,
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
              Field(None, "foo", Nil, Nil, Nil, None, Some(Position(34, 3, 13))),
              Field(None, "bar", Nil, Nil, Nil, None, Some(Position(38, 3, 17))),
              Field(None, "baz", Nil, Nil, Nil, None, Some(Position(55, 4, 13)))),
            None,
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
        """Invalid input "{ field: {", expected OperationDefinition or FragmentDefinition (line 1, column 1):
          |{ field: {} }
          |^""".stripMargin) (after being strippedOfCarriageReturns)
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
        case Document(defs, _, _) ⇒ defs map findAst[T] find (_.isDefined) flatten
        case OperationDefinition(_, _, vars, _, _, _, _) ⇒ vars map findAst[T] find (_.isDefined) flatten
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
          Some(BigIntValue(expected._2, None, Some(Position(24, 1, 25)))))
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
            Some(BigDecimalValue(expected._2, None, Some(Position(24, 1, 25)))))
        }
      }
    }

    "parse input values independently" in {
      val expectedTable = List(
        "null" → NullValue(None, Some(Position(0, 1, 1))),
        "1.234" → BigDecimalValue(BigDecimal("1.234"), None, Some(Position(0, 1, 1))),
        "HELLO_WORLD" → EnumValue("HELLO_WORLD", None, Some(Position(0, 1, 1))),
        "[1, 2 \"test\"]" → ListValue(
          List(
            BigIntValue(1, None, Some(Position(1, 1, 2))),
            BigIntValue(2, None, Some(Position(4, 1, 5))),
            StringValue("test", None, Some(Position(6, 1, 7)))),
          None,
          Some(Position(0, 1, 1))),
        "{a: 1, b: \"foo\" c: {nest: true, oops: null, e: FOO_BAR}}" →
          ObjectValue(
            List(
              ObjectField("a", BigIntValue(1, None, Some(Position(4, 1, 5))), None, Some(Position(1, 1, 2))),
              ObjectField("b", StringValue("foo", None, Some(Position(10, 1, 11))), None, Some(Position(7, 1, 8))),
              ObjectField("c",
                ObjectValue(
                  List(
                    ObjectField("nest", BooleanValue(true, None, Some(Position(26, 1, 27))), None, Some(Position(20, 1, 21))),
                    ObjectField("oops", NullValue(None, Some(Position(38, 1, 39))), None, Some(Position(32, 1, 33))),
                    ObjectField("e", EnumValue("FOO_BAR", None, Some(Position(47, 1, 48))), None, Some(Position(44, 1, 45)))),
                  None,
                  Some(Position(19, 1, 20))),
                None,
                Some(Position(16, 1, 17)))),
            None,
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
              ObjectField("a", BigIntValue(1, None, Some(Position(26, 3, 15))), None, Some(Position(23, 3, 12))),
              ObjectField("b", StringValue("foo", None, Some(Position(80, 6, 15))),
                Some(Comment(Vector(" This is a test comment!"), Some(Position(40, 5, 12)))),
                Some(Position(77, 6, 12)))),
            None,
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
        Success(
          Document(
            List(
              OperationDefinition(OperationType.Query, Some("queryName"),
                List(
                  VariableDefinition("foo", NamedType("ComplexType", Some(Position(434, 23, 1))),
                    None,
                    Some(Comment(Vector(" comment 5", " comment 6"), Some(Position(354, 15, 1)))),
                    Some(Position(378, 17, 1))),
                  VariableDefinition("site", NamedType("Site", Some(Position(565, 36, 1))),
                    Some(EnumValue("MOBILE", Some(Comment(Vector(" comment 16.5", " comment 16.6"), Some(Position(602, 40, 1)))),
                      Some(Position(632, 42, 1)))),
                    Some(Comment(Vector(" comment 11", " comment 12", " comment 13", " comment 14"), Some(Position(446, 24, 1)))),
                    Some(Position(501, 30, 1))),
                  VariableDefinition("foo", NamedType("ComplexType", Some(Position(703, 48, 7))),
                    Some(ObjectValue(
                      List(
                        ObjectField("field1",
                          StringValue("val",
                            Some(Comment(Vector(" comment 18.11", " comment 18.12"), Some(Position(849, 61, 1)))),
                            Some(Position(881, 63, 1))),
                          Some(Comment(Vector(" comment 18.7", " comment 18.8"), Some(Position(779, 55, 1)))),
                          Some(Position(809, 57, 1))),
                        ObjectField("list",
                          ListValue(
                            List(
                              BigIntValue(1,
                                Some(Comment(Vector(" comment 18.21", " comment 18.22"), Some(Position(1026, 76, 1)))),
                                Some(Position(1058, 78, 1))),
                              BigIntValue(2,
                                Some(Comment(Vector(" comment 18.23", " comment 18.24"), Some(Position(1061, 79, 1)))),
                                Some(Position(1093, 81, 1))),
                              BigIntValue(3,
                                Some(Comment(Vector(" comment 18.25", " comment 18.26"), Some(Position(1096, 82, 1)))),
                                Some(Position(1128, 84, 1)))),
                            Some(Comment(Vector(" comment 18.19", " comment 18.20"), Some(Position(992, 73, 1)))),
                            Some(Position(1024, 75, 1))),
                          Some(Comment(Vector(" comment 18.13", " comment 18.14", " comment 18.15", " comment 18.16"), Some(Position(887, 64, 1)))),
                          Some(Position(953, 69, 1))),
                        ObjectField("field2",
                          BooleanValue(true,
                            Some(Comment(Vector(" comment 18.35", " comment 18.36"), Some(Position(1271, 97, 1)))),
                            Some(Position(1303, 99, 1))),
                          Some(Comment(Vector(" comment 18.29", " comment 18.30", " comment 18.31", " comment 18.32"), Some(Position(1164, 88, 1)))),
                          Some(Position(1230, 93, 1)))),
                      Some(Comment(Vector(" comment 18.5", " comment 18.6"), Some(Position(747, 52, 1)))), Some(Position(777, 54, 1)))),
                    Some(Comment(Vector(" comment 17", " comment 18", " comment 18.1", " comment 18.2"), Some(Position(639, 43, 1)))),
                    Some(Position(697, 48, 1)))),
                Nil,
                List(
                  Field(Some("whoever123is"), "node",
                    List(
                      Argument("id",
                        ListValue(List(
                          BigIntValue(123,
                            Some(Comment(Vector(" comment 35", " comment 36"), Some(Position(1660, 130, 3)))),
                            Some(Position(1690, 132, 3))),
                          BigIntValue(456,
                            Some(Comment(Vector(" comment 37", " comment 38"), Some(Position(1696, 133, 3)))),
                            Some(Position(1726, 135, 3)))),
                          Some(Comment(Vector(" comment 33", " comment 34"), Some(Position(1626, 127, 3)))),
                          Some(Position(1656, 129, 3))),
                        Some(Comment(Vector(" comment 29", " comment 30"), Some(Position(1557, 121, 3)))),
                        Some(Position(1587, 123, 3)))),
                    Nil,
                    List(
                      Field(None, "id", Nil, Nil, Nil,
                        Some(Comment(Vector(" comment 44", " comment 45"), Some(Position(1837, 145, 4)))),
                        Some(Position(1870, 147, 5))),
                      InlineFragment(Some(NamedType("User", Some(Position(1996, 156, 5)))),
                        List(Directive("defer", Nil,
                          Some(Comment(Vector(" comment 52", " comment 53"), Some(Position(2005, 157, 5)))),
                          Some(Position(2039, 159, 5)))),
                        List(
                          Field(None, "field2", Nil, Nil,
                            List(
                              Field(Some("alias"), "field1",
                                List(
                                  Argument("first",
                                    BigIntValue(10,
                                      Some(Comment(Vector(" comment 70", " comment 71"), Some(Position(2474, 185, 9)))),
                                      Some(Position(2516, 187, 9))),
                                    Some(Comment(Vector(" comment 66", " comment 67"), Some(Position(2366, 179, 9)))),
                                    Some(Position(2408, 181, 9))),
                                  Argument("after",
                                    VariableValue("foo",
                                      Some(Comment(Vector(" comment 76", " comment 77"), Some(Position(2636, 194, 9)))),
                                      Some(Position(2678, 196, 9))),
                                    Some(Comment(Vector(" comment 72", " comment 73"), Some(Position(2528, 188, 9)))),
                                    Some(Position(2570, 190, 9)))),
                                List(
                                  Directive("include",
                                    List(Argument("if",
                                      VariableValue("foo",
                                        Some(Comment(Vector(" comment 88", " comment 89"), Some(Position(2961, 212, 10)))),
                                        Some(Position(3005, 214, 10))),
                                      Some(Comment(Vector(" comment 84", " comment 85"), Some(Position(2855, 206, 9)))),
                                      Some(Position(2897, 208, 9)))),
                                    Some(Comment(Vector(" comment 80", " comment 81"), Some(Position(2744, 200, 9)))),
                                    Some(Position(2786, 202, 9)))),
                                List(
                                  Field(None, "id", Nil, Nil, Nil,
                                    Some(Comment(Vector(" comment 94", " comment 95"), Some(Position(3130, 221, 11)))),
                                    Some(Position(3176, 223, 11))),
                                  FragmentSpread("frag", Nil,
                                    Some(Comment(Vector(" comment 96", " comment 97"), Some(Position(3190, 224, 11)))),
                                    Some(Position(3237, 227, 11)))),
                                Some(Comment(Vector(" comment 58", " comment 59"), Some(Position(2151, 167, 7)))),
                                Some(Position(2191, 169, 9)))),
                            None,
                            Some(Position(2092, 163, 7)))),
                        Some(Comment(Vector(" comment 46", " comment 47"), Some(Position(1879, 148, 5)))),
                        Some(Position(1913, 150, 5)))),
                    Some(Comment(Vector(" comment 21", " comment 22"), Some(Position(1408, 109, 2)))),
                    Some(Position(1437, 111, 3)))),
                Some(Comment(Vector(" Copyright (c) 2015, Facebook, Inc.", " All rights reserved.", "", " This source code is licensed under the BSD-style license found in the", " LICENSE file in the root directory of this source tree. An additional grant", " of patent rights can be found in the PATENTS file in the same directory."), Some(Position(0, 1, 1)))),
                Some(Position(288, 8, 1))),
              OperationDefinition(OperationType.Mutation, Some("likeStory"), Nil, Nil,
                List(
                  Field(None, "like",
                    List(
                      Argument("story",
                        BigIntValue(123,
                          Some(Comment(Vector(" comment 124", " comment 125"), Some(Position(3793, 268, 3)))),
                          Some(Position(3825, 270, 3))),
                        Some(Comment(Vector(" comment 120", " comment 121"), Some(Position(3717, 262, 3)))),
                        Some(Position(3749, 264, 3)))),
                    List(
                      Directive("defer", Nil,
                        Some(Comment(Vector(" comment 128", " comment 129"), Some(Position(3867, 274, 3)))),
                        Some(Position(3899, 276, 3)))),
                    List(
                      Field(None, "story", Nil, Nil,
                        List(
                          Field(None, "id", Nil, Nil, Nil,
                            Some(Comment(Vector(" comment 136", " comment 137", " comment 138", " comment 139"), Some(Position(4030, 286, 5)))),
                            Some(Position(4105, 291, 7)))),
                        Some(Comment(Vector(" comment 132", " comment 133"), Some(Position(3944, 280, 3)))),
                        Some(Position(3978, 282, 5)))),
                    Some(Comment(Vector(" comment 116", " comment 117"), Some(Position(3644, 256, 1)))),
                    Some(Position(3674, 258, 3)))),
                Some(Comment(Vector(" comment 110", " comment 111"), Some(Position(3536, 247, 4)))),
                Some(Position(3567, 249, 1))),
              FragmentDefinition("frag", NamedType("Friend", Some(Position(4358, 312, 1))), Nil,
                List(
                  InlineFragment(None,
                    List(
                      Directive("skip",
                        List(Argument("unless",
                          VariableValue("foo",
                            Some(Comment(Vector(" comment 168", " comment 169"), Some(Position(4613, 334, 3)))),
                            Some(Position(4645, 336, 3))),
                          Some(Comment(Vector(" comment 164", " comment 165"), Some(Position(4536, 328, 3)))),
                          Some(Position(4568, 330, 3)))),
                        Some(Comment(Vector(" comment 160", " comment 161"), Some(Position(4460, 322, 3)))),
                        Some(Position(4492, 324, 3)))),
                    List(
                      Field(None, "id", Nil, Nil, Nil,
                        Some(Comment(Vector(" comment 174", " comment 175"), Some(Position(4724, 343, 3)))),
                        Some(Position(4758, 345, 5)))),
                    Some(Comment(Vector(" comment 156", " comment 157", " comment 158", " comment 159"), Some(Position(4395, 316, 1)))),
                    Some(Position(4454, 321, 3)))),
                Some(Comment(Vector(" comment 146", " comment 147"), Some(Position(4228, 300, 3)))),
                Some(Position(4257, 303, 1)))),
            Some(Position(0, 1, 1)),
            None))

      QueryParser.parse(query) map (_.copy(sourceMapper = None)) should be (expected)
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