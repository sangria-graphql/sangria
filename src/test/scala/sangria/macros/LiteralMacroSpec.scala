package sangria.macros

import org.parboiled2.Position
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._

class LiteralMacroSpec extends WordSpec with Matchers {
  "literal graphql macro" should {
    "fail compilation on syntax error" in {
      """
        val ast = graphql" query Foo { luke...on 111 Fragment} "
      """ shouldNot compile
    }

    "parse complex query" in {
      val ast =
        graphql"""
          # test query
          query FetchLukeAndLeiaAliased($$someVar: Int = 1.23,$$anotherVar: Int = 123)@include(if: true) @include(if: false){
            luke: human(id: "1000")@include(if: true){
              friends(sort: NAME)
            }
            leia: human(id , : , "10103\n \u00F6 ö") {
              name # some name
            }

            ... on User {
              birth{day}
            }

            ...Foo
          }

          fragment Foo on User @foo(bar: 1){
            baz # field in fragment!
          }
        """

      ast.sourceMapper should not be ('empty)

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            OperationDefinition(OperationType.Query, Some("FetchLukeAndLeiaAliased"),
              List(
                VariableDefinition("someVar", NamedType("Int", Some(Position(74, 3, 51))),
                  Some(BigDecimalValue(1.23, None, Some(Position(80, 3, 57)))),
                  None, Some(Position(64, 3, 41))),
                VariableDefinition("anotherVar", NamedType("Int", Some(Position(98, 3, 75))),
                  Some(BigIntValue(123, None, Some(Position(104, 3, 81)))),
                  None, Some(Position(85, 3, 62)))),
              List(
                Directive("include",
                  List(Argument("if", BooleanValue(true, None, Some(Position(121, 3, 98))), None, Some(Position(117, 3, 94)))),
                  None, Some(Position(108, 3, 85))),
                Directive("include",
                  List(Argument("if", BooleanValue(false, None, Some(Position(140, 3, 117))), None, Some(Position(136, 3, 113)))),
                  None, Some(Position(127, 3, 104)))),
              List(
                Field(Some("luke"), "human",
                  List(Argument("id", StringValue("1000", None, Some(Position(176, 4, 29))), None, Some(Position(172, 4, 25)))),
                  List(
                    Directive("include",
                      List(Argument("if", BooleanValue(true, None, Some(Position(196, 4, 49))), None, Some(Position(192, 4, 45)))),
                      None, Some(Position(183, 4, 36)))),
                  List(
                    Field(None, "friends",
                      List(Argument("sort", EnumValue("NAME", None, Some(Position(231, 5, 29))), None, Some(Position(225, 5, 23)))),
                      Nil,
                      Nil,
                      None, Some(Position(217, 5, 15)))),
                  None, Some(Position(160, 4, 13))),
                Field(Some("leia"), "human",
                  List(Argument("id", StringValue("10103\n ö ö", None, Some(Position(284, 7, 34))), None, Some(Position(275, 7, 25)))),
                  Nil,
                  List(Field(None, "name", Nil, Nil, Nil, None, Some(Position(315, 8, 15)))),
                  None, Some(Position(263, 7, 13))),
                InlineFragment(Some(NamedType("User", Some(Position(366, 11, 20)))), Nil,
                  List(Field(None, "birth", Nil, Nil, List(Field(None, "day", Nil, Nil, Nil, None, Some(Position(393, 12, 21)))), None, Some(Position(387, 12, 15)))),
                  None, Some(Position(359, 11, 13))),
                FragmentSpread("Foo", Nil, None, Some(Position(425, 15, 13)))),
              Some(Comment(Vector(" test query"), Some(Position(11, 2, 11)))),
              Some(Position(34, 3, 11))),
            FragmentDefinition("Foo", NamedType("User", Some(Position(471, 18, 27))),
              List(
                Directive("foo",
                  List(Argument("bar", BigIntValue(1, None, Some(Position(486, 18, 42))), None, Some(Position(481, 18, 37)))),
                  None, Some(Position(476, 18, 32)))),
              List(Field(None, "baz", Nil, Nil, Nil, None, Some(Position(502, 19, 13)))), None, Some(Position(455, 18, 11)))),
          Some(Position(11, 2, 11)), None))
    }

    "parse kitchen sink" in {
      val ast =
        graphql"""
          # Copyright (c) 2015, Facebook, Inc.
          # All rights reserved.
          #
          # This source code is licensed under the BSD-style license found in the
          # LICENSE file in the root directory of this source tree. An additional grant
          # of patent rights can be found in the PATENTS file in the same directory.

          query queryName($$foo: ComplexType, $$site: Site = MOBILE) {
            whoever123is: node(id: [123, 456]) {
              id ,
              ... on User @defer {
                field2 {
                  id ,
                  alias: field1(first:10, after:$$foo,) @include(if: $$foo) {
                    id,
                    ...frag
                  }
                }
              }
            }
          }

          subscription StoryLikeSubscription($$input: StoryLikeSubscribeInput) {
            storyLikeSubscribe(input: $$input) {
              story {
                likers {
                  count
                }
                likeSentence {
                  text
                }
              }
            }
          }

          mutation likeStory {
            like(story: 123) @defer {
              story {
                id
              }
            }
          }

          fragment frag on Friend {
            foo(size: $$size, bar: $$b, obj: {key: "value"})
          }

          {
            unnamed(truthy: true, falsey: false),
            query
          }
        """

      ast.sourceMapper should not be ('empty)

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            OperationDefinition(OperationType.Query, Some("queryName"),
              List(
                VariableDefinition("foo", NamedType("ComplexType", Some(Position(381, 9, 33))), None, None, Some(Position(375, 9, 27))),
                VariableDefinition("site", NamedType("Site", Some(Position(401, 9, 53))),
                  Some(EnumValue("MOBILE", None, Some(Position(408, 9, 60)))), None, Some(Position(394, 9, 46)))),
              Nil,
              List(
                Field(Some("whoever123is"), "node",
                  List(
                    Argument("id",
                      ListValue(
                        List(
                          BigIntValue(123, None, Some(Position(454, 10, 37))),
                          BigIntValue(456, None, Some(Position(459, 10, 42)))),
                        None, Some(Position(453, 10, 36))),
                      None, Some(Position(449, 10, 32)))),
                  Nil,
                  List(
                    Field(None, "id", Nil, Nil, Nil, None, Some(Position(481, 11, 15))),
                    InlineFragment(Some(NamedType("User", Some(Position(507, 12, 22)))),
                      List(Directive("defer", Nil, None, Some(Position(512, 12, 27)))),
                      List(
                        Field(None, "field2", Nil, Nil,
                          List(
                            Field(None, "id", Nil, Nil, Nil, None, Some(Position(564, 14, 19))),
                            Field(Some("alias"), "field1",
                              List(
                                Argument("first", BigIntValue(10, None, Some(Position(607, 15, 39))), None, Some(Position(601, 15, 33))),
                                Argument("after", VariableValue("foo", None, Some(Position(617, 15, 49))),
                                  None, Some(Position(611, 15, 43)))),
                              List(
                                Directive("include",
                                  List(Argument("if", VariableValue("foo", None, Some(Position(637, 15, 69))), None, Some(Position(633, 15, 65)))),
                                  None, Some(Position(624, 15, 56)))),
                              List(
                                Field(None, "id", Nil, Nil, Nil, None, Some(Position(665, 16, 21))),
                                FragmentSpread("frag", Nil, None, Some(Position(689, 17, 21)))),
                              None, Some(Position(587, 15, 19)))),
                          None, Some(Position(537, 13, 17)))),
                      None, Some(Position(500, 12, 15)))),
                  None, Some(Position(430, 10, 13)))),
              Some(Comment(
                Vector(
                  " Copyright (c) 2015, Facebook, Inc.",
                  " All rights reserved.",
                  "",
                  " This source code is licensed under the BSD-style license found in the",
                  " LICENSE file in the root directory of this source tree. An additional grant",
                  " of patent rights can be found in the PATENTS file in the same directory."),
                Some(Position(11, 2, 11)))),
              Some(Position(359, 9, 11))),
            OperationDefinition(OperationType.Subscription, Some("StoryLikeSubscription"),
              List(VariableDefinition("input", NamedType("StoryLikeSubscribeInput", Some(Position(831, 24, 54))), None, None, Some(Position(823, 24, 46)))),
              Nil,
              List(
                Field(None, "storyLikeSubscribe",
                  List(Argument("input", VariableValue("input", None, Some(Position(896, 25, 39))), None, Some(Position(889, 25, 32)))),
                  Nil,
                  List(
                    Field(None, "story", Nil, Nil,
                      List(
                        Field(None, "likers", Nil, Nil,
                          List(
                            Field(None, "count", Nil, Nil, Nil, None, Some(Position(971, 28, 19)))),
                              None, Some(Position(944, 27, 17))),
                        Field(None, "likeSentence", Nil, Nil,
                          List(Field(None, "text", Nil, Nil, Nil, None, Some(Position(1044, 31, 19)))),
                          None, Some(Position(1011, 30, 17)))),
                      None, Some(Position(920, 26, 15)))),
                  None, Some(Position(870, 25, 13)))),
              None, Some(Position(788, 24, 11))),
            OperationDefinition(OperationType.Mutation, Some("likeStory"), Nil, Nil,
              List(
                Field(None, "like",
                  List(Argument("story", BigIntValue(123, None, Some(Position(1165, 38, 25))), None, Some(Position(1158, 38, 18)))),
                  List(Directive("defer", Nil, None, Some(Position(1170, 38, 30)))),
                  List(Field(None, "story", Nil, Nil,
                    List(Field(None, "id", Nil, Nil, Nil, None, Some(Position(1217, 40, 17)))),
                    None, Some(Position(1193, 39, 15)))),
                  None, Some(Position(1153, 38, 13)))),
              None, Some(Position(1120, 37, 11))),
            FragmentDefinition("frag", NamedType("Friend", Some(Position(1290, 45, 28))), Nil,
              List(
                Field(None, "foo",
                  List(
                    Argument("size", VariableValue("size", None, Some(Position(1321, 46, 23))), None, Some(Position(1315, 46, 17))),
                    Argument("bar", VariableValue("b", None, Some(Position(1333, 46, 35))), None, Some(Position(1328, 46, 30))),
                    Argument("obj",
                      ObjectValue(
                        List(ObjectField("key", StringValue("value", None, Some(Position(1348, 46, 50))), None, Some(Position(1343, 46, 45)))),
                        None, Some(Position(1342, 46, 44))),
                      None, Some(Position(1337, 46, 39)))),
                  Nil,
                  Nil,
                  None, Some(Position(1311, 46, 13)))),
              None, Some(Position(1273, 45, 11))),
            OperationDefinition(OperationType.Query, None, Nil, Nil,
              List(
                Field(None, "unnamed",
                  List(
                    Argument("truthy", BooleanValue(true, None, Some(Position(1411, 50, 29))), None, Some(Position(1403, 50, 21))),
                    Argument("falsey", BooleanValue(false, None, Some(Position(1425, 50, 43))), None, Some(Position(1417, 50, 35)))),
                  Nil,
                  Nil,
                  None, Some(Position(1395, 50, 13))),
                Field(None, "query", Nil, Nil, Nil, None, Some(Position(1445, 51, 13)))),
              None, Some(Position(1381, 49, 11)))),
          Some(Position(11, 2, 11)),
          None))
    }

    "parse input values with macro independently" in {
      val ast = graphqlInput"""
        {
          a: null

          # test comment
          b: 1
          c: {
            someNull: null
            enum: HELLO
          }
        }
      """

      ast should be (
        ObjectValue(
          List(
            ObjectField("a", NullValue(None, Some(Position(24, 3, 14))), None, Some(Position(21, 3, 11))),
            ObjectField("b", BigIntValue(1, None, Some(Position(68, 6, 14))),
              Some(Comment(Vector(" test comment"), Some(Position(40, 5, 11)))),
              Some(Position(65, 6, 11))),
            ObjectField("c",
              ObjectValue(
                List(
                  ObjectField("someNull", NullValue(None, Some(Position(107, 8, 23))), None, Some(Position(97, 8, 13))),
                  ObjectField("enum", EnumValue("HELLO", None, Some(Position(130, 9, 19))), None, Some(Position(124, 9, 13)))),
                None, Some(Position(83, 7, 14))),
              None, Some(Position(80, 7, 11)))),
          None, Some(Position(9, 2, 9))))
    }

    "fail compilation on syntax error in input values" in {
      """
        val ast = graphqlInput" {a: 1 c} "
      """ shouldNot compile
    }
  }
}
