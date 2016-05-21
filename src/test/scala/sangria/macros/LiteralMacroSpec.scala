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

//    "parse complex query" in {
//      val ast = graphql"""|# test query
//        |query FetchLukeAndLeiaAliased($$someVar: Int = 1.23,$$anotherVar: Int = 123)@include(if: true) @include(if: false){
//        |  luke: human(id: "1000")@include(if: true){
//        |    friends(sort: NAME)
//        |  }
//        |  leia: human(id , : , "10103\n \u00F6 ö") {
//        |    name # some name
//        |  }
//        |
//        |  ... on User {
//        |    birth{day}
//        |  }
//        |
//        |  ...Foo
//        |}
//        |
//        |fragment Foo on User @foo(bar: 1){
//        |  baz # field in fragment!
//        |}"""
//
//      ast.sourceMapper should not be ('empty)
//
//      ast.copy(sourceMapper = None) should be {
//        Document(
//          List(
//            OperationDefinition(
//              OperationType.Query,
//              Some("FetchLukeAndLeiaAliased"),
//              List(
//                VariableDefinition(
//                  "someVar",
//                  NamedType("Int", Some(Position(53, 2, 41))),
//                  Some(BigDecimalValue(BigDecimal("1.23"), Some(Position(59, 2, 47)))), Some(Position(43, 2, 31))),
//                VariableDefinition(
//                  "anotherVar",
//                  NamedType("Int", Some(Position(77, 2, 65))),
//                  Some(BigIntValue(BigInt(123), Some(Position(83, 2, 71)))), Some(Position(64, 2, 52)))),
//              List(
//                Directive(
//                  "include",
//                  List(Argument("if", BooleanValue(true, Some(Position(100, 2, 88))), Some(Position(96, 2, 84)))),
//                  Some(Position(87, 2, 75))),
//                Directive(
//                  "include",
//                  List(Argument("if", BooleanValue(false, Some(Position(119, 2, 107))), Some(Position(115, 2, 103)))),
//                  Some(Position(106, 2, 94)))),
//              List(
//                Field(
//                  Some("luke"),
//                  "human",
//                  List(Argument("id", StringValue("1000", Some(Position(145, 3, 19))), Some(Position(141, 3, 15)))),
//                  List(Directive("include", List(Argument("if", BooleanValue(true, Some(Position(165, 3, 39))), Some(Position(161, 3, 35)))), Some(Position(152, 3, 26)))),
//                  List(Field(None, "friends", List(Argument("sort", EnumValue("NAME", Some(Position(190, 4, 19))), Some(Position(184, 4, 13)))), Nil, Nil, Some(Position(176, 4, 5)))),
//                  Some(Position(129, 3, 3))),
//                Field(
//                  Some("leia"), "human",
//                  List(Argument("id", StringValue("10103\n ö ö", Some(Position(223, 6, 24))), Some(Position(214, 6, 15)))),
//                  Nil,
//                  List(Field(None, "name", Nil, Nil, Nil, Some(Position(244, 7, 5)))),
//                  Some(Position(202, 6, 3))),
//                InlineFragment(
//                  Some(NamedType("User", Some(Position(275, 10, 10)))),
//                  Nil,
//                  List(Field(None, "birth", Nil, Nil, List(Field(None, "day", Nil, Nil, Nil, Some(Position(292, 11, 11)))), Some(Position(286, 11, 5)))),
//                  Some(Position(268, 10, 3))),
//                FragmentSpread("Foo", Nil, Some(Position(304, 14, 3)))), Nil, Some(Position(13, 2, 1))),
//            FragmentDefinition(
//              "Foo",
//              NamedType("User", Some(Position(330, 17, 17))),
//              List(
//                Directive(
//                  "foo",
//                  List(Argument("bar", BigIntValue(BigInt(1), Some(Position(345, 17, 32))), Some(Position(340, 17, 27)))),
//                  Some(Position(335, 17, 22)))),
//              List(Field(None, "baz", Nil, Nil, Nil, Some(Position(351, 18, 3)))),
//              Some(Position(314, 17, 1)))),
//          Some(Position(13, 2, 1)))
//      }
//    }
//
//    "parse kitchen sink" in {
//      val ast =
//        graphql"""|# Copyright (c) 2015, Facebook, Inc.
//          |# All rights reserved.
//          |#
//          |# This source code is licensed under the BSD-style license found in the
//          |# LICENSE file in the root directory of this source tree. An additional grant
//          |# of patent rights can be found in the PATENTS file in the same directory.
//          |
//          |query queryName($$foo: ComplexType, $$site: Site = MOBILE) {
//          |  whoever123is: node(id: [123, 456]) {
//          |    id ,
//          |    ... on User @defer {
//          |      field2 {
//          |        id ,
//          |        alias: field1(first:10, after:$$foo,) @include(if: $$foo) {
//          |          id,
//          |          ...frag
//          |        }
//          |      }
//          |    }
//          |  }
//          |}
//          |
//          |subscription StoryLikeSubscription($$input: StoryLikeSubscribeInput) {
//          |  storyLikeSubscribe(input: $$input) {
//          |    story {
//          |      likers {
//          |        count
//          |      }
//          |      likeSentence {
//          |        text
//          |      }
//          |    }
//          |  }
//          |}
//          |
//          |mutation likeStory {
//          |  like(story: 123) @defer {
//          |    story {
//          |      id
//          |    }
//          |  }
//          |}
//          |
//          |fragment frag on Friend {
//          |  foo(size: $$size, bar: $$b, obj: {key: "value"})
//          |}
//          |
//          |{
//          |  unnamed(truthy: true, falsey: false),
//          |  query
//          |}"""
//
//      ast.sourceMapper should not be ('empty)
//
//
//      ast.copy(sourceMapper = None) should be {
//        Document(
//          List(
//            OperationDefinition(OperationType.Query,
//              Some("queryName"),
//              List(
//                VariableDefinition(
//                  "foo",
//                  NamedType(
//                    "ComplexType",
//                    Some(Position(310, 8, 23))),
//                  None,
//                  Some(Position(304, 8, 17))),
//                VariableDefinition(
//                  "site",
//                  NamedType(
//                    "Site",
//                    Some(Position(330, 8, 43))),
//                  Some(
//                    EnumValue(
//                      "MOBILE",
//                      Some(Position(337, 8, 50)))),
//                  Some(Position(323, 8, 36)))),
//              Nil,
//              List(
//                Field(
//                  Some("whoever123is"),
//                  "node",
//                  List(
//                    Argument(
//                      "id",
//                      ListValue(
//                        List(
//                          BigIntValue(123, Some(Position(373, 9, 27))),
//                          BigIntValue(456, Some(Position(378, 9, 32)))),
//                        Some(Position(372, 9, 26))),
//                      Some(Position(368, 9, 22)))),
//                  Nil,
//                  List(
//                    Field(
//                      None, "id", Nil, Nil, Nil, Some(Position(390, 10, 5))),
//                    InlineFragment(
//                      Some(NamedType("User", Some(Position(406, 11, 12)))),
//                      List(Directive("defer", Nil, Some(Position(411, 11, 17)))),
//                      List(
//                        Field(None, "field2", Nil, Nil,
//                          List(
//                            Field(None, "id", Nil, Nil, Nil, Some(Position(443, 13, 9))),
//                            Field(Some("alias"), "field1",
//                              List(
//                                Argument("first", BigIntValue(10, Some(Position(476, 14, 29))), Some(Position(470, 14, 23))),
//                                Argument("after", VariableValue("foo", Some(Position(486, 14, 39))), Some(Position(480, 14, 33)))),
//                              List(
//                                Directive("include",
//                                  List(Argument("if", VariableValue("foo", Some(Position(506, 14, 59))), Some(Position(502, 14, 55)))),
//                                  Some(Position(493, 14, 46)))),
//                              List(
//                                Field(None, "id", Nil, Nil, Nil, Some(Position(524, 15, 11))),
//                                FragmentSpread("frag", Nil, Some(Position(538, 16, 11)))),
//                              Some(Position(456, 14, 9)))),
//                          Some(Position(426, 12, 7)))),
//                      Some(Position(399, 11, 5)))),
//                  Some(Position(349, 9, 3)))),
//              Nil,
//              Some(Position(288, 8, 1))),
//            OperationDefinition(
//              OperationType.Subscription,
//              Some("StoryLikeSubscription"),
//              List(
//                VariableDefinition("input",
//                  NamedType("StoryLikeSubscribeInput", Some(Position(620, 23, 44))),
//                  None, Some(Position(612, 23, 36)))),
//              Nil,
//              List(
//                Field(None, "storyLikeSubscribe",
//                  List(Argument("input", VariableValue("input", Some(Position(675, 24, 29))), Some(Position(668, 24, 22)))),
//                  Nil,
//                  List(
//                    Field(None, "story", Nil, Nil, List(
//                      Field(None, "likers", Nil, Nil, List(
//                        Field(None, "count", Nil, Nil, Nil, Some(Position(720, 27, 9)))), Some(Position(703, 26, 7))),
//                      Field(None, "likeSentence", Nil, Nil, List(
//                        Field(None, "text", Nil, Nil, Nil, Some(Position(763, 30, 9)))), Some(Position(740, 29, 7)))), Some(Position(689, 25, 5)))),
//                  Some(Position(649, 24, 3)))),
//              Nil,
//              Some(Position(577, 23, 1))),
//            OperationDefinition(
//              OperationType.Mutation,
//              Some("likeStory"), Nil, Nil,
//              List(
//                Field(None, "like",
//                  List(Argument("story", BigIntValue(123, Some(Position(824, 37, 15))), Some(Position(817, 37, 8)))),
//                  List(Directive("defer", Nil, Some(Position(829, 37, 20)))),
//                  List(
//                    Field(None, "story", Nil, Nil, List(
//                      Field(None, "id", Nil, Nil, Nil, Some(Position(856, 39, 7)))), Some(Position(842, 38, 5)))),
//                  Some(Position(812, 37, 3)))),
//              Nil,
//              Some(Position(789, 36, 1))),
//            FragmentDefinition("frag", NamedType("Friend", Some(Position(889, 44, 18))),
//              Nil,
//              List(
//                Field(None, "foo",
//                  List(
//                    Argument("size", VariableValue("size", Some(Position(910, 45, 13))), Some(Position(904, 45, 7))),
//                    Argument("bar", VariableValue("b", Some(Position(922, 45, 25))), Some(Position(917, 45, 20))),
//                    Argument("obj",
//                      ObjectValue(List(
//                        ObjectField("key", StringValue("value", Some(Position(937, 45, 40))), Some(Position(932, 45, 35)))), Some(Position(931, 45, 34))),
//                      Some(Position(926, 45, 29)))),
//                  Nil,
//                  Nil,
//                  Some(Position(900, 45, 3)))),
//              Some(Position(872, 44, 1))),
//            OperationDefinition(OperationType.Query, None, Nil, Nil,
//              List(
//                Field(None, "unnamed",
//                  List(
//                    Argument("truthy", BooleanValue(true, Some(Position(970, 49, 19))), Some(Position(962, 49, 11))),
//                    Argument("falsey", BooleanValue(false, Some(Position(984, 49, 33))), Some(Position(976, 49, 25)))),
//                  Nil,
//                  Nil,
//                  Some(Position(954, 49, 3))),
//                Field(None, "query", Nil, Nil, Nil, Some(Position(994, 50, 3)))), Nil, Some(Position(950, 48, 1)))), Some(Position(288, 8, 1)), None)
//      }
//    }

//    "parse input values with macro independently" in {
//      val ast = graphqlInput"""
//        {
//          a: null
//
//          # test comment
//          b: 1
//          c: {
//            someNull: null
//            enum: HELLO
//          }
//        }
//      """
//
//      ast should be (
//        ObjectValue(
//          List(
//            ObjectField("a", NullValue(Some(Position(24, 3, 14))), Some(Position(21, 3, 11))),
//            ObjectField("b", BigIntValue(1, Some(Position(68, 6, 14))), Some(Position(65, 6, 11))),
//            ObjectField("c",
//              ObjectValue(
//                List(
//                  ObjectField("someNull", NullValue(Some(Position(107, 8, 23))), Some(Position(97, 8, 13))),
//                  ObjectField("enum", EnumValue("HELLO", Some(Position(130, 9, 19))), Some(Position(124, 9, 13)))),
//                Some(Position(83, 7, 14))),
//              Some(Position(80, 7, 11)))),
//          Some(Position(9, 2, 9))))
//    }

    "fail compilation on syntax error in input values" in {
      """
        val ast = graphqlInput" {a: 1 c} "
      """ shouldNot compile
    }
  }
}
