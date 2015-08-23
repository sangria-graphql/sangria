package sangria.macros

import org.parboiled2.Position
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._
import sangria.macros.literal.LiteralGraphQLStringContext

class LiteralMacroSpec extends WordSpec with Matchers {
  "literal graphql macro" should {
    "parse complex query" in {
      val ast = graphql"""|# test query
        |query FetchLukeAndLeiaAliased($$someVar: Int = 1.23,$$anotherVar: Int = 123)@include(if: true) @include(if: false){
        |  luke: human(id: "1000")@include(if: true){
        |    friends(sort: NAME)
        |  }
        |  leia: human(id , : , "10103\n \u00F6 ö") {
        |    name # some name
        |  }
        |
        |  ... on User {
        |    birth{day}
        |  }
        |
        |  ...Foo
        |}
        |
        |fragment Foo on User @foo(bar: 1){
        |  baz # field in fragment!
        |}"""

      ast should be {
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
                  List(Field(None, "name", Nil, Nil, Nil, Some(Position(244, 7, 5)))),
                  Some(Position(202, 6, 3))),
                InlineFragment(
                  NamedType("User", Some(Position(275, 10, 10))),
                  Nil,
                  List(Field(None, "birth", Nil, Nil, List(Field(None, "day", Nil, Nil, Nil, Some(Position(292, 11, 11)))), Some(Position(286, 11, 5)))),
                  Some(Position(268, 10, 3))),
                FragmentSpread("Foo", Nil, Some(Position(304, 14, 3)))), Some(Position(13, 2, 1))),
            FragmentDefinition(
              "Foo",
              NamedType("User", Some(Position(330, 17, 17))),
              List(
                Directive(
                  "foo",
                  List(Argument("bar", BigIntValue(BigInt(1), Some(Position(345, 17, 32))), Some(Position(340, 17, 27)))),
                  Some(Position(335, 17, 22)))),
              List(Field(None, "baz", Nil, Nil, Nil, Some(Position(351, 18, 3)))),
              Some(Position(314, 17, 1)))),
          Some(Position(13, 2, 1)))
      }
    }

    "parse kitchen sink" in {
      val ast =
        graphql"""|# Copyright (c) 2015, Facebook, Inc.
          |# All rights reserved.
          |#
          |# This source code is licensed under the BSD-style license found in the
          |# LICENSE file in the root directory of this source tree. An additional grant
          |# of patent rights can be found in the PATENTS file in the same directory.
          |
          |query queryName($$foo: ComplexType, $$site: Site = MOBILE) {
          |  whoever123is: node(id: [123, 456]) {
          |    id ,
          |    ... on User @defer {
          |      field2 {
          |        id ,
          |        alias: field1(first:10, after:$$foo,) @include(if: $$foo) {
          |          id,
          |          ...frag
          |        }
          |      }
          |    }
          |  }
          |}
          |
          |mutation likeStory {
          |  like(story: 123) @defer {
          |    story {
          |      id
          |    }
          |  }
          |}
          |
          |fragment frag on Friend {
          |  foo(size: $$size, bar: $$b, obj: {key: "value"})
          |}
          |
          |{
          |  unnamed(truthy: true, falsey: false),
          |  query
          |}"""

      ast should be {
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
                  Some(Position(304, 8, 17))),
                VariableDefinition(
                  "site",
                  NamedType("Site", Some(Position(330, 8, 43))),
                  Some(EnumValue("MOBILE", Some(Position(337, 8, 50)))),
                  Some(Position(323, 8, 36)))),
              Nil,
              List(
                Field(
                  Some("whoever123is"),
                  "node",
                  List(
                    Argument(
                      "id",
                      ListValue(List(BigIntValue(BigInt(123), Some(Position(373, 9, 27))), BigIntValue(BigInt(456), Some(Position(378, 9, 32)))), Some(Position(372, 9, 26))),
                      Some(Position(368, 9, 22)))),
                  Nil,
                  List(
                    Field(
                      None,
                      "id",
                      Nil,
                      Nil,
                      Nil,
                      Some(Position(390, 10, 5))),
                    InlineFragment(
                      NamedType("User", Some(Position(406, 11, 12))),
                      List(
                        Directive("defer", Nil, Some(Position(411, 11, 17)))),
                      List(
                        Field(
                          None,
                          "field2",
                          Nil,
                          Nil,
                          List(
                            Field(None, "id", Nil, Nil, Nil, Some(Position(443, 13, 9))),
                            Field(
                              Some("alias"),
                              "field1",
                              List(
                                Argument("first", BigIntValue(BigInt(10), Some(Position(476, 14, 29))), Some(Position(470, 14, 23))),
                                Argument("after", VariableValue("foo", Some(Position(486, 14, 39))), Some(Position(480, 14, 33)))),
                              List(
                                Directive(
                                  "include",
                                  List(Argument("if", VariableValue("foo", Some(Position(506, 14, 59))), Some(Position(502, 14, 55)))),
                                  Some(Position(493, 14, 46)))),
                              List(
                                Field(None, "id", Nil, Nil, Nil, Some(Position(524, 15, 11))),
                                FragmentSpread("frag", Nil, Some(Position(538, 16, 11)))),
                              Some(Position(456, 14, 9)))),
                          Some(Position(426, 12, 7)))),
                      Some(Position(399, 11, 5)))),
                  Some(Position(349, 9, 3)))),
              Some(Position(288, 8, 1))),
            OperationDefinition(
              OperationType.Mutation,
              Some("likeStory"),
              Nil,
              Nil,
              List(
                Field(
                  None,
                  "like",
                  List(Argument("story", BigIntValue(BigInt(123), Some(Position(612, 24, 15))), Some(Position(605, 24, 8)))),
                  List(Directive("defer", Nil, Some(Position(617, 24, 20)))),
                  List(
                    Field(None, "story", Nil, Nil, List(
                      Field(None, "id", Nil, Nil, Nil, Some(Position(644, 26, 7)))), Some(Position(630, 25, 5)))),
                  Some(Position(600, 24, 3)))),
              Some(Position(577, 23, 1))),
            FragmentDefinition(
              "frag",
              NamedType("Friend", Some(Position(677, 31, 18))),
              Nil,
              List(
                Field(
                  None,
                  "foo",
                  List(
                    Argument("size", VariableValue("size", Some(Position(698, 32, 13))), Some(Position(692, 32, 7))),
                    Argument("bar", VariableValue("b", Some(Position(710, 32, 25))), Some(Position(705, 32, 20))),
                    Argument("obj", ObjectValue(List(ObjectField("key", StringValue("value", Some(Position(725, 32, 40))), Some(Position(720, 32, 35)))), Some(Position(719, 32, 34))), Some(Position(714, 32, 29)))),
                  Nil,
                  Nil,
                  Some(Position(688, 32, 3)))),
              Some(Position(660, 31, 1))),
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
                    Argument("truthy", BooleanValue(true, Some(Position(758, 36, 19))), Some(Position(750, 36, 11))),
                    Argument("falsey", BooleanValue(false, Some(Position(772, 36, 33))), Some(Position(764, 36, 25)))),
                  Nil,
                  Nil,
                  Some(Position(742, 36, 3))),
                Field(None, "query", Nil, Nil, Nil, Some(Position(782, 37, 3)))),
              Some(Position(738, 35, 1)))),
          Some(Position(288, 8, 1)))
      }
    }
  }
}
