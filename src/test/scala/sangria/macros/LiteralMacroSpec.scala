package sangria.macros

import org.parboiled2.Position
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._
import sangria.util.DebugUtil

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

      ast.sourceMapper should not be 'empty

      ast.withoutSourceMapper should be (
        Document(
          List(
            OperationDefinition(
              OperationType.Query,
              Some("FetchLukeAndLeiaAliased"),
              List(
                VariableDefinition(
                  "someVar",
                  NamedType("Int", Some(Position(74, 3, 51))),
                  Some(BigDecimalValue(1.23, Nil, Some(Position(80, 3, 57)))),
                  Nil,
                  Some(Position(64, 3, 41))
                ),
                VariableDefinition(
                  "anotherVar",
                  NamedType("Int", Some(Position(98, 3, 75))),
                  Some(BigIntValue(123, Nil, Some(Position(104, 3, 81)))),
                  Nil,
                  Some(Position(85, 3, 62))
                )),
              List(
                Directive(
                  "include",
                  List(
                    Argument(
                      "if",
                      BooleanValue(true, Nil, Some(Position(121, 3, 98))),
                      Nil,
                      Some(Position(117, 3, 94))
                    )),
                  Nil,
                  Some(Position(108, 3, 85))
                ),
                Directive(
                  "include",
                  List(
                    Argument(
                      "if",
                      BooleanValue(false, Nil, Some(Position(140, 3, 117))),
                      Nil,
                      Some(Position(136, 3, 113))
                    )),
                  Nil,
                  Some(Position(127, 3, 104))
                )),
              List(
                Field(
                  Some("luke"),
                  "human",
                  List(
                    Argument(
                      "id",
                      StringValue("1000", Nil, Some(Position(176, 4, 29))),
                      Nil,
                      Some(Position(172, 4, 25))
                    )),
                  List(
                    Directive(
                      "include",
                      List(
                        Argument(
                          "if",
                          BooleanValue(true, Nil, Some(Position(196, 4, 49))),
                          Nil,
                          Some(Position(192, 4, 45))
                        )),
                      Nil,
                      Some(Position(183, 4, 36))
                    )),
                  List(
                    Field(
                      None,
                      "friends",
                      List(
                        Argument(
                          "sort",
                          EnumValue("NAME", Nil, Some(Position(231, 5, 29))),
                          Nil,
                          Some(Position(225, 5, 23))
                        )),
                      Nil,
                      Nil,
                      Nil,
                      Nil,
                      Some(Position(217, 5, 15))
                    )),
                  Nil,
                  Nil,
                  Some(Position(160, 4, 13))
                ),
                Field(
                  Some("leia"),
                  "human",
                  List(
                    Argument(
                      "id",
                      StringValue("10103\n \u00F6 \u00F6", Nil, Some(Position(284, 7, 34))),
                      Nil,
                      Some(Position(275, 7, 25))
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
                      Some(Position(315, 8, 15))
                    )),
                  Nil,
                  List(
                    Comment(" some name", Some(Position(320, 8, 20)))),
                  Some(Position(263, 7, 13))
                ),
                InlineFragment(
                  Some(NamedType("User", Some(Position(366, 11, 20)))),
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
                          Some(Position(393, 12, 21))
                        )),
                      Nil,
                      Nil,
                      Some(Position(387, 12, 15))
                    )),
                  Nil,
                  Nil,
                  Some(Position(359, 11, 13))
                ),
                FragmentSpread("Foo", Nil, Nil, Some(Position(425, 15, 13)))),
              List(
                Comment(" test query", Some(Position(11, 2, 11)))),
              Nil,
              Some(Position(34, 3, 11))
            ),
            FragmentDefinition(
              "Foo",
              NamedType("User", Some(Position(471, 18, 27))),
              List(
                Directive(
                  "foo",
                  List(
                    Argument(
                      "bar",
                      BigIntValue(1, Nil, Some(Position(486, 18, 42))),
                      Nil,
                      Some(Position(481, 18, 37))
                    )),
                  Nil,
                  Some(Position(476, 18, 32))
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
                  Some(Position(502, 19, 13))
                )),
              Nil,
              List(
                Comment(" field in fragment!", Some(Position(506, 19, 17)))),
              Some(Position(455, 18, 11))
            )),
          Nil,
          Some(Position(11, 2, 11)),
          None
        ))
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

      ast.sourceMapper should not be 'empty

      ast.withoutSourceMapper should be (
        Document(
          List(
            OperationDefinition(
              OperationType.Query,
              Some("queryName"),
              List(
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", Some(Position(381, 9, 33))),
                  None,
                  Nil,
                  Some(Position(375, 9, 27))
                ),
                VariableDefinition(
                  "site",
                  NamedType("Site", Some(Position(401, 9, 53))),
                  Some(EnumValue("MOBILE", Nil, Some(Position(408, 9, 60)))),
                  Nil,
                  Some(Position(394, 9, 46))
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
                          BigIntValue(123, Nil, Some(Position(454, 10, 37))),
                          BigIntValue(456, Nil, Some(Position(459, 10, 42)))),
                        Nil,
                        Some(Position(453, 10, 36))
                      ),
                      Nil,
                      Some(Position(449, 10, 32))
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
                      Some(Position(481, 11, 15))
                    ),
                    InlineFragment(
                      Some(NamedType("User", Some(Position(507, 12, 22)))),
                      List(
                        Directive(
                          "defer",
                          Nil,
                          Nil,
                          Some(Position(512, 12, 27))
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
                              Some(Position(564, 14, 19))
                            ),
                            Field(
                              Some("alias"),
                              "field1",
                              List(
                                Argument(
                                  "first",
                                  BigIntValue(10, Nil, Some(Position(607, 15, 39))),
                                  Nil,
                                  Some(Position(601, 15, 33))
                                ),
                                Argument(
                                  "after",
                                  VariableValue("foo", Nil, Some(Position(617, 15, 49))),
                                  Nil,
                                  Some(Position(611, 15, 43))
                                )),
                              List(
                                Directive(
                                  "include",
                                  List(
                                    Argument(
                                      "if",
                                      VariableValue("foo", Nil, Some(Position(637, 15, 69))),
                                      Nil,
                                      Some(Position(633, 15, 65))
                                    )),
                                  Nil,
                                  Some(Position(624, 15, 56))
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
                                  Some(Position(665, 16, 21))
                                ),
                                FragmentSpread("frag", Nil, Nil, Some(Position(689, 17, 21)))),
                              Nil,
                              Nil,
                              Some(Position(587, 15, 19))
                            )),
                          Nil,
                          Nil,
                          Some(Position(537, 13, 17))
                        )),
                      Nil,
                      Nil,
                      Some(Position(500, 12, 15))
                    )),
                  Nil,
                  Nil,
                  Some(Position(430, 10, 13))
                )),
              List(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(Position(11, 2, 11))),
                Comment(" All rights reserved.", Some(Position(58, 3, 11))),
                Comment("", Some(Position(91, 4, 11))),
                Comment(" This source code is licensed under the BSD-style license found in the", Some(Position(103, 5, 11))),
                Comment(" LICENSE file in the root directory of this source tree. An additional grant", Some(Position(185, 6, 11))),
                Comment(" of patent rights can be found in the PATENTS file in the same directory.", Some(Position(273, 7, 11)))),
              Nil,
              Some(Position(359, 9, 11))
            ),
            OperationDefinition(
              OperationType.Subscription,
              Some("StoryLikeSubscription"),
              List(
                VariableDefinition(
                  "input",
                  NamedType("StoryLikeSubscribeInput", Some(Position(831, 24, 54))),
                  None,
                  Nil,
                  Some(Position(823, 24, 46))
                )),
              Nil,
              List(
                Field(
                  None,
                  "storyLikeSubscribe",
                  List(
                    Argument(
                      "input",
                      VariableValue("input", Nil, Some(Position(896, 25, 39))),
                      Nil,
                      Some(Position(889, 25, 32))
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
                              Some(Position(971, 28, 19))
                            )),
                          Nil,
                          Nil,
                          Some(Position(944, 27, 17))
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
                              Some(Position(1044, 31, 19))
                            )),
                          Nil,
                          Nil,
                          Some(Position(1011, 30, 17))
                        )),
                      Nil,
                      Nil,
                      Some(Position(920, 26, 15))
                    )),
                  Nil,
                  Nil,
                  Some(Position(870, 25, 13))
                )),
              Nil,
              Nil,
              Some(Position(788, 24, 11))
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
                      BigIntValue(123, Nil, Some(Position(1165, 38, 25))),
                      Nil,
                      Some(Position(1158, 38, 18))
                    )),
                  List(
                    Directive(
                      "defer",
                      Nil,
                      Nil,
                      Some(Position(1170, 38, 30))
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
                          Some(Position(1217, 40, 17))
                        )),
                      Nil,
                      Nil,
                      Some(Position(1193, 39, 15))
                    )),
                  Nil,
                  Nil,
                  Some(Position(1153, 38, 13))
                )),
              Nil,
              Nil,
              Some(Position(1120, 37, 11))
            ),
            FragmentDefinition(
              "frag",
              NamedType("Friend", Some(Position(1290, 45, 28))),
              Nil,
              List(
                Field(
                  None,
                  "foo",
                  List(
                    Argument(
                      "size",
                      VariableValue("size", Nil, Some(Position(1321, 46, 23))),
                      Nil,
                      Some(Position(1315, 46, 17))
                    ),
                    Argument(
                      "bar",
                      VariableValue("b", Nil, Some(Position(1333, 46, 35))),
                      Nil,
                      Some(Position(1328, 46, 30))
                    ),
                    Argument(
                      "obj",
                      ObjectValue(
                        List(
                          ObjectField(
                            "key",
                            StringValue("value", Nil, Some(Position(1348, 46, 50))),
                            Nil,
                            Some(Position(1343, 46, 45))
                          )),
                        Nil,
                        Some(Position(1342, 46, 44))
                      ),
                      Nil,
                      Some(Position(1337, 46, 39))
                    )),
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Some(Position(1311, 46, 13))
                )),
              Nil,
              Nil,
              Some(Position(1273, 45, 11))
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
                      BooleanValue(true, Nil, Some(Position(1411, 50, 29))),
                      Nil,
                      Some(Position(1403, 50, 21))
                    ),
                    Argument(
                      "falsey",
                      BooleanValue(false, Nil, Some(Position(1425, 50, 43))),
                      Nil,
                      Some(Position(1417, 50, 35))
                    )),
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Some(Position(1395, 50, 13))
                ),
                Field(
                  None,
                  "query",
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Nil,
                  Some(Position(1445, 51, 13))
                )),
              Nil,
              Nil,
              Some(Position(1381, 49, 11))
            )),
          Nil,
          Some(Position(11, 2, 11)),
          None
        )
      )
    }

    "parse schema kitchen sink" in {
      val ast =
        graphql"""
          # Copyright (c) 2015, Facebook, Inc.
          # All rights reserved.
          #
          # This source code is licensed under the BSD-style license found in the
          # LICENSE file in the root directory of this source tree. An additional grant
          # of patent rights can be found in the PATENTS file in the same directory.

          schema {
            query: QueryType
            mutation: MutationType
          }

          type Foo implements Bar {
            one: Type
            two(argument: InputType!): Type
            three(argument: InputType, other: String): Int
            four(argument: String = "string"): String
            five(argument: [String] = ["string", "string"]): String
            six(argument: InputType = {key: "value"}): Type
          }

          type AnnotatedObject @onObject(arg: "value") {
            annotatedField(arg: Type = "default" @onArg): Type @onField
          }

          # It's an interface!
          interface Bar {
            one: Type
            four(argument: String = "string"): String
          }

          interface AnnotatedInterface @onInterface {
            annotatedField(arg: Type @onArg): Type @onField
          }

          union Feed = Story | Article | Advert

          union AnnotatedUnion @onUnion = A | B

          scalar CustomScalar

          scalar AnnotatedScalar @onScalar

          enum Site {
            # value 1
            DESKTOP
            # value 2
            MOBILE
          }

          enum AnnotatedEnum @onEnum {
            ANNOTATED_VALUE @onEnumValue
            OTHER_VALUE
          }

          input InputType {
            key: String!
            answer: Int = 42
          }

          input AnnotatedInput @onInputObjectType {
            # field comment
            annotatedField: Type @onField
          }

          extend type Foo {
            seven(argument: [String]): Type
          }

          extend type Foo @onType {}

          type NoFields {}

          directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

          directive @include(if: Boolean!)
            on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

        """

      ast.sourceMapper should not be 'empty

      ast.withoutSourceMapper should be (
        Document(
          List(
            SchemaDefinition(
              List(
                OperationTypeDefinition(OperationType.Query, NamedType("QueryType", Some(Position(387, 10, 20))), Nil, Some(Position(380, 10, 13))),
                OperationTypeDefinition(OperationType.Mutation, NamedType("MutationType", Some(Position(419, 11, 23))), Nil, Some(Position(409, 11, 13)))),
              Nil,
              List(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(Position(11, 2, 11))),
                Comment(" All rights reserved.", Some(Position(58, 3, 11))),
                Comment("", Some(Position(91, 4, 11))),
                Comment(" This source code is licensed under the BSD-style license found in the", Some(Position(103, 5, 11))),
                Comment(" LICENSE file in the root directory of this source tree. An additional grant", Some(Position(185, 6, 11))),
                Comment(" of patent rights can be found in the PATENTS file in the same directory.", Some(Position(273, 7, 11)))),
              Nil,
              Some(Position(359, 9, 11))
            ),
            ObjectTypeDefinition(
              "Foo",
              List(
                NamedType("Bar", Some(Position(475, 14, 31)))),
              List(
                FieldDefinition("one", NamedType("Type", Some(Position(498, 15, 18))), Nil, Nil, Nil, Some(Position(493, 15, 13))),
                FieldDefinition("two", NamedType("Type", Some(Position(542, 16, 40))), List(InputValueDefinition("argument", NotNullType(NamedType("InputType", Some(Position(529, 16, 27))), Some(Position(529, 16, 27))), None, Nil, Nil, Some(Position(519, 16, 17)))), Nil, Nil, Some(Position(515, 16, 13))),
                FieldDefinition("three", NamedType("Int", Some(Position(602, 17, 56))), List(InputValueDefinition("argument", NamedType("InputType", Some(Position(575, 17, 29))), None, Nil, Nil, Some(Position(565, 17, 19))), InputValueDefinition("other", NamedType("String", Some(Position(593, 17, 47))), None, Nil, Nil, Some(Position(586, 17, 40)))), Nil, Nil, Some(Position(559, 17, 13))),
                FieldDefinition("four", NamedType("String", Some(Position(653, 18, 48))), List(InputValueDefinition("argument", NamedType("String", Some(Position(633, 18, 28))), Some(StringValue("string", Nil, Some(Position(642, 18, 37)))), Nil, Nil, Some(Position(623, 18, 18)))), Nil, Nil, Some(Position(618, 18, 13))),
                FieldDefinition("five", NamedType("String", Some(Position(721, 19, 62))), List(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(688, 19, 29))), Some(Position(687, 19, 28))), Some(ListValue(
                  List(
                    StringValue("string", Nil, Some(Position(699, 19, 40))),
                    StringValue("string", Nil, Some(Position(709, 19, 50)))),
                  Nil,
                  Some(Position(698, 19, 39))
                )), Nil, Nil, Some(Position(677, 19, 18)))), Nil, Nil, Some(Position(672, 19, 13))),
                FieldDefinition("six", NamedType("Type", Some(Position(783, 20, 56))), List(InputValueDefinition("argument", NamedType("InputType", Some(Position(754, 20, 27))), Some(ObjectValue(
                  List(
                    ObjectField(
                      "key",
                      StringValue("value", Nil, Some(Position(772, 20, 45))),
                      Nil,
                      Some(Position(767, 20, 40))
                    )),
                  Nil,
                  Some(Position(766, 20, 39))
                )), Nil, Nil, Some(Position(744, 20, 17)))), Nil, Nil, Some(Position(740, 20, 13)))),
              Nil,
              Nil,
              Nil,
              Some(Position(455, 14, 11))
            ),
            ObjectTypeDefinition(
              "AnnotatedObject",
              Nil,
              List(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(916, 24, 59))), List(InputValueDefinition("arg", NamedType("Type", Some(Position(890, 24, 33))), Some(StringValue("default", Nil, Some(Position(897, 24, 40)))), List(Directive(
                  "onArg",
                  Nil,
                  Nil,
                  Some(Position(907, 24, 50))
                )), Nil, Some(Position(885, 24, 28)))), List(Directive(
                  "onField",
                  Nil,
                  Nil,
                  Some(Position(921, 24, 64))
                )), Nil, Some(Position(870, 24, 13)))),
              List(
                Directive(
                  "onObject",
                  List(
                    Argument(
                      "arg",
                      StringValue("value", Nil, Some(Position(847, 23, 47))),
                      Nil,
                      Some(Position(842, 23, 42))
                    )),
                  Nil,
                  Some(Position(832, 23, 32))
                )),
              Nil,
              Nil,
              Some(Position(811, 23, 11))
            ),
            InterfaceTypeDefinition(
              "Bar",
              List(
                FieldDefinition("one", NamedType("Type", Some(Position(1017, 29, 18))), Nil, Nil, Nil, Some(Position(1012, 29, 13))),
                FieldDefinition("four", NamedType("String", Some(Position(1069, 30, 48))), List(InputValueDefinition("argument", NamedType("String", Some(Position(1049, 30, 28))), Some(StringValue("string", Nil, Some(Position(1058, 30, 37)))), Nil, Nil, Some(Position(1039, 30, 18)))), Nil, Nil, Some(Position(1034, 30, 13)))),
              Nil,
              List(
                Comment(" It's an interface!", Some(Position(953, 27, 11)))),
              Nil,
              Some(Position(984, 28, 11))
            ),
            InterfaceTypeDefinition(
              "AnnotatedInterface",
              List(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(1189, 34, 47))), List(InputValueDefinition("arg", NamedType("Type", Some(Position(1175, 34, 33))), None, List(Directive(
                  "onArg",
                  Nil,
                  Nil,
                  Some(Position(1180, 34, 38))
                )), Nil, Some(Position(1170, 34, 28)))), List(Directive(
                  "onField",
                  Nil,
                  Nil,
                  Some(Position(1194, 34, 52))
                )), Nil, Some(Position(1155, 34, 13)))),
              List(
                Directive(
                  "onInterface",
                  Nil,
                  Nil,
                  Some(Position(1128, 33, 40))
                )),
              Nil,
              Nil,
              Some(Position(1099, 33, 11))
            ),
            UnionTypeDefinition(
              "Feed",
              List(
                NamedType("Story", Some(Position(1239, 37, 24))),
                NamedType("Article", Some(Position(1247, 37, 32))),
                NamedType("Advert", Some(Position(1257, 37, 42)))),
              Nil,
              Nil,
              Some(Position(1226, 37, 11))
            ),
            UnionTypeDefinition(
              "AnnotatedUnion",
              List(
                NamedType("A", Some(Position(1307, 39, 43))),
                NamedType("B", Some(Position(1311, 39, 47)))),
              List(
                Directive(
                  "onUnion",
                  Nil,
                  Nil,
                  Some(Position(1296, 39, 32))
                )),
              Nil,
              Some(Position(1275, 39, 11))
            ),
            ScalarTypeDefinition(
              "CustomScalar",
              Nil,
              Nil,
              Some(Position(1324, 41, 11))
            ),
            ScalarTypeDefinition(
              "AnnotatedScalar",
              List(
                Directive(
                  "onScalar",
                  Nil,
                  Nil,
                  Some(Position(1378, 43, 34))
                )),
              Nil,
              Some(Position(1355, 43, 11))
            ),
            EnumTypeDefinition(
              "Site",
              List(
                EnumValueDefinition("DESKTOP", Nil, List(Comment(" value 1", Some(Position(1423, 46, 13)))), Some(Position(1445, 47, 13))),
                EnumValueDefinition("MOBILE", Nil, List(Comment(" value 2", Some(Position(1465, 48, 13)))), Some(Position(1487, 49, 13)))),
              Nil,
              Nil,
              Nil,
              Some(Position(1399, 45, 11))
            ),
            EnumTypeDefinition(
              "AnnotatedEnum",
              List(
                EnumValueDefinition("ANNOTATED_VALUE", List(Directive(
                  "onEnumValue",
                  Nil,
                  Nil,
                  Some(Position(1574, 53, 29))
                )), Nil, Some(Position(1558, 53, 13))),
                EnumValueDefinition("OTHER_VALUE", Nil, Nil, Some(Position(1599, 54, 13)))),
              List(
                Directive(
                  "onEnum",
                  Nil,
                  Nil,
                  Some(Position(1536, 52, 30))
                )),
              Nil,
              Nil,
              Some(Position(1517, 52, 11))
            ),
            InputObjectTypeDefinition(
              "InputType",
              List(
                InputValueDefinition("key", NotNullType(NamedType("String", Some(Position(1669, 58, 18))), Some(Position(1669, 58, 18))), None, Nil, Nil, Some(Position(1664, 58, 13))),
                InputValueDefinition("answer", NamedType("Int", Some(Position(1697, 59, 21))), Some(BigIntValue(42, Nil, Some(Position(1703, 59, 27)))), Nil, Nil, Some(Position(1689, 59, 13)))),
              Nil,
              Nil,
              Nil,
              Some(Position(1634, 57, 11))
            ),
            InputObjectTypeDefinition(
              "AnnotatedInput",
              List(
                InputValueDefinition("annotatedField", NamedType("Type", Some(Position(1827, 64, 29))), None, List(Directive(
                  "onField",
                  Nil,
                  Nil,
                  Some(Position(1832, 64, 34))
                )), List(Comment(" field comment", Some(Position(1783, 63, 13)))), Some(Position(1811, 64, 13)))),
              List(
                Directive(
                  "onInputObjectType",
                  Nil,
                  Nil,
                  Some(Position(1750, 62, 32))
                )),
              Nil,
              Nil,
              Some(Position(1729, 62, 11))
            ),
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Foo",
                Nil,
                List(
                  FieldDefinition("seven", NamedType("Type", Some(Position(1921, 68, 40))), List(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(1911, 68, 30))), Some(Position(1910, 68, 29))), None, Nil, Nil, Some(Position(1900, 68, 19)))), Nil, Nil, Some(Position(1894, 68, 13)))),
                Nil,
                Nil,
                Nil,
                Some(Position(1871, 67, 18))
              ),
              Nil,
              Some(Position(1864, 67, 11))
            ),
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Foo",
                Nil,
                Nil,
                List(
                  Directive(
                    "onType",
                    Nil,
                    Nil,
                    Some(Position(1965, 71, 27))
                  )),
                Nil,
                Nil,
                Some(Position(1956, 71, 18))
              ),
              Nil,
              Some(Position(1949, 71, 11))
            ),
            ObjectTypeDefinition(
              "NoFields",
              Nil,
              Nil,
              Nil,
              Nil,
              Nil,
              Some(Position(1987, 73, 11))
            ),
            DirectiveDefinition(
              "skip",
              List(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(2035, 75, 31))), Some(Position(2035, 75, 31))), None, Nil, Nil, Some(Position(2031, 75, 27)))),
              List(
                DirectiveLocation("FIELD", Nil, Some(Position(2048, 75, 44))),
                DirectiveLocation("FRAGMENT_SPREAD", Nil, Some(Position(2056, 75, 52))),
                DirectiveLocation("INLINE_FRAGMENT", Nil, Some(Position(2074, 75, 70)))),
              Nil,
              Some(Position(2015, 75, 11))
            ),
            DirectiveDefinition(
              "include",
              List(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(2124, 77, 34))), Some(Position(2124, 77, 34))), None, Nil, Nil, Some(Position(2120, 77, 30)))),
              List(
                DirectiveLocation("FIELD", Nil, Some(Position(2149, 78, 16))),
                DirectiveLocation("FRAGMENT_SPREAD", Nil, Some(Position(2157, 78, 24))),
                DirectiveLocation("INLINE_FRAGMENT", Nil, Some(Position(2175, 78, 42)))),
              Nil,
              Some(Position(2101, 77, 11))
            )),
          Nil,
          Some(Position(11, 2, 11)),
          None
        )
      )
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
            ObjectField(
              "a",
              NullValue(Nil, Some(Position(24, 3, 14))),
              Nil,
              Some(Position(21, 3, 11))
            ),
            ObjectField(
              "b",
              BigIntValue(1, Nil, Some(Position(68, 6, 14))),
              List(
                Comment(" test comment", Some(Position(40, 5, 11)))),
              Some(Position(65, 6, 11))
            ),
            ObjectField(
              "c",
              ObjectValue(
                List(
                  ObjectField(
                    "someNull",
                    NullValue(Nil, Some(Position(107, 8, 23))),
                    Nil,
                    Some(Position(97, 8, 13))
                  ),
                  ObjectField(
                    "enum",
                    EnumValue("HELLO", Nil, Some(Position(130, 9, 19))),
                    Nil,
                    Some(Position(124, 9, 13))
                  )),
                Nil,
                Some(Position(83, 7, 14))
              ),
              Nil,
              Some(Position(80, 7, 11))
            )),
          Nil,
          Some(Position(9, 2, 9))
        ))
    }

    "fail compilation on syntax error in input values" in {
      """
        val ast = graphqlInput" {a: 1 c} "
      """ shouldNot compile
    }
  }
}
