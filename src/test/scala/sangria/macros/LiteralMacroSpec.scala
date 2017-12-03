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
          Vector(
            OperationDefinition(
              OperationType.Query,
              Some("FetchLukeAndLeiaAliased"),
              Vector(
                VariableDefinition(
                  "someVar",
                  NamedType("Int", Some(Position(74, 3, 51))),
                  Some(BigDecimalValue(1.23, Vector.empty, Some(Position(80, 3, 57)))),
                  Vector.empty,
                  Some(Position(64, 3, 41))
                ),
                VariableDefinition(
                  "anotherVar",
                  NamedType("Int", Some(Position(98, 3, 75))),
                  Some(BigIntValue(123, Vector.empty, Some(Position(104, 3, 81)))),
                  Vector.empty,
                  Some(Position(85, 3, 62))
                )),
              Vector(
                Directive(
                  "include",
                  Vector(
                    Argument(
                      "if",
                      BooleanValue(true, Vector.empty, Some(Position(121, 3, 98))),
                      Vector.empty,
                      Some(Position(117, 3, 94))
                    )),
                  Vector.empty,
                  Some(Position(108, 3, 85))
                ),
                Directive(
                  "include",
                  Vector(
                    Argument(
                      "if",
                      BooleanValue(false, Vector.empty, Some(Position(140, 3, 117))),
                      Vector.empty,
                      Some(Position(136, 3, 113))
                    )),
                  Vector.empty,
                  Some(Position(127, 3, 104))
                )),
              Vector(
                Field(
                  Some("luke"),
                  "human",
                  Vector(
                    Argument(
                      "id",
                      StringValue("1000", false, None, Vector.empty, Some(Position(176, 4, 29))),
                      Vector.empty,
                      Some(Position(172, 4, 25))
                    )),
                  Vector(
                    Directive(
                      "include",
                      Vector(
                        Argument(
                          "if",
                          BooleanValue(true, Vector.empty, Some(Position(196, 4, 49))),
                          Vector.empty,
                          Some(Position(192, 4, 45))
                        )),
                      Vector.empty,
                      Some(Position(183, 4, 36))
                    )),
                  Vector(
                    Field(
                      None,
                      "friends",
                      Vector(
                        Argument(
                          "sort",
                          EnumValue("NAME", Vector.empty, Some(Position(231, 5, 29))),
                          Vector.empty,
                          Some(Position(225, 5, 23))
                        )),
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Some(Position(217, 5, 15))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Some(Position(160, 4, 13))
                ),
                Field(
                  Some("leia"),
                  "human",
                  Vector(
                    Argument(
                      "id",
                      StringValue("10103\n \u00F6 \u00F6", false, None, Vector.empty, Some(Position(284, 7, 34))),
                      Vector.empty,
                      Some(Position(275, 7, 25))
                    )),
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "name",
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Some(Position(315, 8, 15))
                    )),
                  Vector.empty,
                  Vector(
                    Comment(" some name", Some(Position(320, 8, 20)))),
                  Some(Position(263, 7, 13))
                ),
                InlineFragment(
                  Some(NamedType("User", Some(Position(366, 11, 20)))),
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "birth",
                      Vector.empty,
                      Vector.empty,
                      Vector(
                        Field(
                          None,
                          "day",
                          Vector.empty,
                          Vector.empty,
                          Vector.empty,
                          Vector.empty,
                          Vector.empty,
                          Some(Position(393, 12, 21))
                        )),
                      Vector.empty,
                      Vector.empty,
                      Some(Position(387, 12, 15))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Some(Position(359, 11, 13))
                ),
                FragmentSpread("Foo", Vector.empty, Vector.empty, Some(Position(425, 15, 13)))),
              Vector(
                Comment(" test query", Some(Position(11, 2, 11)))),
              Vector.empty,
              Some(Position(34, 3, 11))
            ),
            FragmentDefinition(
              "Foo",
              NamedType("User", Some(Position(471, 18, 27))),
              Vector(
                Directive(
                  "foo",
                  Vector(
                    Argument(
                      "bar",
                      BigIntValue(1, Vector.empty, Some(Position(486, 18, 42))),
                      Vector.empty,
                      Some(Position(481, 18, 37))
                    )),
                  Vector.empty,
                  Some(Position(476, 18, 32))
                )),
              Vector(
                Field(
                  None,
                  "baz",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(Position(502, 19, 13))
                )),
              Vector.empty,
              Vector(
                Comment(" field in fragment!", Some(Position(506, 19, 17)))),
              Some(Position(455, 18, 11))
            )),
          Vector.empty,
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
          Vector(
            OperationDefinition(
              OperationType.Query,
              Some("queryName"),
              Vector(
                VariableDefinition(
                  "foo",
                  NamedType("ComplexType", Some(Position(381, 9, 33))),
                  None,
                  Vector.empty,
                  Some(Position(375, 9, 27))
                ),
                VariableDefinition(
                  "site",
                  NamedType("Site", Some(Position(401, 9, 53))),
                  Some(EnumValue("MOBILE", Vector.empty, Some(Position(408, 9, 60)))),
                  Vector.empty,
                  Some(Position(394, 9, 46))
                )),
              Vector.empty,
              Vector(
                Field(
                  Some("whoever123is"),
                  "node",
                  Vector(
                    Argument(
                      "id",
                      ListValue(
                        Vector(
                          BigIntValue(123, Vector.empty, Some(Position(454, 10, 37))),
                          BigIntValue(456, Vector.empty, Some(Position(459, 10, 42)))),
                        Vector.empty,
                        Some(Position(453, 10, 36))
                      ),
                      Vector.empty,
                      Some(Position(449, 10, 32))
                    )),
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "id",
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Vector.empty,
                      Some(Position(481, 11, 15))
                    ),
                    InlineFragment(
                      Some(NamedType("User", Some(Position(507, 12, 22)))),
                      Vector(
                        Directive(
                          "defer",
                          Vector.empty,
                          Vector.empty,
                          Some(Position(512, 12, 27))
                        )),
                      Vector(
                        Field(
                          None,
                          "field2",
                          Vector.empty,
                          Vector.empty,
                          Vector(
                            Field(
                              None,
                              "id",
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Some(Position(564, 14, 19))
                            ),
                            Field(
                              Some("alias"),
                              "field1",
                              Vector(
                                Argument(
                                  "first",
                                  BigIntValue(10, Vector.empty, Some(Position(607, 15, 39))),
                                  Vector.empty,
                                  Some(Position(601, 15, 33))
                                ),
                                Argument(
                                  "after",
                                  VariableValue("foo", Vector.empty, Some(Position(617, 15, 49))),
                                  Vector.empty,
                                  Some(Position(611, 15, 43))
                                )),
                              Vector(
                                Directive(
                                  "include",
                                  Vector(
                                    Argument(
                                      "if",
                                      VariableValue("foo", Vector.empty, Some(Position(637, 15, 69))),
                                      Vector.empty,
                                      Some(Position(633, 15, 65))
                                    )),
                                  Vector.empty,
                                  Some(Position(624, 15, 56))
                                )),
                              Vector(
                                Field(
                                  None,
                                  "id",
                                  Vector.empty,
                                  Vector.empty,
                                  Vector.empty,
                                  Vector.empty,
                                  Vector.empty,
                                  Some(Position(665, 16, 21))
                                ),
                                FragmentSpread("frag", Vector.empty, Vector.empty, Some(Position(689, 17, 21)))),
                              Vector.empty,
                              Vector.empty,
                              Some(Position(587, 15, 19))
                            )),
                          Vector.empty,
                          Vector.empty,
                          Some(Position(537, 13, 17))
                        )),
                      Vector.empty,
                      Vector.empty,
                      Some(Position(500, 12, 15))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Some(Position(430, 10, 13))
                )),
              Vector(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(Position(11, 2, 11))),
                Comment(" All rights reserved.", Some(Position(58, 3, 11))),
                Comment("", Some(Position(91, 4, 11))),
                Comment(" This source code is licensed under the BSD-style license found in the", Some(Position(103, 5, 11))),
                Comment(" LICENSE file in the root directory of this source tree. An additional grant", Some(Position(185, 6, 11))),
                Comment(" of patent rights can be found in the PATENTS file in the same directory.", Some(Position(273, 7, 11)))),
              Vector.empty,
              Some(Position(359, 9, 11))
            ),
            OperationDefinition(
              OperationType.Subscription,
              Some("StoryLikeSubscription"),
              Vector(
                VariableDefinition(
                  "input",
                  NamedType("StoryLikeSubscribeInput", Some(Position(831, 24, 54))),
                  None,
                  Vector.empty,
                  Some(Position(823, 24, 46))
                )),
              Vector.empty,
              Vector(
                Field(
                  None,
                  "storyLikeSubscribe",
                  Vector(
                    Argument(
                      "input",
                      VariableValue("input", Vector.empty, Some(Position(896, 25, 39))),
                      Vector.empty,
                      Some(Position(889, 25, 32))
                    )),
                  Vector.empty,
                  Vector(
                    Field(
                      None,
                      "story",
                      Vector.empty,
                      Vector.empty,
                      Vector(
                        Field(
                          None,
                          "likers",
                          Vector.empty,
                          Vector.empty,
                          Vector(
                            Field(
                              None,
                              "count",
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Some(Position(971, 28, 19))
                            )),
                          Vector.empty,
                          Vector.empty,
                          Some(Position(944, 27, 17))
                        ),
                        Field(
                          None,
                          "likeSentence",
                          Vector.empty,
                          Vector.empty,
                          Vector(
                            Field(
                              None,
                              "text",
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Vector.empty,
                              Some(Position(1044, 31, 19))
                            )),
                          Vector.empty,
                          Vector.empty,
                          Some(Position(1011, 30, 17))
                        )),
                      Vector.empty,
                      Vector.empty,
                      Some(Position(920, 26, 15))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Some(Position(870, 25, 13))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(788, 24, 11))
            ),
            OperationDefinition(
              OperationType.Mutation,
              Some("likeStory"),
              Vector.empty,
              Vector.empty,
              Vector(
                Field(
                  None,
                  "like",
                  Vector(
                    Argument(
                      "story",
                      BigIntValue(123, Vector.empty, Some(Position(1165, 38, 25))),
                      Vector.empty,
                      Some(Position(1158, 38, 18))
                    )),
                  Vector(
                    Directive(
                      "defer",
                      Vector.empty,
                      Vector.empty,
                      Some(Position(1170, 38, 30))
                    )),
                  Vector(
                    Field(
                      None,
                      "story",
                      Vector.empty,
                      Vector.empty,
                      Vector(
                        Field(
                          None,
                          "id",
                          Vector.empty,
                          Vector.empty,
                          Vector.empty,
                          Vector.empty,
                          Vector.empty,
                          Some(Position(1217, 40, 17))
                        )),
                      Vector.empty,
                      Vector.empty,
                      Some(Position(1193, 39, 15))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1153, 38, 13))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(1120, 37, 11))
            ),
            FragmentDefinition(
              "frag",
              NamedType("Friend", Some(Position(1290, 45, 28))),
              Vector.empty,
              Vector(
                Field(
                  None,
                  "foo",
                  Vector(
                    Argument(
                      "size",
                      VariableValue("size", Vector.empty, Some(Position(1321, 46, 23))),
                      Vector.empty,
                      Some(Position(1315, 46, 17))
                    ),
                    Argument(
                      "bar",
                      VariableValue("b", Vector.empty, Some(Position(1333, 46, 35))),
                      Vector.empty,
                      Some(Position(1328, 46, 30))
                    ),
                    Argument(
                      "obj",
                      ObjectValue(
                        Vector(
                          ObjectField(
                            "key",
                            StringValue("value", false, None, Vector.empty, Some(Position(1348, 46, 50))),
                            Vector.empty,
                            Some(Position(1343, 46, 45))
                          )),
                        Vector.empty,
                        Some(Position(1342, 46, 44))
                      ),
                      Vector.empty,
                      Some(Position(1337, 46, 39))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1311, 46, 13))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(1273, 45, 11))
            ),
            OperationDefinition(
              OperationType.Query,
              None,
              Vector.empty,
              Vector.empty,
              Vector(
                Field(
                  None,
                  "unnamed",
                  Vector(
                    Argument(
                      "truthy",
                      BooleanValue(true, Vector.empty, Some(Position(1411, 50, 29))),
                      Vector.empty,
                      Some(Position(1403, 50, 21))
                    ),
                    Argument(
                      "falsey",
                      BooleanValue(false, Vector.empty, Some(Position(1425, 50, 43))),
                      Vector.empty,
                      Some(Position(1417, 50, 35))
                    )),
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1395, 50, 13))
                ),
                Field(
                  None,
                  "query",
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1445, 51, 13))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(1381, 49, 11))
            )),
          Vector.empty,
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

          "Type description *test*"
          type Foo implements Bar {
            one: Type
            "another description"
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

          extend type Foo @onType {
            foo: String
          }

          directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

          directive @include(if: Boolean!)
            on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT

        """

      ast.sourceMapper should not be 'empty

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            SchemaDefinition(
              Vector(
                OperationTypeDefinition(OperationType.Query, NamedType("QueryType", Some(Position(387, 10, 20))), Vector.empty, Some(Position(380, 10, 13))),
                OperationTypeDefinition(OperationType.Mutation, NamedType("MutationType", Some(Position(419, 11, 23))), Vector.empty, Some(Position(409, 11, 13)))),
              Vector.empty,
              Vector(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(Position(11, 2, 11))),
                Comment(" All rights reserved.", Some(Position(58, 3, 11))),
                Comment("", Some(Position(91, 4, 11))),
                Comment(" This source code is licensed under the BSD-style license found in the", Some(Position(103, 5, 11))),
                Comment(" LICENSE file in the root directory of this source tree. An additional grant", Some(Position(185, 6, 11))),
                Comment(" of patent rights can be found in the PATENTS file in the same directory.", Some(Position(273, 7, 11)))),
              Vector.empty,
              Some(Position(359, 9, 11))
            ),
            ObjectTypeDefinition(
              "Foo",
              Vector(
                NamedType("Bar", Some(Position(511, 15, 31)))),
              Vector(
                FieldDefinition("one", NamedType("Type", Some(Position(534, 16, 18))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(529, 16, 13))),
                FieldDefinition("two", NamedType("Type", Some(Position(612, 18, 40))), Vector(InputValueDefinition("argument", NotNullType(NamedType("InputType", Some(Position(599, 18, 27))), Some(Position(599, 18, 27))), None, Vector.empty, None, Vector.empty, Some(Position(589, 18, 17)))), Vector.empty, Some(StringValue("another description", false, None, Vector.empty, Some(Position(551, 17, 13)))), Vector.empty, Some(Position(585, 18, 13))),
                FieldDefinition("three", NamedType("Int", Some(Position(672, 19, 56))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(Position(645, 19, 29))), None, Vector.empty, None, Vector.empty, Some(Position(635, 19, 19))), InputValueDefinition("other", NamedType("String", Some(Position(663, 19, 47))), None, Vector.empty, None, Vector.empty, Some(Position(656, 19, 40)))), Vector.empty, None, Vector.empty, Some(Position(629, 19, 13))),
                FieldDefinition("four", NamedType("String", Some(Position(723, 20, 48))), Vector(InputValueDefinition("argument", NamedType("String", Some(Position(703, 20, 28))), Some(StringValue("string", false, None, Vector.empty, Some(Position(712, 20, 37)))), Vector.empty, None, Vector.empty, Some(Position(693, 20, 18)))), Vector.empty, None, Vector.empty, Some(Position(688, 20, 13))),
                FieldDefinition("five", NamedType("String", Some(Position(791, 21, 62))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(758, 21, 29))), Some(Position(757, 21, 28))), Some(ListValue(
                  Vector(
                    StringValue("string", false, None, Vector.empty, Some(Position(769, 21, 40))),
                    StringValue("string", false, None, Vector.empty, Some(Position(779, 21, 50)))),
                  Vector.empty,
                  Some(Position(768, 21, 39))
                )), Vector.empty, None, Vector.empty, Some(Position(747, 21, 18)))), Vector.empty, None, Vector.empty, Some(Position(742, 21, 13))),
                FieldDefinition("six", NamedType("Type", Some(Position(853, 22, 56))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(Position(824, 22, 27))), Some(ObjectValue(
                  Vector(
                    ObjectField(
                      "key",
                      StringValue("value", false, None, Vector.empty, Some(Position(842, 22, 45))),
                      Vector.empty,
                      Some(Position(837, 22, 40))
                    )),
                  Vector.empty,
                  Some(Position(836, 22, 39))
                )), Vector.empty, None, Vector.empty, Some(Position(814, 22, 17)))), Vector.empty, None, Vector.empty, Some(Position(810, 22, 13)))),
              Vector.empty,
              Some(StringValue("Type description *test*", false, None, Vector.empty, Some(Position(455, 14, 11)))),
              Vector.empty,
              Vector.empty,
              Some(Position(491, 15, 11))
            ),
            ObjectTypeDefinition(
              "AnnotatedObject",
              Vector.empty,
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(986, 26, 59))), Vector(InputValueDefinition("arg", NamedType("Type", Some(Position(960, 26, 33))), Some(StringValue("default", false, None, Vector.empty, Some(Position(967, 26, 40)))), Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(977, 26, 50))
                )), None, Vector.empty, Some(Position(955, 26, 28)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(991, 26, 64))
                )), None, Vector.empty, Some(Position(940, 26, 13)))),
              Vector(
                Directive(
                  "onObject",
                  Vector(
                    Argument(
                      "arg",
                      StringValue("value", false, None, Vector.empty, Some(Position(917, 25, 47))),
                      Vector.empty,
                      Some(Position(912, 25, 42))
                    )),
                  Vector.empty,
                  Some(Position(902, 25, 32))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(881, 25, 11))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector(
                FieldDefinition("one", NamedType("Type", Some(Position(1087, 31, 18))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(1082, 31, 13))),
                FieldDefinition("four", NamedType("String", Some(Position(1139, 32, 48))), Vector(InputValueDefinition("argument", NamedType("String", Some(Position(1119, 32, 28))), Some(StringValue("string", false, None, Vector.empty, Some(Position(1128, 32, 37)))), Vector.empty, None, Vector.empty, Some(Position(1109, 32, 18)))), Vector.empty, None, Vector.empty, Some(Position(1104, 32, 13)))),
              Vector.empty,
              None,
              Vector(
                Comment(" It's an interface!", Some(Position(1023, 29, 11)))),
              Vector.empty,
              Some(Position(1054, 30, 11))
            ),
            InterfaceTypeDefinition(
              "AnnotatedInterface",
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(1259, 36, 47))), Vector(InputValueDefinition("arg", NamedType("Type", Some(Position(1245, 36, 33))), None, Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1250, 36, 38))
                )), None, Vector.empty, Some(Position(1240, 36, 28)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1264, 36, 52))
                )), None, Vector.empty, Some(Position(1225, 36, 13)))),
              Vector(
                Directive(
                  "onInterface",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1198, 35, 40))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1169, 35, 11))
            ),
            UnionTypeDefinition(
              "Feed",
              Vector(
                NamedType("Story", Some(Position(1309, 39, 24))),
                NamedType("Article", Some(Position(1317, 39, 32))),
                NamedType("Advert", Some(Position(1327, 39, 42)))),
              Vector.empty,
              None,
              Vector.empty,
              Some(Position(1296, 39, 11))
            ),
            UnionTypeDefinition(
              "AnnotatedUnion",
              Vector(
                NamedType("A", Some(Position(1377, 41, 43))),
                NamedType("B", Some(Position(1381, 41, 47)))),
              Vector(
                Directive(
                  "onUnion",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1366, 41, 32))
                )),
              None,
              Vector.empty,
              Some(Position(1345, 41, 11))
            ),
            ScalarTypeDefinition(
              "CustomScalar",
              Vector.empty,
              None,
              Vector.empty,
              Some(Position(1394, 43, 11))
            ),
            ScalarTypeDefinition(
              "AnnotatedScalar",
              Vector(
                Directive(
                  "onScalar",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1448, 45, 34))
                )),
              None,
              Vector.empty,
              Some(Position(1425, 45, 11))
            ),
            EnumTypeDefinition(
              "Site",
              Vector(
                EnumValueDefinition("DESKTOP", Vector.empty, None, Vector(Comment(" value 1", Some(Position(1493, 48, 13)))), Some(Position(1515, 49, 13))),
                EnumValueDefinition("MOBILE", Vector.empty, None, Vector(Comment(" value 2", Some(Position(1535, 50, 13)))), Some(Position(1557, 51, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1469, 47, 11))
            ),
            EnumTypeDefinition(
              "AnnotatedEnum",
              Vector(
                EnumValueDefinition("ANNOTATED_VALUE", Vector(Directive(
                  "onEnumValue",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1644, 55, 29))
                )), None, Vector.empty, Some(Position(1628, 55, 13))),
                EnumValueDefinition("OTHER_VALUE", Vector.empty, None, Vector.empty, Some(Position(1669, 56, 13)))),
              Vector(
                Directive(
                  "onEnum",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1606, 54, 30))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1587, 54, 11))
            ),
            InputObjectTypeDefinition(
              "InputType",
              Vector(
                InputValueDefinition("key", NotNullType(NamedType("String", Some(Position(1739, 60, 18))), Some(Position(1739, 60, 18))), None, Vector.empty, None, Vector.empty, Some(Position(1734, 60, 13))),
                InputValueDefinition("answer", NamedType("Int", Some(Position(1767, 61, 21))), Some(BigIntValue(42, Vector.empty, Some(Position(1773, 61, 27)))), Vector.empty, None, Vector.empty, Some(Position(1759, 61, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1704, 59, 11))
            ),
            InputObjectTypeDefinition(
              "AnnotatedInput",
              Vector(
                InputValueDefinition("annotatedField", NamedType("Type", Some(Position(1897, 66, 29))), None, Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1902, 66, 34))
                )), None, Vector(Comment(" field comment", Some(Position(1853, 65, 13)))), Some(Position(1881, 66, 13)))),
              Vector(
                Directive(
                  "onInputObjectType",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1820, 64, 32))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1799, 64, 11))
            ),
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Foo",
                Vector.empty,
                Vector(
                  FieldDefinition("seven", NamedType("Type", Some(Position(1991, 70, 40))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(1981, 70, 30))), Some(Position(1980, 70, 29))), None, Vector.empty, None, Vector.empty, Some(Position(1970, 70, 19)))), Vector.empty, None, Vector.empty, Some(Position(1964, 70, 13)))),
                Vector.empty,
                None,
                Vector.empty,
                Vector.empty,
                Some(Position(1934, 69, 11))
              ),
              Vector.empty,
              Some(Position(1934, 69, 11))
            ),
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Foo",
                Vector.empty,
                Vector(
                  FieldDefinition("foo", NamedType("String", Some(Position(2062, 74, 18))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(2057, 74, 13)))),
                Vector(
                  Directive(
                    "onType",
                    Vector.empty,
                    Vector.empty,
                    Some(Position(2035, 73, 27))
                  )),
                None,
                Vector.empty,
                Vector.empty,
                Some(Position(2019, 73, 11))
              ),
              Vector.empty,
              Some(Position(2019, 73, 11))
            ),
            DirectiveDefinition(
              "skip",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(2112, 77, 31))), Some(Position(2112, 77, 31))), None, Vector.empty, None, Vector.empty, Some(Position(2108, 77, 27)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(Position(2125, 77, 44))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(Position(2133, 77, 52))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(Position(2151, 77, 70)))),
              None,
              Vector.empty,
              Some(Position(2092, 77, 11))
            ),
            DirectiveDefinition(
              "include",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(2201, 79, 34))), Some(Position(2201, 79, 34))), None, Vector.empty, None, Vector.empty, Some(Position(2197, 79, 30)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(Position(2226, 80, 16))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(Position(2234, 80, 24))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(Position(2252, 80, 42)))),
              None,
              Vector.empty,
              Some(Position(2178, 79, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
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
          Vector(
            ObjectField(
              "a",
              NullValue(Vector.empty, Some(Position(24, 3, 14))),
              Vector.empty,
              Some(Position(21, 3, 11))
            ),
            ObjectField(
              "b",
              BigIntValue(1, Vector.empty, Some(Position(68, 6, 14))),
              Vector(
                Comment(" test comment", Some(Position(40, 5, 11)))),
              Some(Position(65, 6, 11))
            ),
            ObjectField(
              "c",
              ObjectValue(
                Vector(
                  ObjectField(
                    "someNull",
                    NullValue(Vector.empty, Some(Position(107, 8, 23))),
                    Vector.empty,
                    Some(Position(97, 8, 13))
                  ),
                  ObjectField(
                    "enum",
                    EnumValue("HELLO", Vector.empty, Some(Position(130, 9, 19))),
                    Vector.empty,
                    Some(Position(124, 9, 13))
                  )),
                Vector.empty,
                Some(Position(83, 7, 14))
              ),
              Vector.empty,
              Some(Position(80, 7, 11))
            )),
          Vector.empty,
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
