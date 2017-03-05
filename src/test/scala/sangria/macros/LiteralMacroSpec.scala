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
                      StringValue("1000", Vector.empty, Some(Position(176, 4, 29))),
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
                      StringValue("10103\n \u00F6 \u00F6", Vector.empty, Some(Position(284, 7, 34))),
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
                            StringValue("value", Vector.empty, Some(Position(1348, 46, 50))),
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
                NamedType("Bar", Some(Position(475, 14, 31)))),
              Vector(
                FieldDefinition("one", NamedType("Type", Some(Position(498, 15, 18))), Vector.empty, Vector.empty, Vector.empty, Some(Position(493, 15, 13))),
                FieldDefinition("two", NamedType("Type", Some(Position(542, 16, 40))), Vector(InputValueDefinition("argument", NotNullType(NamedType("InputType", Some(Position(529, 16, 27))), Some(Position(529, 16, 27))), None, Vector.empty, Vector.empty, Some(Position(519, 16, 17)))), Vector.empty, Vector.empty, Some(Position(515, 16, 13))),
                FieldDefinition("three", NamedType("Int", Some(Position(602, 17, 56))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(Position(575, 17, 29))), None, Vector.empty, Vector.empty, Some(Position(565, 17, 19))), InputValueDefinition("other", NamedType("String", Some(Position(593, 17, 47))), None, Vector.empty, Vector.empty, Some(Position(586, 17, 40)))), Vector.empty, Vector.empty, Some(Position(559, 17, 13))),
                FieldDefinition("four", NamedType("String", Some(Position(653, 18, 48))), Vector(InputValueDefinition("argument", NamedType("String", Some(Position(633, 18, 28))), Some(StringValue("string", Vector.empty, Some(Position(642, 18, 37)))), Vector.empty, Vector.empty, Some(Position(623, 18, 18)))), Vector.empty, Vector.empty, Some(Position(618, 18, 13))),
                FieldDefinition("five", NamedType("String", Some(Position(721, 19, 62))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(688, 19, 29))), Some(Position(687, 19, 28))), Some(ListValue(
                  Vector(
                    StringValue("string", Vector.empty, Some(Position(699, 19, 40))),
                    StringValue("string", Vector.empty, Some(Position(709, 19, 50)))),
                  Vector.empty,
                  Some(Position(698, 19, 39))
                )), Vector.empty, Vector.empty, Some(Position(677, 19, 18)))), Vector.empty, Vector.empty, Some(Position(672, 19, 13))),
                FieldDefinition("six", NamedType("Type", Some(Position(783, 20, 56))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(Position(754, 20, 27))), Some(ObjectValue(
                  Vector(
                    ObjectField(
                      "key",
                      StringValue("value", Vector.empty, Some(Position(772, 20, 45))),
                      Vector.empty,
                      Some(Position(767, 20, 40))
                    )),
                  Vector.empty,
                  Some(Position(766, 20, 39))
                )), Vector.empty, Vector.empty, Some(Position(744, 20, 17)))), Vector.empty, Vector.empty, Some(Position(740, 20, 13)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(Position(455, 14, 11))
            ),
            ObjectTypeDefinition(
              "AnnotatedObject",
              Vector.empty,
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(916, 24, 59))), Vector(InputValueDefinition("arg", NamedType("Type", Some(Position(890, 24, 33))), Some(StringValue("default", Vector.empty, Some(Position(897, 24, 40)))), Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(907, 24, 50))
                )), Vector.empty, Some(Position(885, 24, 28)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(921, 24, 64))
                )), Vector.empty, Some(Position(870, 24, 13)))),
              Vector(
                Directive(
                  "onObject",
                  Vector(
                    Argument(
                      "arg",
                      StringValue("value", Vector.empty, Some(Position(847, 23, 47))),
                      Vector.empty,
                      Some(Position(842, 23, 42))
                    )),
                  Vector.empty,
                  Some(Position(832, 23, 32))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(811, 23, 11))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector(
                FieldDefinition("one", NamedType("Type", Some(Position(1017, 29, 18))), Vector.empty, Vector.empty, Vector.empty, Some(Position(1012, 29, 13))),
                FieldDefinition("four", NamedType("String", Some(Position(1069, 30, 48))), Vector(InputValueDefinition("argument", NamedType("String", Some(Position(1049, 30, 28))), Some(StringValue("string", Vector.empty, Some(Position(1058, 30, 37)))), Vector.empty, Vector.empty, Some(Position(1039, 30, 18)))), Vector.empty, Vector.empty, Some(Position(1034, 30, 13)))),
              Vector.empty,
              Vector(
                Comment(" It's an interface!", Some(Position(953, 27, 11)))),
              Vector.empty,
              Some(Position(984, 28, 11))
            ),
            InterfaceTypeDefinition(
              "AnnotatedInterface",
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(1189, 34, 47))), Vector(InputValueDefinition("arg", NamedType("Type", Some(Position(1175, 34, 33))), None, Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1180, 34, 38))
                )), Vector.empty, Some(Position(1170, 34, 28)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1194, 34, 52))
                )), Vector.empty, Some(Position(1155, 34, 13)))),
              Vector(
                Directive(
                  "onInterface",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1128, 33, 40))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(1099, 33, 11))
            ),
            UnionTypeDefinition(
              "Feed",
              Vector(
                NamedType("Story", Some(Position(1239, 37, 24))),
                NamedType("Article", Some(Position(1247, 37, 32))),
                NamedType("Advert", Some(Position(1257, 37, 42)))),
              Vector.empty,
              Vector.empty,
              Some(Position(1226, 37, 11))
            ),
            UnionTypeDefinition(
              "AnnotatedUnion",
              Vector(
                NamedType("A", Some(Position(1307, 39, 43))),
                NamedType("B", Some(Position(1311, 39, 47)))),
              Vector(
                Directive(
                  "onUnion",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1296, 39, 32))
                )),
              Vector.empty,
              Some(Position(1275, 39, 11))
            ),
            ScalarTypeDefinition(
              "CustomScalar",
              Vector.empty,
              Vector.empty,
              Some(Position(1324, 41, 11))
            ),
            ScalarTypeDefinition(
              "AnnotatedScalar",
              Vector(
                Directive(
                  "onScalar",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1378, 43, 34))
                )),
              Vector.empty,
              Some(Position(1355, 43, 11))
            ),
            EnumTypeDefinition(
              "Site",
              Vector(
                EnumValueDefinition("DESKTOP", Vector.empty, Vector(Comment(" value 1", Some(Position(1423, 46, 13)))), Some(Position(1445, 47, 13))),
                EnumValueDefinition("MOBILE", Vector.empty, Vector(Comment(" value 2", Some(Position(1465, 48, 13)))), Some(Position(1487, 49, 13)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(Position(1399, 45, 11))
            ),
            EnumTypeDefinition(
              "AnnotatedEnum",
              Vector(
                EnumValueDefinition("ANNOTATED_VALUE", Vector(Directive(
                  "onEnumValue",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1574, 53, 29))
                )), Vector.empty, Some(Position(1558, 53, 13))),
                EnumValueDefinition("OTHER_VALUE", Vector.empty, Vector.empty, Some(Position(1599, 54, 13)))),
              Vector(
                Directive(
                  "onEnum",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1536, 52, 30))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(1517, 52, 11))
            ),
            InputObjectTypeDefinition(
              "InputType",
              Vector(
                InputValueDefinition("key", NotNullType(NamedType("String", Some(Position(1669, 58, 18))), Some(Position(1669, 58, 18))), None, Vector.empty, Vector.empty, Some(Position(1664, 58, 13))),
                InputValueDefinition("answer", NamedType("Int", Some(Position(1697, 59, 21))), Some(BigIntValue(42, Vector.empty, Some(Position(1703, 59, 27)))), Vector.empty, Vector.empty, Some(Position(1689, 59, 13)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(Position(1634, 57, 11))
            ),
            InputObjectTypeDefinition(
              "AnnotatedInput",
              Vector(
                InputValueDefinition("annotatedField", NamedType("Type", Some(Position(1827, 64, 29))), None, Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1832, 64, 34))
                )), Vector(Comment(" field comment", Some(Position(1783, 63, 13)))), Some(Position(1811, 64, 13)))),
              Vector(
                Directive(
                  "onInputObjectType",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1750, 62, 32))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(1729, 62, 11))
            ),
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Foo",
                Vector.empty,
                Vector(
                  FieldDefinition("seven", NamedType("Type", Some(Position(1921, 68, 40))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(1911, 68, 30))), Some(Position(1910, 68, 29))), None, Vector.empty, Vector.empty, Some(Position(1900, 68, 19)))), Vector.empty, Vector.empty, Some(Position(1894, 68, 13)))),
                Vector.empty,
                Vector.empty,
                Vector.empty,
                Some(Position(1871, 67, 18))
              ),
              Vector.empty,
              Some(Position(1864, 67, 11))
            ),
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Foo",
                Vector.empty,
                Vector.empty,
                Vector(
                  Directive(
                    "onType",
                    Vector.empty,
                    Vector.empty,
                    Some(Position(1965, 71, 27))
                  )),
                Vector.empty,
                Vector.empty,
                Some(Position(1956, 71, 18))
              ),
              Vector.empty,
              Some(Position(1949, 71, 11))
            ),
            ObjectTypeDefinition(
              "NoFields",
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(Position(1987, 73, 11))
            ),
            DirectiveDefinition(
              "skip",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(2035, 75, 31))), Some(Position(2035, 75, 31))), None, Vector.empty, Vector.empty, Some(Position(2031, 75, 27)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(Position(2048, 75, 44))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(Position(2056, 75, 52))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(Position(2074, 75, 70)))),
              Vector.empty,
              Some(Position(2015, 75, 11))
            ),
            DirectiveDefinition(
              "include",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(2124, 77, 34))), Some(Position(2124, 77, 34))), None, Vector.empty, Vector.empty, Some(Position(2120, 77, 30)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(Position(2149, 78, 16))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(Position(2157, 78, 24))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(Position(2175, 78, 42)))),
              Vector.empty,
              Some(Position(2101, 77, 11))
            )),
          Vector.empty,
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
