package sangria.parser

import sangria.ast.AstLocation
import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.ast._
import sangria.util.{DebugUtil, FileUtil, StringMatchers}

import scala.util.Success

class SchemaParserSpec extends WordSpec with Matchers with StringMatchers {

  def parseQuery(query: String)(implicit scheme: DeliveryScheme[ast.Document]): scheme.Result =
    QueryParser.parse(query, ParserConfig.default.withEmptySourceId.withEmptySourceMapper)(scheme)

  "QueryParser" should {
    "parse schema kitchen sink" in {
      val query = FileUtil loadQuery "schema-kitchen-sink.graphql"

      val expectedAst =
        Document(
          Vector(
            SchemaDefinition(
              Vector(
                OperationTypeDefinition(OperationType.Query, NamedType("QueryType", Some(AstLocation(306, 9, 10))), Vector.empty, Some(AstLocation(299, 9, 3))),
                OperationTypeDefinition(OperationType.Mutation, NamedType("MutationType", Some(AstLocation(328, 10, 13))), Vector.empty, Some(AstLocation(318, 10, 3)))),
              Vector.empty,
              Vector(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(AstLocation(0, 1, 1))),
                Comment(" All rights reserved.", Some(AstLocation(37, 2, 1))),
                Comment("", Some(AstLocation(60, 3, 1))),
                Comment(" This source code is licensed under the BSD-style license found in the", Some(AstLocation(62, 4, 1))),
                Comment(" LICENSE file in the root directory of this source tree. An additional grant", Some(AstLocation(134, 5, 1))),
                Comment(" of patent rights can be found in the PATENTS file in the same directory.", Some(AstLocation(212, 6, 1)))),
              Vector.empty,
              Some(AstLocation(288, 8, 1))
            ),
            ObjectTypeDefinition(
              "Foo",
              Vector(
                NamedType("Bar", Some(AstLocation(390, 14, 21)))),
              Vector(
                FieldDefinition("one", NamedType("Type", Some(AstLocation(403, 15, 8))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(398, 15, 3))),
                FieldDefinition("two", NamedType("Type", Some(AstLocation(437, 16, 30))), Vector(InputValueDefinition("argument", NotNullType(NamedType("InputType", Some(AstLocation(424, 16, 17))), Some(AstLocation(424, 16, 17))), None, Vector.empty, None, Vector.empty, Some(AstLocation(414, 16, 7)))), Vector.empty, None, Vector.empty, Some(AstLocation(410, 16, 3))),
                FieldDefinition("three", NamedType("Int", Some(AstLocation(487, 17, 46))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(AstLocation(460, 17, 19))), None, Vector.empty, None, Vector.empty, Some(AstLocation(450, 17, 9))), InputValueDefinition("other", NamedType("String", Some(AstLocation(478, 17, 37))), None, Vector.empty, None, Vector.empty, Some(AstLocation(471, 17, 30)))), Vector.empty, None, Vector.empty, Some(AstLocation(444, 17, 3))),
                FieldDefinition("four", NamedType("String", Some(AstLocation(528, 18, 38))), Vector(InputValueDefinition("argument", NamedType("String", Some(AstLocation(508, 18, 18))), Some(StringValue("string", false, None, Vector.empty, Some(AstLocation(517, 18, 27)))), Vector.empty, None, Vector.empty, Some(AstLocation(498, 18, 8)))), Vector.empty, None, Vector.empty, Some(AstLocation(493, 18, 3))),
                FieldDefinition("five", NamedType("String", Some(AstLocation(586, 19, 52))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(AstLocation(553, 19, 19))), Some(AstLocation(552, 19, 18))), Some(ListValue(
                  Vector(
                    StringValue("string", false, None, Vector.empty, Some(AstLocation(564, 19, 30))),
                    StringValue("string", false, None, Vector.empty, Some(AstLocation(574, 19, 40)))),
                  Vector.empty,
                  Some(AstLocation(563, 19, 29))
                )), Vector.empty, None, Vector.empty, Some(AstLocation(542, 19, 8)))), Vector.empty, None, Vector.empty, Some(AstLocation(537, 19, 3))),
                FieldDefinition("six", NamedType("Type", Some(AstLocation(678, 22, 46))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(AstLocation(649, 22, 17))), Some(ObjectValue(
                  Vector(
                    ObjectField(
                      "key",
                      StringValue("value", false, None, Vector.empty, Some(AstLocation(667, 22, 35))),
                      Vector.empty,
                      Some(AstLocation(662, 22, 30))
                    )),
                  Vector.empty,
                  Some(AstLocation(661, 22, 29))
                )), Vector.empty, None, Vector.empty, Some(AstLocation(639, 22, 7)))), Vector.empty, Some(StringValue("More \"\"\" descriptions \\", true, Some("\n  More \"\"\" descriptions \\\n  "), Vector.empty, Some(AstLocation(596, 21, 3)))), Vector.empty, Some(AstLocation(635, 22, 3)))),
              Vector.empty,
              Some(StringValue("type description!", true, Some("\ntype description!\n"), Vector.empty, Some(AstLocation(344, 13, 1)))),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(370, 14, 1))
            ),
            ObjectTypeDefinition(
              "AnnotatedObject",
              Vector.empty,
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(AstLocation(781, 26, 49))), Vector(InputValueDefinition("arg", NamedType("Type", Some(AstLocation(755, 26, 23))), Some(StringValue("default", false, None, Vector.empty, Some(AstLocation(762, 26, 30)))), Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(772, 26, 40))
                )), None, Vector.empty, Some(AstLocation(750, 26, 18)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(786, 26, 54))
                )), None, Vector.empty, Some(AstLocation(735, 26, 3)))),
              Vector(
                Directive(
                  "onObject",
                  Vector(
                    Argument(
                      "arg",
                      StringValue("value", false, None, Vector.empty, Some(AstLocation(722, 25, 37))),
                      Vector.empty,
                      Some(AstLocation(717, 25, 32))
                    )),
                  Vector.empty,
                  Some(AstLocation(707, 25, 22))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(686, 25, 1))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector(
                FieldDefinition("one", NamedType("Type", Some(AstLocation(875, 33, 8))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(870, 33, 3))),
                FieldDefinition("four", NamedType("String", Some(AstLocation(917, 34, 38))), Vector(InputValueDefinition("argument", NamedType("String", Some(AstLocation(897, 34, 18))), Some(StringValue("string", false, None, Vector.empty, Some(AstLocation(906, 34, 27)))), Vector.empty, None, Vector.empty, Some(AstLocation(887, 34, 8)))), Vector.empty, None, Vector.empty, Some(AstLocation(882, 34, 3)))),
              Vector.empty,
              Some(StringValue(" It's an interface!", false, None, Vector(Comment(" comment above", Some(AstLocation(798, 29, 1)))), Some(AstLocation(814, 30, 1)))),
              Vector(
                Comment(" comment below", Some(AstLocation(836, 31, 1)))),
              Vector.empty,
              Some(AstLocation(852, 32, 1))
            ),
            InterfaceTypeDefinition(
              "AnnotatedInterface",
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(AstLocation(1007, 38, 37))), Vector(InputValueDefinition("arg", NamedType("Type", Some(AstLocation(993, 38, 23))), None, Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(998, 38, 28))
                )), None, Vector.empty, Some(AstLocation(988, 38, 18)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1012, 38, 42))
                )), None, Vector.empty, Some(AstLocation(973, 38, 3)))),
              Vector(
                Directive(
                  "onInterface",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(956, 37, 30))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(927, 37, 1))
            ),
            UnionTypeDefinition(
              "Feed",
              Vector(
                NamedType("Story", Some(AstLocation(1037, 41, 14))),
                NamedType("Article", Some(AstLocation(1045, 41, 22))),
                NamedType("Advert", Some(AstLocation(1055, 41, 32)))),
              Vector.empty,
              None,
              Vector.empty,
              Some(AstLocation(1024, 41, 1))
            ),
            UnionTypeDefinition(
              "AnnotatedUnion",
              Vector(
                NamedType("A", Some(AstLocation(1095, 43, 33))),
                NamedType("B", Some(AstLocation(1099, 43, 37)))),
              Vector(
                Directive(
                  "onUnion",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1084, 43, 22))
                )),
              None,
              Vector.empty,
              Some(AstLocation(1063, 43, 1))
            ),
            ScalarTypeDefinition(
              "CustomScalar",
              Vector.empty,
              None,
              Vector.empty,
              Some(AstLocation(1102, 45, 1))
            ),
            ScalarTypeDefinition(
              "AnnotatedScalar",
              Vector(
                Directive(
                  "onScalar",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1146, 47, 24))
                )),
              None,
              Vector.empty,
              Some(AstLocation(1123, 47, 1))
            ),
            EnumTypeDefinition(
              "Site",
              Vector(
                EnumValueDefinition("DESKTOP", Vector.empty, Some(StringValue("description 1", false, None, Vector.empty, Some(AstLocation(1171, 50, 3)))), Vector.empty, Some(AstLocation(1189, 51, 3))),
                EnumValueDefinition("MOBILE", Vector.empty, Some(StringValue("description 2", true, Some("\n  description 2\n  "), Vector.empty, Some(AstLocation(1199, 52, 3)))), Vector.empty, Some(AstLocation(1227, 53, 3)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1157, 49, 1))
            ),
            EnumTypeDefinition(
              "AnnotatedEnum",
              Vector(
                EnumValueDefinition("ANNOTATED_VALUE", Vector(Directive(
                  "onEnumValue",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1284, 57, 19))
                )), None, Vector.empty, Some(AstLocation(1268, 57, 3))),
                EnumValueDefinition("OTHER_VALUE", Vector.empty, None, Vector.empty, Some(AstLocation(1299, 58, 3)))),
              Vector(
                Directive(
                  "onEnum",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1256, 56, 20))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1237, 56, 1))
            ),
            InputObjectTypeDefinition(
              "InputType",
              Vector(
                InputValueDefinition("key", NotNullType(NamedType("String", Some(AstLocation(1339, 62, 8))), Some(AstLocation(1339, 62, 8))), None, Vector.empty, None, Vector.empty, Some(AstLocation(1334, 62, 3))),
                InputValueDefinition("answer", NamedType("Int", Some(AstLocation(1357, 63, 11))), Some(BigIntValue(42, Vector.empty, Some(AstLocation(1363, 63, 17)))), Vector.empty, None, Vector.empty, Some(AstLocation(1349, 63, 3)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1314, 61, 1))
            ),
            InputObjectTypeDefinition(
              "AnnotatedInput",
              Vector(
                InputValueDefinition("annotatedField", NamedType("Type", Some(AstLocation(1447, 68, 19))), None, Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1452, 68, 24))
                )), None, Vector(Comment(" field comment", Some(AstLocation(1413, 67, 3)))), Some(AstLocation(1431, 68, 3)))),
              Vector(
                Directive(
                  "onInputObjectType",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1390, 66, 22))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1369, 66, 1))
            ),
            ObjectTypeExtensionDefinition(
              "Foo",
              Vector.empty,
              Vector(
                FieldDefinition("seven", NamedType("Type", Some(AstLocation(1511, 72, 30))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(AstLocation(1501, 72, 20))), Some(AstLocation(1500, 72, 19))), None, Vector.empty, None, Vector.empty, Some(AstLocation(1490, 72, 9)))), Vector.empty, None, Vector.empty, Some(AstLocation(1484, 72, 3)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1464, 71, 1))
            ),
            ObjectTypeExtensionDefinition(
              "Foo",
              Vector.empty,
              Vector.empty,
              Vector(
                Directive(
                  "onType",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(1535, 75, 17))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(1519, 75, 1))
            ),
            DirectiveDefinition(
              "skip",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(AstLocation(1577, 78, 21))), Some(AstLocation(1577, 78, 21))), None, Vector.empty, None, Vector.empty, Some(AstLocation(1573, 78, 17)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(AstLocation(1590, 78, 34))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(AstLocation(1598, 78, 42))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(AstLocation(1616, 78, 60)))),
              Some(StringValue("cool skip", false, None, Vector.empty, Some(AstLocation(1545, 77, 1)))),
              Vector.empty,
              Some(AstLocation(1557, 78, 1))
            ),
            DirectiveDefinition(
              "include",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(AstLocation(1656, 80, 24))), Some(AstLocation(1656, 80, 24))), None, Vector.empty, None, Vector.empty, Some(AstLocation(1652, 80, 20)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(AstLocation(1671, 81, 6))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(AstLocation(1682, 82, 6))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(AstLocation(1703, 83, 6)))),
              None,
              Vector.empty,
              Some(AstLocation(1633, 80, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        )

      parseQuery(query) should be(Success(expectedAst))
    }

    "Simple type" in {
      val Success(ast) = parseQuery(
        """
          # my type
          # comment
          type Hello {
            # and field comment as well
            world: String
          }
        """)
      
      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(AstLocation(123, 6, 20))), Vector.empty, Vector.empty, None, Vector(Comment(" and field comment as well", Some(AstLocation(76, 5, 13)))), Some(AstLocation(116, 6, 13)))),
              Vector.empty,
              None,
              Vector(
                Comment(" my type", Some(AstLocation(11, 2, 11))),
                Comment(" comment", Some(AstLocation(31, 3, 11)))),
              Vector.empty,
              Some(AstLocation(51, 4, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple extension" in {
      val Success(ast) = parseQuery(
        """
          # my type
          # comment
          extend type Hello {
            world: String
          }
        """)

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeExtensionDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(AstLocation(90, 5, 20))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(83, 5, 13)))),
              Vector.empty,
              Vector(
                Comment(" my type", Some(AstLocation(11, 2, 11))),
                Comment(" comment", Some(AstLocation(31, 3, 11)))),
              Vector.empty,
              Some(AstLocation(51, 4, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple non-null type" in {
      val Success(ast) = parseQuery(
        """
          type Hello {
            world: String!
          }
        """)
      
      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NotNullType(NamedType("String", Some(AstLocation(43, 3, 20))), Some(AstLocation(43, 3, 20))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(36, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple type inheriting interface" in {
      val Success(ast) = parseQuery(
        "type Hello implements World { foo: Bar }")

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("World", Some(AstLocation(22, 1, 23)))),
              Vector(
                FieldDefinition("foo", NamedType("Bar", Some(AstLocation(35, 1, 36))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(30, 1, 31)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Simple type inheriting multiple interfaces" in {
      val Success(ast) = parseQuery(
        "type Hello implements Wo & rld { foo: Bar }")

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("Wo", Some(AstLocation(22, 1, 23))),
                NamedType("rld", Some(AstLocation(27, 1, 28)))),
              Vector(
                FieldDefinition("foo", NamedType("Bar", Some(AstLocation(38, 1, 39))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(33, 1, 34)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Simple type inheriting multiple interfaces (allow separator at the beginning)" in {
      val Success(ast) = parseQuery(
        """
          type Hello implements
            & Foo
            & Baz
          {
            foo: Bar
          }
        """)
      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("Foo", Some(AstLocation(47, 3, 15))),
                NamedType("Baz", Some(AstLocation(65, 4, 15)))),
              Vector(
                FieldDefinition("foo", NamedType("Bar", Some(AstLocation(98, 6, 18))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(93, 6, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple type inheriting multiple interfaces (legacy syntax)" in {
      val Success(ast) = QueryParser.parse(
        "type Hello implements Wo, rld { foo: Bar }", ParserConfig.default.withEmptySourceId.withEmptySourceMapper.withLegacyImplementsInterface)

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("Wo", Some(AstLocation(22, 1, 23))),
                NamedType("rld", Some(AstLocation(26, 1, 27)))),
              Vector(
                FieldDefinition("foo", NamedType("Bar", Some(AstLocation(37, 1, 38))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(32, 1, 33)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Double value enum" in {
      val Success(ast) = parseQuery(
        "enum Hello { WO, RLD }")

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            EnumTypeDefinition(
              "Hello",
              Vector(
                EnumValueDefinition("WO", Vector.empty, None, Vector.empty, Some(AstLocation(13, 1, 14))),
                EnumValueDefinition("RLD", Vector.empty, None, Vector.empty, Some(AstLocation(17, 1, 18)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Simple interface" in {
      val Success(ast) = parseQuery(
        """
          #foo
          interface Hello {
            world: String
          }
        """)
      
      ast.withoutSourceMapper should be (
        Document(
          Vector(
            InterfaceTypeDefinition(
              "Hello",
              Vector(
                FieldDefinition("world", NamedType("String", Some(AstLocation(63, 4, 20))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(56, 4, 13)))),
              Vector.empty,
              None,
              Vector(
                Comment("foo", Some(AstLocation(11, 2, 11)))),
              Vector.empty,
              Some(AstLocation(26, 3, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple field with arg" in {
      val Success(ast) = parseQuery(
        """
          #c1
          type Hello {
            #c2
            world(
              #c3

              #c4
              flag: Boolean): String
          }
        """)

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(AstLocation(140, 9, 31))), Vector(InputValueDefinition("flag", NamedType("Boolean", Some(AstLocation(130, 9, 21))), None, Vector.empty, None, Vector(Comment("c3", Some(AstLocation(87, 6, 15))), Comment("c4", Some(AstLocation(106, 8, 15)))), Some(AstLocation(124, 9, 15)))), Vector.empty, None, Vector(Comment("c2", Some(AstLocation(50, 4, 13)))), Some(AstLocation(66, 5, 13)))),
              Vector.empty,
              None,
              Vector(
                Comment("c1", Some(AstLocation(11, 2, 11)))),
              Vector.empty,
              Some(AstLocation(25, 3, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple field with arg with default value" in {
      val Success(ast) = parseQuery(
        """
          type Hello {
            world(flag: Boolean =
              # value comment
              true): String
          }
        """)

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(AstLocation(109, 5, 22))), Vector(InputValueDefinition("flag", NamedType("Boolean", Some(AstLocation(48, 3, 25))), Some(BooleanValue(true, Vector(Comment(" value comment", Some(AstLocation(72, 4, 15)))), Some(AstLocation(102, 5, 15)))), Vector.empty, None, Vector.empty, Some(AstLocation(42, 3, 19)))), Vector.empty, None, Vector.empty, Some(AstLocation(36, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple field with list arg" in {
      val Success(ast) = parseQuery(
        """
          type Hello {
            world(things: [String]): String
          }
        """)

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(AstLocation(61, 3, 38))), Vector(InputValueDefinition("things", ListType(NamedType("String", Some(AstLocation(51, 3, 28))), Some(AstLocation(50, 3, 27))), None, Vector.empty, None, Vector.empty, Some(AstLocation(42, 3, 19)))), Vector.empty, None, Vector.empty, Some(AstLocation(36, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple field with two args" in {
      val Success(ast) = parseQuery(
        """
          type Hello {
            world(argOne: Boolean, argTwo: Int): String
          }
        """)

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(AstLocation(73, 3, 50))), Vector(InputValueDefinition("argOne", NamedType("Boolean", Some(AstLocation(50, 3, 27))), None, Vector.empty, None, Vector.empty, Some(AstLocation(42, 3, 19))), InputValueDefinition("argTwo", NamedType("Int", Some(AstLocation(67, 3, 44))), None, Vector.empty, None, Vector.empty, Some(AstLocation(59, 3, 36)))), Vector.empty, None, Vector.empty, Some(AstLocation(36, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple union" in {
      val Success(ast) = parseQuery(
        "union Hello = World")

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            UnionTypeDefinition(
              "Hello",
              Vector(
                NamedType("World", Some(AstLocation(14, 1, 15)))),
              Vector.empty,
              None,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Union with two types" in {
      val Success(ast) = parseQuery(
        "union Hello = Wo | Rld")

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            UnionTypeDefinition(
              "Hello",
              Vector(
                NamedType("Wo", Some(AstLocation(14, 1, 15))),
                NamedType("Rld", Some(AstLocation(19, 1, 20)))),
              Vector.empty,
              None,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Scalar" in {
      val Success(ast) = parseQuery(
        "scalar Hello")
      
      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ScalarTypeDefinition(
              "Hello",
              Vector.empty,
              None,
              Vector.empty,
              Some(AstLocation(0, 1, 1))
            )),
          Vector.empty,
          Some(AstLocation(0, 1, 1)),
          None
        ))
    }

    "Simple input object" in {
      val Success(ast) = parseQuery(
        """
          input Hello {
            world: String
          }
        """)
      
      ast.withoutSourceMapper should be (
        Document(
          Vector(
            InputObjectTypeDefinition(
              "Hello",
              Vector(
                InputValueDefinition("world", NamedType("String", Some(AstLocation(44, 3, 20))), None, Vector.empty, None, Vector.empty, Some(AstLocation(37, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Simple input object with args should fail" in {
      import sangria.parser.DeliveryScheme.Throw

      an [SyntaxError] should be thrownBy parseQuery(
        """
          input Hello {
            world(foo: Int): String
          }
        """)
    }

    "Allow legacy empty fields syntax" in {
      val Success(ast) = QueryParser.parse(
        """
          type Foo @hello {}
          interface Bar {}
          input Baz {}
        """, ParserConfig.default.withEmptySourceId.withEmptySourceMapper.withLegacyEmptyFields)

      ast.withoutSourceMapper should be(
        Document(
          Vector(
            ObjectTypeDefinition(
              "Foo",
              Vector.empty,
              Vector.empty,
              Vector(
                Directive(
                  "hello",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(20, 2, 20))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(40, 3, 11))
            ),
            InputObjectTypeDefinition(
              "Baz",
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(67, 4, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }
    
    "Allow empty fields and values" in {
      val Success(ast) = parseQuery(
        """
          type Foo @hello
          interface Bar
          input Baz
          enum Test
          union Test @bar
        """)

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Foo",
              Vector.empty,
              Vector.empty,
              Vector(
                Directive(
                  "hello",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(20, 2, 20))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(37, 3, 11))
            ),
            InputObjectTypeDefinition(
              "Baz",
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(61, 4, 11))
            ),
            EnumTypeDefinition(
              "Test",
              Vector.empty,
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(81, 5, 11))
            ),
            UnionTypeDefinition(
              "Test",
              Vector.empty,
              Vector(
                Directive(
                  "bar",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(112, 6, 22))
                )),
              None,
              Vector.empty,
              Some(AstLocation(101, 6, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }

    "Allow extensions on various types" in {
      val Success(ast) = parseQuery(
        """
          extend type Foo implements Hello & World @hello(ids: [1, 2]) {
           f1: Int
          }

          extend interface Bar @dir {
            "test field"
            f2: [String!]
          }

          extend input Baz {
           inp: Int = 1
          }

          extend enum Color {
            MAGENTA
          }

          extend union Test @bar = More | Types

          extend type EmptyFoo implements Hello1 & World1 @hello1(ids: [1, 2])
          extend interface EmptyBar @dir1
          extend input EmptyBaz @extraDir
          extend enum EmptyColor @beautiful
          extend union EmptyTest @bar
        """)

      ast.withoutSourceMapper should be (
        Document(
          Vector(
            ObjectTypeExtensionDefinition(
              "Foo",
              Vector(
                NamedType("Hello", Some(AstLocation(38, 2, 38))),
                NamedType("World", Some(AstLocation(46, 2, 46)))),
              Vector(
                FieldDefinition("f1", NamedType("Int", Some(AstLocation(89, 3, 16))), Vector.empty, Vector.empty, None, Vector.empty, Some(AstLocation(85, 3, 12)))),
              Vector(
                Directive(
                  "hello",
                  Vector(
                    Argument(
                      "ids",
                      ListValue(
                        Vector(
                          BigIntValue(1, Vector.empty, Some(AstLocation(65, 2, 65))),
                          BigIntValue(2, Vector.empty, Some(AstLocation(68, 2, 68)))),
                        Vector.empty,
                        Some(AstLocation(64, 2, 64))
                      ),
                      Vector.empty,
                      Some(AstLocation(59, 2, 59))
                    )),
                  Vector.empty,
                  Some(AstLocation(52, 2, 52))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(11, 2, 11))
            ),
            InterfaceTypeExtensionDefinition(
              "Bar",
              Vector(
                FieldDefinition("f2", ListType(NotNullType(NamedType("String", Some(AstLocation(186, 8, 18))), Some(AstLocation(186, 8, 18))), Some(AstLocation(185, 8, 17))), Vector.empty, Vector.empty, Some(StringValue("test field", false, None, Vector.empty, Some(AstLocation(156, 7, 13)))), Vector.empty, Some(AstLocation(181, 8, 13)))),
              Vector(
                Directive(
                  "dir",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(137, 6, 32))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(116, 6, 11))
            ),
            InputObjectTypeExtensionDefinition(
              "Baz",
              Vector(
                InputValueDefinition("inp", NamedType("Int", Some(AstLocation(253, 12, 17))), Some(BigIntValue(1, Vector.empty, Some(AstLocation(259, 12, 23)))), Vector.empty, None, Vector.empty, Some(AstLocation(248, 12, 12)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(218, 11, 11))
            ),
            EnumTypeExtensionDefinition(
              "Color",
              Vector(
                EnumValueDefinition("MAGENTA", Vector.empty, None, Vector.empty, Some(AstLocation(316, 16, 13)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(AstLocation(284, 15, 11))
            ),
            UnionTypeExtensionDefinition(
              "Test",
              Vector(
                NamedType("More", Some(AstLocation(372, 19, 36))),
                NamedType("Types", Some(AstLocation(379, 19, 43)))),
              Vector(
                Directive(
                  "bar",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(365, 19, 29))
                )),
              Vector.empty,
              Some(AstLocation(347, 19, 11))
            ),
            ObjectTypeExtensionDefinition(
              "EmptyFoo",
              Vector(
                NamedType("Hello1", Some(AstLocation(428, 21, 43))),
                NamedType("World1", Some(AstLocation(437, 21, 52)))),
              Vector.empty,
              Vector(
                Directive(
                  "hello1",
                  Vector(
                    Argument(
                      "ids",
                      ListValue(
                        Vector(
                          BigIntValue(1, Vector.empty, Some(AstLocation(458, 21, 73))),
                          BigIntValue(2, Vector.empty, Some(AstLocation(461, 21, 76)))),
                        Vector.empty,
                        Some(AstLocation(457, 21, 72))
                      ),
                      Vector.empty,
                      Some(AstLocation(452, 21, 67))
                    )),
                  Vector.empty,
                  Some(AstLocation(444, 21, 59))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(396, 21, 11))
            ),
            InterfaceTypeExtensionDefinition(
              "EmptyBar",
              Vector.empty,
              Vector(
                Directive(
                  "dir1",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(501, 22, 37))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(475, 22, 11))
            ),
            InputObjectTypeExtensionDefinition(
              "EmptyBaz",
              Vector.empty,
              Vector(
                Directive(
                  "extraDir",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(539, 23, 33))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(517, 23, 11))
            ),
            EnumTypeExtensionDefinition(
              "EmptyColor",
              Vector.empty,
              Vector(
                Directive(
                  "beautiful",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(582, 24, 34))
                )),
              Vector.empty,
              Vector.empty,
              Some(AstLocation(559, 24, 11))
            ),
            UnionTypeExtensionDefinition(
              "EmptyTest",
              Vector.empty,
              Vector(
                Directive(
                  "bar",
                  Vector.empty,
                  Vector.empty,
                  Some(AstLocation(626, 25, 34))
                )),
              Vector.empty,
              Some(AstLocation(603, 25, 11))
            )),
          Vector.empty,
          Some(AstLocation(11, 2, 11)),
          None
        ))
    }
  }
}