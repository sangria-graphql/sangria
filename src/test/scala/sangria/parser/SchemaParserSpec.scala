package sangria.parser

import org.parboiled2.Position
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._
import sangria.util.{DebugUtil, FileUtil, StringMatchers}

import scala.util.Success

class SchemaParserSpec extends WordSpec with Matchers with StringMatchers {

  "QueryParser" should {
    "parse schema kitchen sink" in {
      val query = FileUtil loadQuery "schema-kitchen-sink.graphql"

      val expectedAst =
        Document(
          Vector(
            SchemaDefinition(
              Vector(
                OperationTypeDefinition(OperationType.Query, NamedType("QueryType", Some(Position(306, 9, 10))), Vector.empty, Some(Position(299, 9, 3))),
                OperationTypeDefinition(OperationType.Mutation, NamedType("MutationType", Some(Position(328, 10, 13))), Vector.empty, Some(Position(318, 10, 3)))),
              Vector.empty,
              Vector(
                Comment(" Copyright (c) 2015, Facebook, Inc.", Some(Position(0, 1, 1))),
                Comment(" All rights reserved.", Some(Position(37, 2, 1))),
                Comment("", Some(Position(60, 3, 1))),
                Comment(" This source code is licensed under the BSD-style license found in the", Some(Position(62, 4, 1))),
                Comment(" LICENSE file in the root directory of this source tree. An additional grant", Some(Position(134, 5, 1))),
                Comment(" of patent rights can be found in the PATENTS file in the same directory.", Some(Position(212, 6, 1)))),
              Vector.empty,
              Some(Position(288, 8, 1))
            ),
            ObjectTypeDefinition(
              "Foo",
              Vector(
                NamedType("Bar", Some(Position(390, 14, 21)))),
              Vector(
                FieldDefinition("one", NamedType("Type", Some(Position(403, 15, 8))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(398, 15, 3))),
                FieldDefinition("two", NamedType("Type", Some(Position(437, 16, 30))), Vector(InputValueDefinition("argument", NotNullType(NamedType("InputType", Some(Position(424, 16, 17))), Some(Position(424, 16, 17))), None, Vector.empty, None, Vector.empty, Some(Position(414, 16, 7)))), Vector.empty, None, Vector.empty, Some(Position(410, 16, 3))),
                FieldDefinition("three", NamedType("Int", Some(Position(487, 17, 46))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(Position(460, 17, 19))), None, Vector.empty, None, Vector.empty, Some(Position(450, 17, 9))), InputValueDefinition("other", NamedType("String", Some(Position(478, 17, 37))), None, Vector.empty, None, Vector.empty, Some(Position(471, 17, 30)))), Vector.empty, None, Vector.empty, Some(Position(444, 17, 3))),
                FieldDefinition("four", NamedType("String", Some(Position(528, 18, 38))), Vector(InputValueDefinition("argument", NamedType("String", Some(Position(508, 18, 18))), Some(StringValue("string", false, None, Vector.empty, Some(Position(517, 18, 27)))), Vector.empty, None, Vector.empty, Some(Position(498, 18, 8)))), Vector.empty, None, Vector.empty, Some(Position(493, 18, 3))),
                FieldDefinition("five", NamedType("String", Some(Position(586, 19, 52))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(553, 19, 19))), Some(Position(552, 19, 18))), Some(ListValue(
                  Vector(
                    StringValue("string", false, None, Vector.empty, Some(Position(564, 19, 30))),
                    StringValue("string", false, None, Vector.empty, Some(Position(574, 19, 40)))),
                  Vector.empty,
                  Some(Position(563, 19, 29))
                )), Vector.empty, None, Vector.empty, Some(Position(542, 19, 8)))), Vector.empty, None, Vector.empty, Some(Position(537, 19, 3))),
                FieldDefinition("six", NamedType("Type", Some(Position(678, 22, 46))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(Position(649, 22, 17))), Some(ObjectValue(
                  Vector(
                    ObjectField(
                      "key",
                      StringValue("value", false, None, Vector.empty, Some(Position(667, 22, 35))),
                      Vector.empty,
                      Some(Position(662, 22, 30))
                    )),
                  Vector.empty,
                  Some(Position(661, 22, 29))
                )), Vector.empty, None, Vector.empty, Some(Position(639, 22, 7)))), Vector.empty, Some(StringValue("More \"\"\" descriptions \\", true, Some("\n  More \"\"\" descriptions \\\n  "), Vector.empty, Some(Position(596, 21, 3)))), Vector.empty, Some(Position(635, 22, 3)))),
              Vector.empty,
              Some(StringValue("type description!", true, Some("\ntype description!\n"), Vector.empty, Some(Position(344, 13, 1)))),
              Vector.empty,
              Vector.empty,
              Some(Position(370, 14, 1))
            ),
            ObjectTypeDefinition(
              "AnnotatedObject",
              Vector.empty,
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(781, 26, 49))), Vector(InputValueDefinition("arg", NamedType("Type", Some(Position(755, 26, 23))), Some(StringValue("default", false, None, Vector.empty, Some(Position(762, 26, 30)))), Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(772, 26, 40))
                )), None, Vector.empty, Some(Position(750, 26, 18)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(786, 26, 54))
                )), None, Vector.empty, Some(Position(735, 26, 3)))),
              Vector(
                Directive(
                  "onObject",
                  Vector(
                    Argument(
                      "arg",
                      StringValue("value", false, None, Vector.empty, Some(Position(722, 25, 37))),
                      Vector.empty,
                      Some(Position(717, 25, 32))
                    )),
                  Vector.empty,
                  Some(Position(707, 25, 22))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(686, 25, 1))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector(
                FieldDefinition("one", NamedType("Type", Some(Position(875, 33, 8))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(870, 33, 3))),
                FieldDefinition("four", NamedType("String", Some(Position(917, 34, 38))), Vector(InputValueDefinition("argument", NamedType("String", Some(Position(897, 34, 18))), Some(StringValue("string", false, None, Vector.empty, Some(Position(906, 34, 27)))), Vector.empty, None, Vector.empty, Some(Position(887, 34, 8)))), Vector.empty, None, Vector.empty, Some(Position(882, 34, 3)))),
              Vector.empty,
              Some(StringValue(" It's an interface!", false, None, Vector(Comment(" comment above", Some(Position(798, 29, 1)))), Some(Position(814, 30, 1)))),
              Vector(
                Comment(" comment below", Some(Position(836, 31, 1)))),
              Vector.empty,
              Some(Position(852, 32, 1))
            ),
            InterfaceTypeDefinition(
              "AnnotatedInterface",
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(1007, 38, 37))), Vector(InputValueDefinition("arg", NamedType("Type", Some(Position(993, 38, 23))), None, Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(998, 38, 28))
                )), None, Vector.empty, Some(Position(988, 38, 18)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1012, 38, 42))
                )), None, Vector.empty, Some(Position(973, 38, 3)))),
              Vector(
                Directive(
                  "onInterface",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(956, 37, 30))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(927, 37, 1))
            ),
            UnionTypeDefinition(
              "Feed",
              Vector(
                NamedType("Story", Some(Position(1037, 41, 14))),
                NamedType("Article", Some(Position(1045, 41, 22))),
                NamedType("Advert", Some(Position(1055, 41, 32)))),
              Vector.empty,
              None,
              Vector.empty,
              Some(Position(1024, 41, 1))
            ),
            UnionTypeDefinition(
              "AnnotatedUnion",
              Vector(
                NamedType("A", Some(Position(1095, 43, 33))),
                NamedType("B", Some(Position(1099, 43, 37)))),
              Vector(
                Directive(
                  "onUnion",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1084, 43, 22))
                )),
              None,
              Vector.empty,
              Some(Position(1063, 43, 1))
            ),
            ScalarTypeDefinition(
              "CustomScalar",
              Vector.empty,
              None,
              Vector.empty,
              Some(Position(1102, 45, 1))
            ),
            ScalarTypeDefinition(
              "AnnotatedScalar",
              Vector(
                Directive(
                  "onScalar",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1146, 47, 24))
                )),
              None,
              Vector.empty,
              Some(Position(1123, 47, 1))
            ),
            EnumTypeDefinition(
              "Site",
              Vector(
                EnumValueDefinition("DESKTOP", Vector.empty, Some(StringValue("description 1", false, None, Vector.empty, Some(Position(1171, 50, 3)))), Vector.empty, Some(Position(1189, 51, 3))),
                EnumValueDefinition("MOBILE", Vector.empty, Some(StringValue("description 2", true, Some("\n  description 2\n  "), Vector.empty, Some(Position(1199, 52, 3)))), Vector.empty, Some(Position(1227, 53, 3)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1157, 49, 1))
            ),
            EnumTypeDefinition(
              "AnnotatedEnum",
              Vector(
                EnumValueDefinition("ANNOTATED_VALUE", Vector(Directive(
                  "onEnumValue",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1284, 57, 19))
                )), None, Vector.empty, Some(Position(1268, 57, 3))),
                EnumValueDefinition("OTHER_VALUE", Vector.empty, None, Vector.empty, Some(Position(1299, 58, 3)))),
              Vector(
                Directive(
                  "onEnum",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1256, 56, 20))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1237, 56, 1))
            ),
            InputObjectTypeDefinition(
              "InputType",
              Vector(
                InputValueDefinition("key", NotNullType(NamedType("String", Some(Position(1339, 62, 8))), Some(Position(1339, 62, 8))), None, Vector.empty, None, Vector.empty, Some(Position(1334, 62, 3))),
                InputValueDefinition("answer", NamedType("Int", Some(Position(1357, 63, 11))), Some(BigIntValue(42, Vector.empty, Some(Position(1363, 63, 17)))), Vector.empty, None, Vector.empty, Some(Position(1349, 63, 3)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1314, 61, 1))
            ),
            InputObjectTypeDefinition(
              "AnnotatedInput",
              Vector(
                InputValueDefinition("annotatedField", NamedType("Type", Some(Position(1447, 68, 19))), None, Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1452, 68, 24))
                )), None, Vector(Comment(" field comment", Some(Position(1413, 67, 3)))), Some(Position(1431, 68, 3)))),
              Vector(
                Directive(
                  "onInputObjectType",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1390, 66, 22))
                )),
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(1369, 66, 1))
            ),
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Foo",
                Vector.empty,
                Vector(
                  FieldDefinition("seven", NamedType("Type", Some(Position(1511, 72, 30))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(1501, 72, 20))), Some(Position(1500, 72, 19))), None, Vector.empty, None, Vector.empty, Some(Position(1490, 72, 9)))), Vector.empty, None, Vector.empty, Some(Position(1484, 72, 3)))),
                Vector.empty,
                None,
                Vector.empty,
                Vector.empty,
                Some(Position(1464, 71, 1))
              ),
              Vector.empty,
              Some(Position(1464, 71, 1))
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
                    Some(Position(1535, 75, 17))
                  )),
                None,
                Vector.empty,
                Vector.empty,
                Some(Position(1519, 75, 1))
              ),
              Vector.empty,
              Some(Position(1519, 75, 1))
            ),
            DirectiveDefinition(
              "skip",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(1577, 78, 21))), Some(Position(1577, 78, 21))), None, Vector.empty, None, Vector.empty, Some(Position(1573, 78, 17)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(Position(1590, 78, 34))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(Position(1598, 78, 42))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(Position(1616, 78, 60)))),
              Some(StringValue("cool skip", false, None, Vector.empty, Some(Position(1545, 77, 1)))),
              Vector.empty,
              Some(Position(1557, 78, 1))
            ),
            DirectiveDefinition(
              "include",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(1656, 80, 24))), Some(Position(1656, 80, 24))), None, Vector.empty, None, Vector.empty, Some(Position(1652, 80, 20)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(Position(1671, 81, 6))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(Position(1682, 82, 6))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(Position(1703, 83, 6)))),
              None,
              Vector.empty,
              Some(Position(1633, 80, 1))
            )),
          Vector.empty,
          Some(Position(0, 1, 1)),
          None
        )
      
      QueryParser.parse(query) map (_.withoutSourceMapper) should be(Success(expectedAst))
    }

    "Simple type" in {
      val Success(ast) = QueryParser.parse(
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
                FieldDefinition("world", NamedType("String", Some(Position(123, 6, 20))), Vector.empty, Vector.empty, None, Vector(Comment(" and field comment as well", Some(Position(76, 5, 13)))), Some(Position(116, 6, 13)))),
              Vector.empty,
              None,
              Vector(
                Comment(" my type", Some(Position(11, 2, 11))),
                Comment(" comment", Some(Position(31, 3, 11)))),
              Vector.empty,
              Some(Position(51, 4, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple extension" in {
      val Success(ast) = QueryParser.parse(
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
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Hello",
                Vector.empty,
                Vector(
                  FieldDefinition("world", NamedType("String", Some(Position(90, 5, 20))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(83, 5, 13)))),
                Vector.empty,
                None,
                Vector(
                  Comment(" my type", Some(Position(11, 2, 11))),
                  Comment(" comment", Some(Position(31, 3, 11)))),
                Vector.empty,
                Some(Position(51, 4, 11))
              ),
              Vector.empty,
              Some(Position(51, 4, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple non-null type" in {
      val Success(ast) = QueryParser.parse(
        """
          type Hello {
            world: String!
          }
        """)
      
      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NotNullType(NamedType("String", Some(Position(43, 3, 20))), Some(Position(43, 3, 20))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(36, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(11, 2, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple type inheriting interface" in {
      val Success(ast) = QueryParser.parse(
        "type Hello implements World { foo: Bar }")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("World", Some(Position(22, 1, 23)))),
              Vector(
                FieldDefinition("foo", NamedType("Bar", Some(Position(35, 1, 36))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(30, 1, 31)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(0, 1, 1))
            )),
          Vector.empty,
          Some(Position(0, 1, 1)),
          None
        ))
    }

    "Simple type inheriting multiple interfaces" in {
      val Success(ast) = QueryParser.parse(
        "type Hello implements Wo & rld { foo: Bar }")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("Wo", Some(Position(22, 1, 23))),
                NamedType("rld", Some(Position(27, 1, 28)))),
              Vector(
                FieldDefinition("foo", NamedType("Bar", Some(Position(38, 1, 39))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(33, 1, 34)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(0, 1, 1))
            )),
          Vector.empty,
          Some(Position(0, 1, 1)),
          None
        ))
    }

    "Simple type inheriting multiple interfaces (allow separator at the beginning)" in {
      val Success(ast) = QueryParser.parse(
        """
          type Hello implements
            & Foo
            & Baz
          {
            foo: Bar
          }
        """)
      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("Foo", Some(Position(47, 3, 15))),
                NamedType("Baz", Some(Position(65, 4, 15)))),
              Vector(
                FieldDefinition("foo", NamedType("Bar", Some(Position(98, 6, 18))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(93, 6, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(11, 2, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple type inheriting multiple interfaces (legacy syntax)" in {
      val Success(ast) = QueryParser.parse(
        "type Hello implements Wo, rld { foo: Bar }", legacyImplementsInterface = true)

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector(
                NamedType("Wo", Some(Position(22, 1, 23))),
                NamedType("rld", Some(Position(26, 1, 27)))),
              Vector(
                FieldDefinition("foo", NamedType("Bar", Some(Position(37, 1, 38))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(32, 1, 33)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(0, 1, 1))
            )),
          Vector.empty,
          Some(Position(0, 1, 1)),
          None
        ))
    }

    "Double value enum" in {
      val Success(ast) = QueryParser.parse(
        "enum Hello { WO, RLD }")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            EnumTypeDefinition(
              "Hello",
              Vector(
                EnumValueDefinition("WO", Vector.empty, None, Vector.empty, Some(Position(13, 1, 14))),
                EnumValueDefinition("RLD", Vector.empty, None, Vector.empty, Some(Position(17, 1, 18)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(0, 1, 1))
            )),
          Vector.empty,
          Some(Position(0, 1, 1)),
          None
        ))
    }

    "Simple interface" in {
      val Success(ast) = QueryParser.parse(
        """
          #foo
          interface Hello {
            world: String
          }
        """)
      
      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            InterfaceTypeDefinition(
              "Hello",
              Vector(
                FieldDefinition("world", NamedType("String", Some(Position(63, 4, 20))), Vector.empty, Vector.empty, None, Vector.empty, Some(Position(56, 4, 13)))),
              Vector.empty,
              None,
              Vector(
                Comment("foo", Some(Position(11, 2, 11)))),
              Vector.empty,
              Some(Position(26, 3, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple field with arg" in {
      val Success(ast) = QueryParser.parse(
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
                FieldDefinition("world", NamedType("String", Some(Position(140, 9, 31))), Vector(InputValueDefinition("flag", NamedType("Boolean", Some(Position(130, 9, 21))), None, Vector.empty, None, Vector(Comment("c3", Some(Position(87, 6, 15))), Comment("c4", Some(Position(106, 8, 15)))), Some(Position(124, 9, 15)))), Vector.empty, None, Vector(Comment("c2", Some(Position(50, 4, 13)))), Some(Position(66, 5, 13)))),
              Vector.empty,
              None,
              Vector(
                Comment("c1", Some(Position(11, 2, 11)))),
              Vector.empty,
              Some(Position(25, 3, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple field with arg with default value" in {
      val Success(ast) = QueryParser.parse(
        """
          type Hello {
            world(flag: Boolean =
              # value comment
              true): String
          }
        """)

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(Position(109, 5, 22))), Vector(InputValueDefinition("flag", NamedType("Boolean", Some(Position(48, 3, 25))), Some(BooleanValue(true, Vector(Comment(" value comment", Some(Position(72, 4, 15)))), Some(Position(102, 5, 15)))), Vector.empty, None, Vector.empty, Some(Position(42, 3, 19)))), Vector.empty, None, Vector.empty, Some(Position(36, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(11, 2, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple field with list arg" in {
      val Success(ast) = QueryParser.parse(
        """
          type Hello {
            world(things: [String]): String
          }
        """)

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(Position(61, 3, 38))), Vector(InputValueDefinition("things", ListType(NamedType("String", Some(Position(51, 3, 28))), Some(Position(50, 3, 27))), None, Vector.empty, None, Vector.empty, Some(Position(42, 3, 19)))), Vector.empty, None, Vector.empty, Some(Position(36, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(11, 2, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple field with two args" in {
      val Success(ast) = QueryParser.parse(
        """
          type Hello {
            world(argOne: Boolean, argTwo: Int): String
          }
        """)

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition(
              "Hello",
              Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(Position(73, 3, 50))), Vector(InputValueDefinition("argOne", NamedType("Boolean", Some(Position(50, 3, 27))), None, Vector.empty, None, Vector.empty, Some(Position(42, 3, 19))), InputValueDefinition("argTwo", NamedType("Int", Some(Position(67, 3, 44))), None, Vector.empty, None, Vector.empty, Some(Position(59, 3, 36)))), Vector.empty, None, Vector.empty, Some(Position(36, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(11, 2, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple union" in {
      val Success(ast) = QueryParser.parse(
        "union Hello = World")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            UnionTypeDefinition(
              "Hello",
              Vector(
                NamedType("World", Some(Position(14, 1, 15)))),
              Vector.empty,
              None,
              Vector.empty,
              Some(Position(0, 1, 1))
            )),
          Vector.empty,
          Some(Position(0, 1, 1)),
          None
        ))
    }

    "Union with two types" in {
      val Success(ast) = QueryParser.parse(
        "union Hello = Wo | Rld")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            UnionTypeDefinition(
              "Hello",
              Vector(
                NamedType("Wo", Some(Position(14, 1, 15))),
                NamedType("Rld", Some(Position(19, 1, 20)))),
              Vector.empty,
              None,
              Vector.empty,
              Some(Position(0, 1, 1))
            )),
          Vector.empty,
          Some(Position(0, 1, 1)),
          None
        ))
    }

    "Scalar" in {
      val Success(ast) = QueryParser.parse(
        "scalar Hello")
      
      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ScalarTypeDefinition(
              "Hello",
              Vector.empty,
              None,
              Vector.empty,
              Some(Position(0, 1, 1))
            )),
          Vector.empty,
          Some(Position(0, 1, 1)),
          None
        ))
    }

    "Simple input object" in {
      val Success(ast) = QueryParser.parse(
        """
          input Hello {
            world: String
          }
        """)
      
      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            InputObjectTypeDefinition(
              "Hello",
              Vector(
                InputValueDefinition("world", NamedType("String", Some(Position(44, 3, 20))), None, Vector.empty, None, Vector.empty, Some(Position(37, 3, 13)))),
              Vector.empty,
              None,
              Vector.empty,
              Vector.empty,
              Some(Position(11, 2, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        ))
    }

    "Simple input object with args should fail" in {
      import sangria.parser.DeliveryScheme.Throw

      an [SyntaxError] should be thrownBy QueryParser.parse(
        """
          input Hello {
            world(foo: Int): String
          }
        """)
    }
  }
}