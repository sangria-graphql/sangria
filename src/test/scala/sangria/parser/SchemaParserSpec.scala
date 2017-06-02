package sangria.parser

import org.parboiled2.Position
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._
import sangria.util.{FileUtil, StringMatchers}

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
                NamedType("Bar", Some(Position(364, 13, 21)))),
              Vector(
                FieldDefinition("one", NamedType("Type", Some(Position(377, 14, 8))), Vector.empty, Vector.empty, Vector.empty, Some(Position(372, 14, 3))),
                FieldDefinition("two", NamedType("Type", Some(Position(411, 15, 30))), Vector(InputValueDefinition("argument", NotNullType(NamedType("InputType", Some(Position(398, 15, 17))), Some(Position(398, 15, 17))), None, Vector.empty, Vector.empty, Some(Position(388, 15, 7)))), Vector.empty, Vector.empty, Some(Position(384, 15, 3))),
                FieldDefinition("three", NamedType("Int", Some(Position(461, 16, 46))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(Position(434, 16, 19))), None, Vector.empty, Vector.empty, Some(Position(424, 16, 9))), InputValueDefinition("other", NamedType("String", Some(Position(452, 16, 37))), None, Vector.empty, Vector.empty, Some(Position(445, 16, 30)))), Vector.empty, Vector.empty, Some(Position(418, 16, 3))),
                FieldDefinition("four", NamedType("String", Some(Position(502, 17, 38))), Vector(InputValueDefinition("argument", NamedType("String", Some(Position(482, 17, 18))), Some(StringValue("string", Vector.empty, Some(Position(491, 17, 27)))), Vector.empty, Vector.empty, Some(Position(472, 17, 8)))), Vector.empty, Vector.empty, Some(Position(467, 17, 3))),
                FieldDefinition("five", NamedType("String", Some(Position(560, 18, 52))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(527, 18, 19))), Some(Position(526, 18, 18))), Some(ListValue(
                  Vector(
                    StringValue("string", Vector.empty, Some(Position(538, 18, 30))),
                    StringValue("string", Vector.empty, Some(Position(548, 18, 40)))),
                  Vector.empty,
                  Some(Position(537, 18, 29))
                )), Vector.empty, Vector.empty, Some(Position(516, 18, 8)))), Vector.empty, Vector.empty, Some(Position(511, 18, 3))),
                FieldDefinition("six", NamedType("Type", Some(Position(612, 19, 46))), Vector(InputValueDefinition("argument", NamedType("InputType", Some(Position(583, 19, 17))), Some(ObjectValue(
                  Vector(
                    ObjectField(
                      "key",
                      StringValue("value", Vector.empty, Some(Position(601, 19, 35))),
                      Vector.empty,
                      Some(Position(596, 19, 30))
                    )),
                  Vector.empty,
                  Some(Position(595, 19, 29))
                )), Vector.empty, Vector.empty, Some(Position(573, 19, 7)))), Vector.empty, Vector.empty, Some(Position(569, 19, 3)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(Position(344, 13, 1))
            ),
            ObjectTypeDefinition(
              "AnnotatedObject",
              Vector.empty,
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(715, 23, 49))), Vector(InputValueDefinition("arg", NamedType("Type", Some(Position(689, 23, 23))), Some(StringValue("default", Vector.empty, Some(Position(696, 23, 30)))), Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(706, 23, 40))
                )), Vector.empty, Some(Position(684, 23, 18)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(720, 23, 54))
                )), Vector.empty, Some(Position(669, 23, 3)))),
              Vector(
                Directive(
                  "onObject",
                  Vector(
                    Argument(
                      "arg",
                      StringValue("value", Vector.empty, Some(Position(656, 22, 37))),
                      Vector.empty,
                      Some(Position(651, 22, 32))
                    )),
                  Vector.empty,
                  Some(Position(641, 22, 22))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(620, 22, 1))
            ),
            InterfaceTypeDefinition(
              "Bar",
              Vector(
                FieldDefinition("one", NamedType("Type", Some(Position(776, 28, 8))), Vector.empty, Vector.empty, Vector.empty, Some(Position(771, 28, 3))),
                FieldDefinition("four", NamedType("String", Some(Position(818, 29, 38))), Vector(InputValueDefinition("argument", NamedType("String", Some(Position(798, 29, 18))), Some(StringValue("string", Vector.empty, Some(Position(807, 29, 27)))), Vector.empty, Vector.empty, Some(Position(788, 29, 8)))), Vector.empty, Vector.empty, Some(Position(783, 29, 3)))),
              Vector.empty,
              Vector(
                Comment(" It's an interface!", Some(Position(732, 26, 1)))),
              Vector.empty,
              Some(Position(753, 27, 1))
            ),
            InterfaceTypeDefinition(
              "AnnotatedInterface",
              Vector(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(908, 33, 37))), Vector(InputValueDefinition("arg", NamedType("Type", Some(Position(894, 33, 23))), None, Vector(Directive(
                  "onArg",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(899, 33, 28))
                )), Vector.empty, Some(Position(889, 33, 18)))), Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(913, 33, 42))
                )), Vector.empty, Some(Position(874, 33, 3)))),
              Vector(
                Directive(
                  "onInterface",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(857, 32, 30))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(828, 32, 1))
            ),
            UnionTypeDefinition(
              "Feed",
              Vector(
                NamedType("Story", Some(Position(938, 36, 14))),
                NamedType("Article", Some(Position(946, 36, 22))),
                NamedType("Advert", Some(Position(956, 36, 32)))),
              Vector.empty,
              Vector.empty,
              Some(Position(925, 36, 1))
            ),
            UnionTypeDefinition(
              "AnnotatedUnion",
              Vector(
                NamedType("A", Some(Position(996, 38, 33))),
                NamedType("B", Some(Position(1000, 38, 37)))),
              Vector(
                Directive(
                  "onUnion",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(985, 38, 22))
                )),
              Vector.empty,
              Some(Position(964, 38, 1))
            ),
            ScalarTypeDefinition(
              "CustomScalar",
              Vector.empty,
              Vector.empty,
              Some(Position(1003, 40, 1))
            ),
            ScalarTypeDefinition(
              "AnnotatedScalar",
              Vector(
                Directive(
                  "onScalar",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1047, 42, 24))
                )),
              Vector.empty,
              Some(Position(1024, 42, 1))
            ),
            EnumTypeDefinition(
              "Site",
              Vector(
                EnumValueDefinition("DESKTOP", Vector.empty, Vector(Comment(" value 1", Some(Position(1072, 45, 3)))), Some(Position(1084, 46, 3))),
                EnumValueDefinition("MOBILE", Vector.empty, Vector(Comment(" value 2", Some(Position(1094, 47, 3)))), Some(Position(1106, 48, 3)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(Position(1058, 44, 1))
            ),
            EnumTypeDefinition(
              "AnnotatedEnum",
              Vector(
                EnumValueDefinition("ANNOTATED_VALUE", Vector(Directive(
                  "onEnumValue",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1163, 52, 19))
                )), Vector.empty, Some(Position(1147, 52, 3))),
                EnumValueDefinition("OTHER_VALUE", Vector.empty, Vector.empty, Some(Position(1178, 53, 3)))),
              Vector(
                Directive(
                  "onEnum",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1135, 51, 20))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(1116, 51, 1))
            ),
            InputObjectTypeDefinition(
              "InputType",
              Vector(
                InputValueDefinition("key", NotNullType(NamedType("String", Some(Position(1218, 57, 8))), Some(Position(1218, 57, 8))), None, Vector.empty, Vector.empty, Some(Position(1213, 57, 3))),
                InputValueDefinition("answer", NamedType("Int", Some(Position(1236, 58, 11))), Some(BigIntValue(42, Vector.empty, Some(Position(1242, 58, 17)))), Vector.empty, Vector.empty, Some(Position(1228, 58, 3)))),
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(Position(1193, 56, 1))
            ),
            InputObjectTypeDefinition(
              "AnnotatedInput",
              Vector(
                InputValueDefinition("annotatedField", NamedType("Type", Some(Position(1326, 63, 19))), None, Vector(Directive(
                  "onField",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1331, 63, 24))
                )), Vector(Comment(" field comment", Some(Position(1292, 62, 3)))), Some(Position(1310, 63, 3)))),
              Vector(
                Directive(
                  "onInputObjectType",
                  Vector.empty,
                  Vector.empty,
                  Some(Position(1269, 61, 22))
                )),
              Vector.empty,
              Vector.empty,
              Some(Position(1248, 61, 1))
            ),
            TypeExtensionDefinition(
              ObjectTypeDefinition(
                "Foo",
                Vector.empty,
                Vector(
                  FieldDefinition("seven", NamedType("Type", Some(Position(1390, 67, 30))), Vector(InputValueDefinition("argument", ListType(NamedType("String", Some(Position(1380, 67, 20))), Some(Position(1379, 67, 19))), None, Vector.empty, Vector.empty, Some(Position(1369, 67, 9)))), Vector.empty, Vector.empty, Some(Position(1363, 67, 3)))),
                Vector.empty,
                Vector.empty,
                Vector.empty,
                Some(Position(1350, 66, 8))
              ),
              Vector.empty,
              Some(Position(1343, 66, 1))
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
                    Some(Position(1414, 70, 17))
                  )),
                Vector.empty,
                Vector.empty,
                Some(Position(1405, 70, 8))
              ),
              Vector.empty,
              Some(Position(1398, 70, 1))
            ),
            ObjectTypeDefinition(
              "NoFields",
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Vector.empty,
              Some(Position(1426, 72, 1))
            ),
            DirectiveDefinition(
              "skip",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(1464, 74, 21))), Some(Position(1464, 74, 21))), None, Vector.empty, Vector.empty, Some(Position(1460, 74, 17)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(Position(1477, 74, 34))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(Position(1485, 74, 42))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(Position(1503, 74, 60)))),
              Vector.empty,
              Some(Position(1444, 74, 1))
            ),
            DirectiveDefinition(
              "include",
              Vector(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(1543, 76, 24))), Some(Position(1543, 76, 24))), None, Vector.empty, Vector.empty, Some(Position(1539, 76, 20)))),
              Vector(
                DirectiveLocation("FIELD", Vector.empty, Some(Position(1558, 77, 6))),
                DirectiveLocation("FRAGMENT_SPREAD", Vector.empty, Some(Position(1569, 78, 6))),
                DirectiveLocation("INLINE_FRAGMENT", Vector.empty, Some(Position(1590, 79, 6)))),
              Vector.empty,
              Some(Position(1520, 76, 1))
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
                FieldDefinition("world", NamedType("String", Some(Position(123, 6, 20))), Vector.empty, Vector.empty, Vector(Comment(" and field comment as well", Some(Position(76, 5, 13)))), Some(Position(116, 6, 13)))),
              Vector.empty,
              Vector(
                Comment(" my type", Some(Position(11, 2, 11))),
                Comment(" comment", Some(Position(31, 3, 11)))),
              Vector.empty,
              Some(Position(51, 4, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        )
      )
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
                  FieldDefinition("world", NamedType("String", Some(Position(90, 5, 20))), Vector.empty, Vector.empty, Vector.empty, Some(Position(83, 5, 13)))),
                Vector.empty,
                Vector.empty,
                Vector.empty,
                Some(Position(58, 4, 18))
              ),
              Vector(
                Comment(" my type", Some(Position(11, 2, 11))),
                Comment(" comment", Some(Position(31, 3, 11)))),
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
            ObjectTypeDefinition("Hello", Vector.empty,
              Vector(
                FieldDefinition("world", NotNullType(NamedType("String", Some(Position(43, 3, 20))), Some(Position(43, 3, 20))),
                  Vector.empty, Vector.empty, Vector.empty, Some(Position(36, 3, 13)))),
              Vector.empty, Vector.empty, Vector.empty, Some(Position(11, 2, 11)))),
          Vector.empty,
          Some(Position(11, 2, 11)), None))
    }

    "Simple type inheriting interface" in {
      val Success(ast) = QueryParser.parse(
        "type Hello implements World { }")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition("Hello", Vector(NamedType("World", Some(Position(22, 1, 23)))), Vector.empty, Vector.empty, Vector.empty, Vector.empty, Some(Position(0, 1, 1)))),
          Vector.empty,
          Some(Position(0, 1, 1)), None))
    }

    "Simple type inheriting multiple interfaces" in {
      val Success(ast) = QueryParser.parse(
        "type Hello implements Wo, rld { }")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            ObjectTypeDefinition("Hello",
              Vector(NamedType("Wo", Some(Position(22, 1, 23))), NamedType("rld", Some(Position(26, 1, 27)))),
              Vector.empty, Vector.empty, Vector.empty, Vector.empty, Some(Position(0, 1, 1)))),
          Vector.empty,
          Some(Position(0, 1, 1)), None))
    }

    "Double value enum" in {
      val Success(ast) = QueryParser.parse(
        "enum Hello { WO, RLD }")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            EnumTypeDefinition("Hello",
              Vector(
                EnumValueDefinition("WO", Vector.empty, Vector.empty, Some(Position(13, 1, 14))),
                EnumValueDefinition("RLD", Vector.empty, Vector.empty, Some(Position(17, 1, 18)))),
              Vector.empty, Vector.empty, Vector.empty, Some(Position(0, 1, 1)))),
          Vector.empty,
          Some(Position(0, 1, 1)), None))
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
            InterfaceTypeDefinition("Hello",
              Vector(
                FieldDefinition("world", NamedType("String", Some(Position(63, 4, 20))),
                  Vector.empty, Vector.empty, Vector.empty, Some(Position(56, 4, 13)))),
              Vector.empty,
              Vector(Comment("foo", Some(Position(11, 2, 11)))),
              Vector.empty,
              Some(Position(26, 3, 11)))),
          Vector.empty,
          Some(Position(11, 2, 11)), None))
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
                FieldDefinition("world", NamedType("String", Some(Position(140, 9, 31))), Vector(InputValueDefinition("flag", NamedType("Boolean", Some(Position(130, 9, 21))), None, Vector.empty, Vector(Comment("c3", Some(Position(87, 6, 15))), Comment("c4", Some(Position(106, 8, 15)))), Some(Position(124, 9, 15)))), Vector.empty, Vector(Comment("c2", Some(Position(50, 4, 13)))), Some(Position(66, 5, 13)))),
              Vector.empty,
              Vector(
                Comment("c1", Some(Position(11, 2, 11)))),
              Vector.empty,
              Some(Position(25, 3, 11))
            )),
          Vector.empty,
          Some(Position(11, 2, 11)),
          None
        )
      )
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
            ObjectTypeDefinition("Hello", Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(Position(109, 5, 22))),
                  Vector(
                    InputValueDefinition("flag", NamedType("Boolean", Some(Position(48, 3, 25))),
                      Some(BooleanValue(true,
                        Vector(Comment(" value comment", Some(Position(72, 4, 15)))),
                        Some(Position(102, 5, 15)))),
                      Vector.empty, Vector.empty, Some(Position(42, 3, 19)))),
                  Vector.empty, Vector.empty, Some(Position(36, 3, 13)))),
              Vector.empty, Vector.empty, Vector.empty, Some(Position(11, 2, 11)))),
          Vector.empty,
          Some(Position(11, 2, 11)), None))
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
            ObjectTypeDefinition("Hello", Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(Position(61, 3, 38))),
                  Vector(
                    InputValueDefinition("things",
                      ListType(NamedType("String", Some(Position(51, 3, 28))), Some(Position(50, 3, 27))),
                      None, Vector.empty, Vector.empty, Some(Position(42, 3, 19)))),
                  Vector.empty, Vector.empty, Some(Position(36, 3, 13)))),
              Vector.empty, Vector.empty, Vector.empty, Some(Position(11, 2, 11)))),
          Vector.empty,
          Some(Position(11, 2, 11)), None))
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
            ObjectTypeDefinition("Hello", Vector.empty,
              Vector(
                FieldDefinition("world", NamedType("String", Some(Position(73, 3, 50))),
                  Vector(
                    InputValueDefinition("argOne", NamedType("Boolean", Some(Position(50, 3, 27))), None, Vector.empty, Vector.empty, Some(Position(42, 3, 19))),
                    InputValueDefinition("argTwo", NamedType("Int", Some(Position(67, 3, 44))), None, Vector.empty, Vector.empty, Some(Position(59, 3, 36)))),
                  Vector.empty, Vector.empty, Some(Position(36, 3, 13)))),
              Vector.empty, Vector.empty, Vector.empty, Some(Position(11, 2, 11)))),
          Vector.empty,
          Some(Position(11, 2, 11)), None))
    }

    "Simple union" in {
      val Success(ast) = QueryParser.parse(
        "union Hello = World")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            UnionTypeDefinition("Hello",
              Vector(
                NamedType("World", Some(Position(14, 1, 15)))),
              Vector.empty, Vector.empty, Some(Position(0, 1, 1)))),
          Vector.empty,
          Some(Position(0, 1, 1)), None))
    }

    "Union with two types" in {
      val Success(ast) = QueryParser.parse(
        "union Hello = Wo | Rld")

      ast.copy(sourceMapper = None) should be (
        Document(
          Vector(
            UnionTypeDefinition("Hello",
              Vector(
                NamedType("Wo", Some(Position(14, 1, 15))),
                NamedType("Rld", Some(Position(19, 1, 20)))),
              Vector.empty, Vector.empty, Some(Position(0, 1, 1)))),
          Vector.empty,
          Some(Position(0, 1, 1)), None))
    }

    "Scalar" in {
      val Success(ast) = QueryParser.parse(
        "scalar Hello")

      ast.copy(sourceMapper = None) should be (
        Document(Vector(ScalarTypeDefinition("Hello", Vector.empty, Vector.empty, Some(Position(0, 1, 1)))), Vector.empty, Some(Position(0, 1, 1)), None))
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
            InputObjectTypeDefinition("Hello",
              Vector(
                InputValueDefinition("world", NamedType("String", Some(Position(44, 3, 20))),
                  None, Vector.empty, Vector.empty, Some(Position(37, 3, 13)))),
              Vector.empty, Vector.empty, Vector.empty, Some(Position(11, 2, 11)))),
          Vector.empty,
          Some(Position(11, 2, 11)), None))
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