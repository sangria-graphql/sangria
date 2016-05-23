package sangria.parser

import org.parboiled2.Position
import org.scalatest.{Matchers, WordSpec}
import sangria.ast._
import sangria.util.{FileUtil, StringMatchers}

import scala.language.postfixOps
import scala.util.Success

class SchemaParserSpec extends WordSpec with Matchers with StringMatchers {

  "QueryParser" should {
    "parse schema kitchen sink" in {
      val query = FileUtil loadQuery "schema-kitchen-sink.graphql"

      // Success(Document(List(SchemaDefinition(List(OperationTypeDefinition(Query,NamedType(QueryType,Some(Position(314,9,10))),None,Some(Position(307,9,3))), OperationTypeDefinition(Mutation,NamedType(MutationType,Some(Position(337,10,13))),None,Some(Position(327,10,3)))),List(),Some(Comment(Vector( Copyright (c) 2015, Facebook, Inc.,  All rights reserved., ,  This source code is licensed under the BSD-style license found in the,  LICENSE file in the root directory of this source tree. An additional grant,  of patent rights can be found in the PATENTS file in the same directory.),Some(Position(0,1,1)))),Some(Position(295,8,1))), ObjectTypeDefinition(Foo,List(NamedType(Bar,Some(Position(376,13,21)))),List(FieldDefinition(one,NamedType(Type,Some(Position(390,14,8))),List(),List(),None,Some(Position(385,14,3))), FieldDefinition(two,NamedType(Type,Some(Position(425,15,30))),List(InputValueDefinition(argument,NotNullType(NamedType(InputType,Some(Position(412,15,17))),Some(Position(412,15,17))),None,List(),None,Some(Position(402,15,7)))),List(),None,Some(Position(398,15,3))), FieldDefinition(three,NamedType(Int,Some(Position(476,16,46))),List(InputValueDefinition(argument,NamedType(InputType,Some(Position(449,16,19))),None,List(),None,Some(Position(439,16,9))), InputValueDefinition(other,NamedType(String,Some(Position(467,16,37))),None,List(),None,Some(Position(460,16,30)))),List(),None,Some(Position(433,16,3))), FieldDefinition(four,NamedType(String,Some(Position(518,17,38))),List(InputValueDefinition(argument,NamedType(String,Some(Position(498,17,18))),Some(StringValue(string,None,Some(Position(507,17,27)))),List(),None,Some(Position(488,17,8)))),List(),None,Some(Position(483,17,3))), FieldDefinition(five,NamedType(String,Some(Position(577,18,52))),List(InputValueDefinition(argument,ListType(NamedType(String,Some(Position(544,18,19))),Some(Position(543,18,18))),Some(ListValue(List(StringValue(string,None,Some(Position(555,18,30))), StringValue(string,None,Some(Position(565,18,40)))),None,Some(Position(554,18,29)))),List(),None,Some(Position(533,18,8)))),List(),None,Some(Position(528,18,3))), FieldDefinition(six,NamedType(Type,Some(Position(630,19,46))),List(InputValueDefinition(argument,NamedType(InputType,Some(Position(601,19,17))),Some(ObjectValue(List(ObjectField(key,StringValue(value,None,Some(Position(619,19,35))),None,Some(Position(614,19,30)))),None,Some(Position(613,19,29)))),List(),None,Some(Position(591,19,7)))),List(),None,Some(Position(587,19,3)))),List(),None,Some(Position(356,13,1))), ObjectTypeDefinition(AnnotatedObject,List(),List(FieldDefinition(annotatedField,NamedType(Type,Some(Position(737,23,49))),List(InputValueDefinition(arg,NamedType(Type,Some(Position(711,23,23))),Some(StringValue(default,None,Some(Position(718,23,30)))),List(Directive(onArg,List(),None,Some(Position(728,23,40)))),None,Some(Position(706,23,18)))),List(Directive(onField,List(),None,Some(Position(742,23,54)))),None,Some(Position(691,23,3)))),List(Directive(onObject,List(Argument(arg,StringValue(value,None,Some(Position(677,22,37))),None,Some(Position(672,22,32)))),None,Some(Position(662,22,22)))),None,Some(Position(641,22,1))), InterfaceTypeDefinition(Bar,List(FieldDefinition(one,NamedType(Type,Some(Position(803,28,8))),List(),List(),None,Some(Position(798,28,3))), FieldDefinition(four,NamedType(String,Some(Position(846,29,38))),List(InputValueDefinition(argument,NamedType(String,Some(Position(826,29,18))),Some(StringValue(string,None,Some(Position(835,29,27)))),List(),None,Some(Position(816,29,8)))),List(),None,Some(Position(811,29,3)))),List(),Some(Comment(Vector( It's an interface!),Some(Position(757,26,1)))),Some(Position(779,27,1))), InterfaceTypeDefinition(AnnotatedInterface,List(FieldDefinition(annotatedField,NamedType(Type,Some(Position(940,33,37))),List(InputValueDefinition(arg,NamedType(Type,Some(Position(926,33,23))),None,List(Directive(onArg,List(),None,Some(Position(931,33,28)))),None,Some(Position(921,33,18)))),List(Directive(onField,List(),None,Some(Position(945,33,42)))),None,Some(Position(906,33,3)))),List(Directive(onInterface,List(),None,Some(Position(888,32,30)))),None,Some(Position(859,32,1))), UnionTypeDefinition(Feed,List(NamedType(Story,Some(Position(973,36,14))), NamedType(Article,Some(Position(981,36,22))), NamedType(Advert,Some(Position(991,36,32)))),List(),None,Some(Position(960,36,1))), UnionTypeDefinition(AnnotatedUnion,List(NamedType(A,Some(Position(1033,38,33))), NamedType(B,Some(Position(1037,38,37)))),List(Directive(onUnion,List(),None,Some(Position(1022,38,22)))),None,Some(Position(1001,38,1))), ScalarTypeDefinition(CustomScalar,List(),None,Some(Position(1042,40,1))), ScalarTypeDefinition(AnnotatedScalar,List(Directive(onScalar,List(),None,Some(Position(1088,42,24)))),None,Some(Position(1065,42,1))), EnumTypeDefinition(Site,List(EnumValueDefinition(DESKTOP,List(),Some(Comment(Vector( value 1),Some(Position(1116,45,3)))),Some(Position(1129,46,3))), EnumValueDefinition(MOBILE,List(),Some(Comment(Vector( value 2),Some(Position(1140,47,3)))),Some(Position(1153,48,3)))),List(),None,Some(Position(1101,44,1))), EnumTypeDefinition(AnnotatedEnum,List(EnumValueDefinition(ANNOTATED_VALUE,List(Directive(onEnumValue,List(),None,Some(Position(1214,52,19)))),None,Some(Position(1198,52,3))), EnumValueDefinition(OTHER_VALUE,List(),None,Some(Position(1230,53,3)))),List(Directive(onEnum,List(),None,Some(Position(1185,51,20)))),None,Some(Position(1166,51,1))), InputObjectTypeDefinition(InputType,List(InputValueDefinition(key,NotNullType(NamedType(String,Some(Position(1274,57,8))),Some(Position(1274,57,8))),None,List(),None,Some(Position(1269,57,3))), InputValueDefinition(answer,NamedType(Int,Some(Position(1293,58,11))),Some(BigIntValue(42,None,Some(Position(1299,58,17)))),List(),None,Some(Position(1285,58,3)))),List(),None,Some(Position(1248,56,1))), InputObjectTypeDefinition(AnnotatedInput,List(InputValueDefinition(annotatedField,NamedType(Type,Some(Position(1388,63,19))),None,List(Directive(onField,List(),None,Some(Position(1393,63,24)))),Some(Comment(Vector( field comment),Some(Position(1353,62,3)))),Some(Position(1372,63,3)))),List(Directive(onInputObjectType,List(),None,Some(Position(1329,61,22)))),None,Some(Position(1308,61,1))), TypeExtensionDefinition(ObjectTypeDefinition(Foo,List(),List(FieldDefinition(seven,NamedType(Type,Some(Position(1456,67,30))),List(InputValueDefinition(argument,ListType(NamedType(String,Some(Position(1446,67,20))),Some(Position(1445,67,19))),None,List(),None,Some(Position(1435,67,9)))),List(),None,Some(Position(1429,67,3)))),List(),None,Some(Position(1415,66,8))),None,Some(Position(1408,66,1))), TypeExtensionDefinition(ObjectTypeDefinition(Foo,List(),List(),List(Directive(onType,List(),None,Some(Position(1483,70,17)))),None,Some(Position(1474,70,8))),None,Some(Position(1467,70,1))), ObjectTypeDefinition(NoFields,List(),List(),List(),None,Some(Position(1497,72,1))), DirectiveDefinition(skip,List(InputValueDefinition(if,NotNullType(NamedType(Boolean,Some(Position(1537,74,21))),Some(Position(1537,74,21))),None,List(),None,Some(Position(1533,74,17)))),List(DirectiveLocation(FIELD,None,Some(Position(1550,74,34))), DirectiveLocation(FRAGMENT_SPREAD,None,Some(Position(1558,74,42))), DirectiveLocation(INLINE_FRAGMENT,None,Some(Position(1576,74,60)))),None,Some(Position(1517,74,1))), DirectiveDefinition(include,List(InputValueDefinition(if,NotNullType(NamedType(Boolean,Some(Position(1618,76,24))),Some(Position(1618,76,24))),None,List(),None,Some(Position(1614,76,20)))),List(DirectiveLocation(FIELD,None,Some(Position(1634,77,6))), DirectiveLocation(FRAGMENT_SPREAD,None,Some(Position(1646,78,6))), DirectiveLocation(INLINE_FRAGMENT,None,Some(Position(1668,79,6)))),None,Some(Position(1595,76,1)))),Some(Position(0,1,1)),None))
      // Success(Document(List(SchemaDefinition(List(OperationTypeDefinition(Query,NamedType(QueryType,Some(Position(306,9,10))),None,Some(Position(299,9,3))), OperationTypeDefinition(Mutation,NamedType(MutationType,Some(Position(328,10,13))),None,Some(Position(318,10,3)))),List(),Some(Comment(Vector( Copyright (c) 2015, Facebook, Inc.,  All rights reserved., ,  This source code is licensed under the BSD-style license found in the,  LICENSE file in the root directory of this source tree. An additional grant,  of patent rights can be found in the PATENTS file in the same directory.),Some(Position(0,1,1)))),Some(Position(288,8,1))), ObjectTypeDefinition(Foo,List(NamedType(Bar,Some(Position(364,13,21)))),List(FieldDefinition(one,NamedType(Type,Some(Position(377,14,8))),List(),List(),None,Some(Position(372,14,3))), FieldDefinition(two,NamedType(Type,Some(Position(411,15,30))),List(InputValueDefinition(argument,NotNullType(NamedType(InputType,Some(Position(398,15,17))),Some(Position(398,15,17))),None,List(),None,Some(Position(388,15,7)))),List(),None,Some(Position(384,15,3))), FieldDefinition(three,NamedType(Int,Some(Position(461,16,46))),List(InputValueDefinition(argument,NamedType(InputType,Some(Position(434,16,19))),None,List(),None,Some(Position(424,16,9))), InputValueDefinition(other,NamedType(String,Some(Position(452,16,37))),None,List(),None,Some(Position(445,16,30)))),List(),None,Some(Position(418,16,3))), FieldDefinition(four,NamedType(String,Some(Position(502,17,38))),List(InputValueDefinition(argument,NamedType(String,Some(Position(482,17,18))),Some(StringValue(string,None,Some(Position(491,17,27)))),List(),None,Some(Position(472,17,8)))),List(),None,Some(Position(467,17,3))), FieldDefinition(five,NamedType(String,Some(Position(560,18,52))),List(InputValueDefinition(argument,ListType(NamedType(String,Some(Position(527,18,19))),Some(Position(526,18,18))),Some(ListValue(List(StringValue(string,None,Some(Position(538,18,30))), StringValue(string,None,Some(Position(548,18,40)))),None,Some(Position(537,18,29)))),List(),None,Some(Position(516,18,8)))),List(),None,Some(Position(511,18,3))), FieldDefinition(six,NamedType(Type,Some(Position(612,19,46))),List(InputValueDefinition(argument,NamedType(InputType,Some(Position(583,19,17))),Some(ObjectValue(List(ObjectField(key,StringValue(value,None,Some(Position(601,19,35))),None,Some(Position(596,19,30)))),None,Some(Position(595,19,29)))),List(),None,Some(Position(573,19,7)))),List(),None,Some(Position(569,19,3)))),List(),None,Some(Position(344,13,1))), ObjectTypeDefinition(AnnotatedObject,List(),List(FieldDefinition(annotatedField,NamedType(Type,Some(Position(715,23,49))),List(InputValueDefinition(arg,NamedType(Type,Some(Position(689,23,23))),Some(StringValue(default,None,Some(Position(696,23,30)))),List(Directive(onArg,List(),None,Some(Position(706,23,40)))),None,Some(Position(684,23,18)))),List(Directive(onField,List(),None,Some(Position(720,23,54)))),None,Some(Position(669,23,3)))),List(Directive(onObject,List(Argument(arg,StringValue(value,None,Some(Position(656,22,37))),None,Some(Position(651,22,32)))),None,Some(Position(641,22,22)))),None,Some(Position(620,22,1))), InterfaceTypeDefinition(Bar,List(FieldDefinition(one,NamedType(Type,Some(Position(776,28,8))),List(),List(),None,Some(Position(771,28,3))), FieldDefinition(four,NamedType(String,Some(Position(818,29,38))),List(InputValueDefinition(argument,NamedType(String,Some(Position(798,29,18))),Some(StringValue(string,None,Some(Position(807,29,27)))),List(),None,Some(Position(788,29,8)))),List(),None,Some(Position(783,29,3)))),List(),Some(Comment(Vector( It's an interface!),Some(Position(732,26,1)))),Some(Position(753,27,1))), InterfaceTypeDefinition(AnnotatedInterface,List(FieldDefinition(annotatedField,NamedType(Type,Some(Position(908,33,37))),List(InputValueDefinition(arg,NamedType(Type,Some(Position(894,33,23))),None,List(Directive(onArg,List(),None,Some(Position(899,33,28)))),None,Some(Position(889,33,18)))),List(Directive(onField,List(),None,Some(Position(913,33,42)))),None,Some(Position(874,33,3)))),List(Directive(onInterface,List(),None,Some(Position(857,32,30)))),None,Some(Position(828,32,1))), UnionTypeDefinition(Feed,List(NamedType(Story,Some(Position(938,36,14))), NamedType(Article,Some(Position(946,36,22))), NamedType(Advert,Some(Position(956,36,32)))),List(),None,Some(Position(925,36,1))), UnionTypeDefinition(AnnotatedUnion,List(NamedType(A,Some(Position(996,38,33))), NamedType(B,Some(Position(1000,38,37)))),List(Directive(onUnion,List(),None,Some(Position(985,38,22)))),None,Some(Position(964,38,1))), ScalarTypeDefinition(CustomScalar,List(),None,Some(Position(1003,40,1))), ScalarTypeDefinition(AnnotatedScalar,List(Directive(onScalar,List(),None,Some(Position(1047,42,24)))),None,Some(Position(1024,42,1))), EnumTypeDefinition(Site,List(EnumValueDefinition(DESKTOP,List(),Some(Comment(Vector( value 1),Some(Position(1072,45,3)))),Some(Position(1084,46,3))), EnumValueDefinition(MOBILE,List(),Some(Comment(Vector( value 2),Some(Position(1094,47,3)))),Some(Position(1106,48,3)))),List(),None,Some(Position(1058,44,1))), EnumTypeDefinition(AnnotatedEnum,List(EnumValueDefinition(ANNOTATED_VALUE,List(Directive(onEnumValue,List(),None,Some(Position(1163,52,19)))),None,Some(Position(1147,52,3))), EnumValueDefinition(OTHER_VALUE,List(),None,Some(Position(1178,53,3)))),List(Directive(onEnum,List(),None,Some(Position(1135,51,20)))),None,Some(Position(1116,51,1))), InputObjectTypeDefinition(InputType,List(InputValueDefinition(key,NotNullType(NamedType(String,Some(Position(1218,57,8))),Some(Position(1218,57,8))),None,List(),None,Some(Position(1213,57,3))), InputValueDefinition(answer,NamedType(Int,Some(Position(1236,58,11))),Some(BigIntValue(42,None,Some(Position(1242,58,17)))),List(),None,Some(Position(1228,58,3)))),List(),None,Some(Position(1193,56,1))), InputObjectTypeDefinition(AnnotatedInput,List(InputValueDefinition(annotatedField,NamedType(Type,Some(Position(1326,63,19))),None,List(Directive(onField,List(),None,Some(Position(1331,63,24)))),Some(Comment(Vector( field comment),Some(Position(1292,62,3)))),Some(Position(1310,63,3)))),List(Directive(onInputObjectType,List(),None,Some(Position(1269,61,22)))),None,Some(Position(1248,61,1))), TypeExtensionDefinition(ObjectTypeDefinition(Foo,List(),List(FieldDefinition(seven,NamedType(Type,Some(Position(1390,67,30))),List(InputValueDefinition(argument,ListType(NamedType(String,Some(Position(1380,67,20))),Some(Position(1379,67,19))),None,List(),None,Some(Position(1369,67,9)))),List(),None,Some(Position(1363,67,3)))),List(),None,Some(Position(1350,66,8))),None,Some(Position(1343,66,1))), TypeExtensionDefinition(ObjectTypeDefinition(Foo,List(),List(),List(Directive(onType,List(),None,Some(Position(1414,70,17)))),None,Some(Position(1405,70,8))),None,Some(Position(1398,70,1))), ObjectTypeDefinition(NoFields,List(),List(),List(),None,Some(Position(1426,72,1))), DirectiveDefinition(skip,List(InputValueDefinition(if,NotNullType(NamedType(Boolean,Some(Position(1464,74,21))),Some(Position(1464,74,21))),None,List(),None,Some(Position(1460,74,17)))),List(DirectiveLocation(FIELD,None,Some(Position(1477,74,34))), DirectiveLocation(FRAGMENT_SPREAD,None,Some(Position(1485,74,42))), DirectiveLocation(INLINE_FRAGMENT,None,Some(Position(1503,74,60)))),None,Some(Position(1444,74,1))), DirectiveDefinition(include,List(InputValueDefinition(if,NotNullType(NamedType(Boolean,Some(Position(1543,76,24))),Some(Position(1543,76,24))),None,List(),None,Some(Position(1539,76,20)))),List(DirectiveLocation(FIELD,None,Some(Position(1558,77,6))), DirectiveLocation(FRAGMENT_SPREAD,None,Some(Position(1569,78,6))), DirectiveLocation(INLINE_FRAGMENT,None,Some(Position(1590,79,6)))),None,Some(Position(1520,76,1)))),Some(Position(0,1,1)),None))

      val expectedAst =
        Document(
          List(
            SchemaDefinition(
              List(
                OperationTypeDefinition(OperationType.Query, NamedType("QueryType", Some(Position(306, 9, 10))),
                  None, Some(Position(299, 9, 3))),
                OperationTypeDefinition(OperationType.Mutation, NamedType("MutationType", Some(Position(328, 10, 13))),
                  None, Some(Position(318, 10, 3)))),
              Nil,
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

            ObjectTypeDefinition("Foo", List(NamedType("Bar", Some(Position(364, 13, 21)))),
              List(
                FieldDefinition("one", NamedType("Type", Some(Position(377, 14, 8))),
                  Nil, Nil, None, Some(Position(372, 14, 3))),
                FieldDefinition("two", NamedType("Type", Some(Position(411, 15, 30))),
                  List(
                    InputValueDefinition("argument", NotNullType(NamedType("InputType", Some(Position(398, 15, 17))), Some(Position(398, 15, 17))),
                      None, Nil, None, Some(Position(388, 15, 7)))),
                  Nil, None, Some(Position(384, 15, 3))),
                FieldDefinition("three", NamedType("Int", Some(Position(461, 16, 46))),
                  List(
                    InputValueDefinition("argument", NamedType("InputType", Some(Position(434, 16, 19))),
                      None, Nil, None, Some(Position(424, 16, 9))),
                    InputValueDefinition("other", NamedType("String", Some(Position(452, 16, 37))),
                      None, Nil, None, Some(Position(445, 16, 30)))),
                  Nil, None, Some(Position(418, 16, 3))),
                FieldDefinition("four", NamedType("String", Some(Position(502, 17, 38))),
                  List(
                    InputValueDefinition("argument", NamedType("String", Some(Position(482, 17, 18))),
                      Some(StringValue("string", None, Some(Position(491, 17, 27)))),
                      Nil, None, Some(Position(472, 17, 8)))),
                  Nil, None, Some(Position(467, 17, 3))),
                FieldDefinition("five", NamedType("String", Some(Position(560, 18, 52))),
                  List(
                    InputValueDefinition("argument", ListType(NamedType("String", Some(Position(527, 18, 19))), Some(Position(526, 18, 18))),
                      Some(
                        ListValue(
                          List(
                            StringValue("string", None, Some(Position(538, 18, 30))),
                            StringValue("string", None, Some(Position(548, 18, 40)))),
                          None, Some(Position(537, 18, 29)))),
                      Nil, None, Some(Position(516, 18, 8)))),
                  Nil, None, Some(Position(511, 18, 3))),
                FieldDefinition("six", NamedType("Type", Some(Position(612, 19, 46))),
                  List(
                    InputValueDefinition("argument", NamedType("InputType", Some(Position(583, 19, 17))),
                      Some(
                        ObjectValue(
                          List(
                            ObjectField("key", StringValue("value", None, Some(Position(601, 19, 35))),
                              None, Some(Position(596, 19, 30)))),
                          None, Some(Position(595, 19, 29)))),
                      Nil, None, Some(Position(573, 19, 7)))),
                  Nil, None, Some(Position(569, 19, 3)))),
              Nil, None, Some(Position(344, 13, 1))),

            ObjectTypeDefinition("AnnotatedObject", Nil,
              List(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(715, 23, 49))),
                  List(
                    InputValueDefinition("arg", NamedType("Type", Some(Position(689, 23, 23))),
                      Some(StringValue("default", None, Some(Position(696, 23, 30)))),
                      List(Directive("onArg", Nil, None, Some(Position(706, 23, 40)))),
                      None, Some(Position(684, 23, 18)))),
                  List(Directive("onField", Nil, None, Some(Position(720, 23, 54)))),
                  None, Some(Position(669, 23, 3)))),
              List(Directive("onObject",
                List(Argument("arg", StringValue("value", None, Some(Position(656, 22, 37))), None, Some(Position(651, 22, 32)))),
                None, Some(Position(641, 22, 22)))),
              None, Some(Position(620, 22, 1))),

            InterfaceTypeDefinition("Bar",
              List(
                FieldDefinition("one", NamedType("Type", Some(Position(776, 28, 8))),
                  Nil, Nil, None, Some(Position(771, 28, 3))),
                FieldDefinition("four", NamedType("String", Some(Position(818, 29, 38))),
                  List(
                    InputValueDefinition("argument", NamedType("String", Some(Position(798, 29, 18))),
                      Some(StringValue("string", None, Some(Position(807, 29, 27)))),
                      Nil, None, Some(Position(788, 29, 8)))),
                  Nil, None, Some(Position(783, 29, 3)))),
              Nil,
              Some(Comment(Vector(" It's an interface!"), Some(Position(732, 26, 1)))),
              Some(Position(753, 27, 1))),

            InterfaceTypeDefinition("AnnotatedInterface",
              List(
                FieldDefinition("annotatedField", NamedType("Type", Some(Position(908, 33, 37))),
                  List(
                    InputValueDefinition("arg", NamedType("Type", Some(Position(894, 33, 23))),
                      None,
                      List(Directive("onArg", Nil, None, Some(Position(899, 33, 28)))),
                      None, Some(Position(889, 33, 18)))),
                  List(Directive("onField", Nil, None, Some(Position(913, 33, 42)))),
                  None, Some(Position(874, 33, 3)))),
              List(Directive("onInterface", Nil, None, Some(Position(857, 32, 30)))),
              None, Some(Position(828, 32, 1))),

            UnionTypeDefinition("Feed",
              List(
                NamedType("Story", Some(Position(938, 36, 14))),
                NamedType("Article", Some(Position(946, 36, 22))),
                NamedType("Advert", Some(Position(956, 36, 32)))),
              Nil, None, Some(Position(925, 36, 1))),

            UnionTypeDefinition("AnnotatedUnion",
              List(
                NamedType("A", Some(Position(996, 38, 33))),
                NamedType("B", Some(Position(1000, 38, 37)))),
              List(Directive("onUnion", Nil, None, Some(Position(985, 38, 22)))),
              None, Some(Position(964, 38, 1))),

            ScalarTypeDefinition("CustomScalar", Nil, None, Some(Position(1003, 40, 1))),

            ScalarTypeDefinition("AnnotatedScalar",
              List(Directive("onScalar", Nil, None, Some(Position(1047, 42, 24)))),
              None, Some(Position(1024, 42, 1))),

            EnumTypeDefinition("Site",
              List(
                EnumValueDefinition("DESKTOP", Nil,
                  Some(Comment(Vector(" value 1"), Some(Position(1072, 45, 3)))),
                  Some(Position(1084, 46, 3))),
                EnumValueDefinition("MOBILE", Nil,
                  Some(Comment(Vector(" value 2"), Some(Position(1094, 47, 3)))),
                  Some(Position(1106, 48, 3)))),
              Nil, None, Some(Position(1058, 44, 1))),

            EnumTypeDefinition("AnnotatedEnum",
              List(
                EnumValueDefinition("ANNOTATED_VALUE",
                  List(Directive("onEnumValue", Nil, None, Some(Position(1163, 52, 19)))),
                  None, Some(Position(1147, 52, 3))),
                EnumValueDefinition("OTHER_VALUE", Nil, None, Some(Position(1178, 53, 3)))),
              List(Directive("onEnum", Nil, None, Some(Position(1135, 51, 20)))),
              None, Some(Position(1116, 51, 1))),

            InputObjectTypeDefinition("InputType",
              List(
                InputValueDefinition("key", NotNullType(NamedType("String", Some(Position(1218, 57, 8))), Some(Position(1218, 57, 8))),
                  None, Nil, None, Some(Position(1213, 57, 3))),
                InputValueDefinition("answer", NamedType("Int", Some(Position(1236, 58, 11))),
                  Some(BigIntValue(42, None, Some(Position(1242, 58, 17)))),
                  Nil, None, Some(Position(1228, 58, 3)))),
              Nil, None, Some(Position(1193, 56, 1))),

            InputObjectTypeDefinition("AnnotatedInput",
              List(
                InputValueDefinition("annotatedField", NamedType("Type", Some(Position(1326, 63, 19))), None,
                  List(Directive("onField", Nil, None, Some(Position(1331, 63, 24)))),
                  Some(Comment(Vector(" field comment"), Some(Position(1292, 62, 3)))),
                  Some(Position(1310, 63, 3)))),
              List(Directive("onInputObjectType", Nil, None, Some(Position(1269, 61, 22)))),
              None, Some(Position(1248, 61, 1))),

            TypeExtensionDefinition(
              ObjectTypeDefinition("Foo", Nil,
                List(
                  FieldDefinition("seven", NamedType("Type", Some(Position(1390, 67, 30))),
                    List(
                      InputValueDefinition("argument", ListType(NamedType("String", Some(Position(1380, 67, 20))), Some(Position(1379, 67, 19))),
                        None, Nil, None, Some(Position(1369, 67, 9)))),
                    Nil, None, Some(Position(1363, 67, 3)))),
                Nil, None, Some(Position(1350, 66, 8))),
              None, Some(Position(1343, 66, 1))),

            TypeExtensionDefinition(
              ObjectTypeDefinition("Foo", Nil, Nil,
                List(Directive("onType", Nil, None, Some(Position(1414, 70, 17)))),
                None, Some(Position(1405, 70, 8))),
              None, Some(Position(1398, 70, 1))),

            ObjectTypeDefinition("NoFields", Nil, Nil, Nil, None, Some(Position(1426, 72, 1))),

            DirectiveDefinition("skip",
              List(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(1464, 74, 21))), Some(Position(1464, 74, 21))),
                  None, Nil, None, Some(Position(1460, 74, 17)))),
              List(
                DirectiveLocation("FIELD", None, Some(Position(1477, 74, 34))),
                DirectiveLocation("FRAGMENT_SPREAD", None, Some(Position(1485, 74, 42))),
                DirectiveLocation("INLINE_FRAGMENT", None, Some(Position(1503, 74, 60)))),
              None, Some(Position(1444, 74, 1))),

            DirectiveDefinition("include",
              List(
                InputValueDefinition("if", NotNullType(NamedType("Boolean", Some(Position(1543, 76, 24))), Some(Position(1543, 76, 24))),
                  None, Nil, None, Some(Position(1539, 76, 20)))),
              List(
                DirectiveLocation("FIELD", None, Some(Position(1558, 77, 6))),
                DirectiveLocation("FRAGMENT_SPREAD", None, Some(Position(1569, 78, 6))),
                DirectiveLocation("INLINE_FRAGMENT", None, Some(Position(1590, 79, 6)))),
              None, Some(Position(1520, 76, 1)))),
          Some(Position(0, 1, 1)), None)

      QueryParser.parse(query) map (_.copy(sourceMapper = None)) should be(Success(expectedAst))
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

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            ObjectTypeDefinition("Hello", Nil,
              List(
                FieldDefinition("world", NamedType("String", Some(Position(123, 6, 20))), Nil, Nil,
                  Some(Comment(Vector(" and field comment as well"), Some(Position(76, 5, 13)))),
                  Some(Position(116, 6, 13)))),
              Nil,
              Some(Comment(Vector(" my type", " comment"), Some(Position(11, 2, 11)))),
              Some(Position(51, 4, 11)))),
          Some(Position(11, 2, 11)),
          None)
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

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            TypeExtensionDefinition(
              ObjectTypeDefinition("Hello", Nil,
                List(FieldDefinition("world", NamedType("String", Some(Position(90, 5, 20))), Nil, Nil, None, Some(Position(83, 5, 13)))),
                Nil, None, Some(Position(58, 4, 18))),
              Some(Comment(Vector(" my type", " comment"), Some(Position(11, 2, 11)))),
              Some(Position(51, 4, 11)))),
          Some(Position(11, 2, 11)), None))
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
          List(
            ObjectTypeDefinition("Hello", Nil,
              List(
                FieldDefinition("world", NotNullType(NamedType("String", Some(Position(43, 3, 20))), Some(Position(43, 3, 20))),
                  Nil, Nil, None, Some(Position(36, 3, 13)))),
              Nil, None, Some(Position(11, 2, 11)))),
          Some(Position(11, 2, 11)), None))
    }

    "Simple type inheriting interface" in {
      val Success(ast) = QueryParser.parse(
        "type Hello implements World { }")

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            ObjectTypeDefinition("Hello", List(NamedType("World", Some(Position(22, 1, 23)))), Nil, Nil, None, Some(Position(0, 1, 1)))),
          Some(Position(0, 1, 1)), None))
    }

    "Simple type inheriting multiple interfaces" in {
      val Success(ast) = QueryParser.parse(
        "type Hello implements Wo, rld { }")

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            ObjectTypeDefinition("Hello",
              List(NamedType("Wo", Some(Position(22, 1, 23))), NamedType("rld", Some(Position(26, 1, 27)))),
              Nil, Nil, None, Some(Position(0, 1, 1)))),
          Some(Position(0, 1, 1)), None))
    }

    "Double value enum" in {
      val Success(ast) = QueryParser.parse(
        "enum Hello { WO, RLD }")

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            EnumTypeDefinition("Hello",
              List(
                EnumValueDefinition("WO", Nil, None, Some(Position(13, 1, 14))),
                EnumValueDefinition("RLD", Nil, None, Some(Position(17, 1, 18)))),
              Nil, None, Some(Position(0, 1, 1)))),
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
          List(
            InterfaceTypeDefinition("Hello",
              List(
                FieldDefinition("world", NamedType("String", Some(Position(63, 4, 20))),
                  Nil, Nil, None, Some(Position(56, 4, 13)))),
              Nil,
              Some(Comment(Vector("foo"), Some(Position(11, 2, 11)))),
              Some(Position(26, 3, 11)))),
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

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            ObjectTypeDefinition("Hello", Nil,
              List(
                FieldDefinition("world", NamedType("String", Some(Position(140, 9, 31))),
                  List(
                    InputValueDefinition("flag", NamedType("Boolean", Some(Position(130, 9, 21))), None, Nil,
                      Some(Comment(Vector("c3", "c4"), Some(Position(87, 6, 15)))),
                      Some(Position(124, 9, 15)))),
                  Nil,
                  Some(Comment(Vector("c2"), Some(Position(50, 4, 13)))),
                  Some(Position(66, 5, 13)))),
              Nil, Some(Comment(Vector("c1"), Some(Position(11, 2, 11)))),
              Some(Position(25, 3, 11)))),
          Some(Position(11, 2, 11)), None))
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
          List(
            ObjectTypeDefinition("Hello", Nil,
              List(
                FieldDefinition("world", NamedType("String", Some(Position(109, 5, 22))),
                  List(
                    InputValueDefinition("flag", NamedType("Boolean", Some(Position(48, 3, 25))),
                      Some(BooleanValue(true,
                        Some(Comment(Vector(" value comment"), Some(Position(72, 4, 15)))),
                        Some(Position(102, 5, 15)))),
                      Nil, None, Some(Position(42, 3, 19)))),
                  Nil, None, Some(Position(36, 3, 13)))),
              Nil, None, Some(Position(11, 2, 11)))),
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
          List(
            ObjectTypeDefinition("Hello", Nil,
              List(
                FieldDefinition("world", NamedType("String", Some(Position(61, 3, 38))),
                  List(
                    InputValueDefinition("things",
                      ListType(NamedType("String", Some(Position(51, 3, 28))), Some(Position(50, 3, 27))),
                      None, Nil, None, Some(Position(42, 3, 19)))),
                  Nil, None, Some(Position(36, 3, 13)))),
              Nil, None, Some(Position(11, 2, 11)))),
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
          List(
            ObjectTypeDefinition("Hello", Nil,
              List(
                FieldDefinition("world", NamedType("String", Some(Position(73, 3, 50))),
                  List(
                    InputValueDefinition("argOne", NamedType("Boolean", Some(Position(50, 3, 27))), None, Nil, None, Some(Position(42, 3, 19))),
                    InputValueDefinition("argTwo", NamedType("Int", Some(Position(67, 3, 44))), None, Nil, None, Some(Position(59, 3, 36)))),
                  Nil, None, Some(Position(36, 3, 13)))),
              Nil, None, Some(Position(11, 2, 11)))),
          Some(Position(11, 2, 11)), None))
    }

    "Simple union" in {
      val Success(ast) = QueryParser.parse(
        "union Hello = World")

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            UnionTypeDefinition("Hello",
              List(
                NamedType("World", Some(Position(14, 1, 15)))),
              Nil, None, Some(Position(0, 1, 1)))),
          Some(Position(0, 1, 1)), None))
    }

    "Union with two types" in {
      val Success(ast) = QueryParser.parse(
        "union Hello = Wo | Rld")

      ast.copy(sourceMapper = None) should be (
        Document(
          List(
            UnionTypeDefinition("Hello",
              List(
                NamedType("Wo", Some(Position(14, 1, 15))),
                NamedType("Rld", Some(Position(19, 1, 20)))),
              Nil, None, Some(Position(0, 1, 1)))),
          Some(Position(0, 1, 1)), None))
    }

    "Scalar" in {
      val Success(ast) = QueryParser.parse(
        "scalar Hello")

      ast.copy(sourceMapper = None) should be (
        Document(List(ScalarTypeDefinition("Hello", Nil, None, Some(Position(0, 1, 1)))), Some(Position(0, 1, 1)), None))
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
          List(
            InputObjectTypeDefinition("Hello",
              List(
                InputValueDefinition("world", NamedType("String", Some(Position(44, 3, 20))),
                  None, Nil, None, Some(Position(37, 3, 13)))),
              Nil, None, Some(Position(11, 2, 11)))),
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