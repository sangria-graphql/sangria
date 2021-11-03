package sangria.schema

import sangria.ast
import sangria.execution.WithViolations
import sangria.validation._
import sangria.macros._
import sangria.macros.derive.{
  InputObjectTypeName,
  ObjectTypeName,
  deriveInputObjectType,
  deriveObjectType
}
import sangria.util.Pos

import scala.util.{Failure, Success, Try}
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SchemaConstraintsSpec extends AnyWordSpec with Matchers {
  "Schema" should {
    "not allow use same type name for different GraphQL type kinds (input & output type)" in {
      val inputType =
        InputObjectType("Point", List(InputField("x", FloatType), InputField("y", FloatType)))

      val outputType = ObjectType(
        "Point",
        fields[Unit, Unit](
          Field("x", FloatType, resolve = _ => 1.234),
          Field("y", FloatType, resolve = _ => 1.234),
          Field("z", FloatType, resolve = _ => 1.234)))

      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field(
            "foo",
            OptionType(outputType),
            arguments = Argument("points", ListInputType(inputType)) :: Nil,
            resolve = _ => None)))

      val error = intercept[SchemaValidationException](Schema(queryType))

      error.getMessage should include(
        "Type name 'Point' is used for several conflicting GraphQL type kinds: ObjectType, InputObjectType. Conflict found in an argument 'points' defined in field 'foo' of 'Query' type.")
    }

    "not allow use same type name for different GraphQL type kinds (input & scalar type)" in {
      val inputType =
        InputObjectType("Point", List(InputField("x", FloatType), InputField("y", FloatType)))

      val scalarType = ScalarType[String](
        "Point",
        coerceOutput = valueOutput,
        coerceUserInput = {
          case s: String => Right(s)
          case _ => Left(StringCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _, _, _, _) => Right(s)
          case _ => Left(StringCoercionViolation)
        }
      )

      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field(
            "foo",
            OptionType(scalarType),
            arguments = Argument("points", ListInputType(inputType)) :: Nil,
            resolve = _ => None)))

      val error = intercept[SchemaValidationException](Schema(queryType))

      error.getMessage should include(
        "Type name 'Point' is used for several conflicting GraphQL type kinds: ScalarType, InputObjectType. Conflict found in an argument 'points' defined in field 'foo' of 'Query' type.")
    }

    "not allow reserved names" in {
      val inputType =
        InputObjectType("__Input", List(InputField("x", FloatType), InputField("__y", FloatType)))

      val scalarType = ScalarType[String](
        "__Point",
        coerceOutput = valueOutput,
        coerceUserInput = {
          case s: String => Right(s)
          case _ => Left(StringCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _, _, _, _) => Right(s)
          case _ => Left(StringCoercionViolation)
        }
      )

      val bazType =
        InterfaceType("__Baz", fields[Unit, Unit](Field("id", IntType, resolve = _ => 1)))

      val barType = ObjectType(
        "__Bar",
        interfaces[Unit, Unit](bazType),
        fields[Unit, Unit](Field("foo", OptionType(scalarType), resolve = _ => None)))

      val colorType = EnumType(
        "__Color",
        values = List(
          EnumValue("RED", value = 1),
          EnumValue("__GREEN", value = 2),
          EnumValue("__BLUE", value = 3)))

      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("__foo", OptionType(scalarType), resolve = _ => None),
          Field("bar", OptionType(barType), resolve = _ => None),
          Field("color", OptionType(colorType), resolve = _ => None)
        )
      )

      val error =
        intercept[SchemaValidationException](Schema(queryType, additionalTypes = inputType :: Nil))

      error.violations.map(_.errorMessage).toSet should be(Set(
        "Input type name '__Input' is invalid. The name is reserved for GraphQL introspection API.",
        "Field name '__y' defined in input type '__Input' is invalid. The name is reserved for GraphQL introspection API.",
        "Field name '__foo' defined in type 'Query' is invalid. The name is reserved for GraphQL introspection API.",
        "Object type name '__Bar' is invalid. The name is reserved for GraphQL introspection API.",
        "Interface type name '__Baz' is invalid. The name is reserved for GraphQL introspection API.",
        "Enum type name '__Color' is invalid. The name is reserved for GraphQL introspection API.",
        "Enum value name '__GREEN' defined in enum type '__Color' is invalid. The name is reserved for GraphQL introspection API.",
        "Enum value name '__BLUE' defined in enum type '__Color' is invalid. The name is reserved for GraphQL introspection API.",
        "Scalar type name '__Point' is invalid. The name is reserved for GraphQL introspection API."
      ))
    }

    "reject an Enum type with incorrectly named values" in {
      val colorType = EnumType(
        "Color",
        values = List(
          EnumValue("RED", value = 1),
          EnumValue("true", value = 2),
          EnumValue("false", value = 3),
          EnumValue("null", value = 4)))

      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](Field("color", OptionType(colorType), resolve = _ => None)))

      val error = intercept[SchemaValidationException](Schema(queryType))

      error.violations.map(_.errorMessage).toSet should be(
        Set(
          "Name 'Color.true' can not be used as an Enum value.",
          "Name 'Color.false' can not be used as an Enum value.",
          "Name 'Color.null' can not be used as an Enum value."
        ))
    }

    "not allow empty list of fields" in {
      val int1Type = InterfaceType[Unit, Unit]("Interface1", Nil)
      val int2Type = InterfaceType[Unit, Unit]("Interface2", Nil, interfaces[Unit, Unit](int1Type))
      val outType = ObjectType[Unit, Unit]("Output", interfaces[Unit, Unit](int2Type), Nil)
      val inputType = InputObjectType("Input", Nil)
      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field(
            "foo",
            OptionType(outType),
            arguments = Argument("bar", inputType) :: Nil,
            resolve = _ => ())))

      val error = intercept[SchemaValidationException](Schema(queryType))

      error.violations.map(_.errorMessage).toSet should be(
        Set(
          "Input type 'Input' must define one or more fields.",
          "Interface type 'Interface1' must define one or more fields.",
          "Interface type 'Interface2' must define one or more fields.",
          "Object type 'Output' must define one or more fields."
        ))
    }

    "Not allow ObjectTypes with same name to be based on different case classes" in {
      val fooBazType = deriveObjectType[Unit, test.foo.Baz]()
      val barBazType = deriveObjectType[Unit, test.bar.Baz]()

      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("fooBaz", OptionType(fooBazType), resolve = _ => Some(test.foo.Baz(1))),
          Field("barBaz", barBazType, resolve = _ => test.bar.Baz("2", 3.0))
        )
      )

      val error = intercept[SchemaValidationException](Schema(queryType))

      error.getMessage should include(
        """Type name 'Baz' is used for several conflicting GraphQL ObjectTypes based on different classes. Conflict found in a field 'barBaz' of 'Query' type. One possible fix is to use ObjectTypeName like this: deriveObjectType[Foo, Bar](ObjectTypeName("OtherBar")) to avoid that two ObjectTypes have the same name.""")
    }

    "Not allow InputObjectTypes with same name but with different field names" in {
      val fooBazType: InputObjectType[test.foo.Baz] =
        deriveInputObjectType(InputObjectTypeName("baz"))
      val barBazType: InputObjectType[test.bar.Baz] =
        deriveInputObjectType(InputObjectTypeName("baz"))

      val draftType = InputObjectType(
        name = "DraftType",
        fields = List(
          InputField("fooBaz", OptionInputType(fooBazType)),
          InputField("barBaz", barBazType)
        )
      )

      val draft = Argument("draft", draftType)

      val mutationType = ObjectType(
        "Mutation",
        fields[Unit, Unit](
          Field("nothing", StringType, arguments = draft :: Nil, resolve = _ => "hello")
        )
      )
      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("nothing", StringType, resolve = _ => "hello")
        ))

      val error = intercept[SchemaValidationException](Schema(queryType, Some(mutationType)))

      error.getMessage should include(
        """Type name 'baz' is used for several conflicting GraphQL InputObjectTypes based on different classes. Conflict found in a field 'barBaz' of 'DraftType' input object type. One possible fix is to use InputObjectTypeName like this: deriveInputObjectType[Foo, Bar](InputObjectTypeName("OtherBar")) to avoid that two InputObjectTypes have the same name.""")
    }

    "Allow ObjectTypes based on different case classes but with different names" in {
      implicit val fooBazType: ObjectType[Unit, test.foo.Baz] = deriveObjectType[Unit, test.foo.Baz]()
      implicit val barBazType: ObjectType[Unit, test.bar.Baz] =
        deriveObjectType[Unit, test.bar.Baz](ObjectTypeName("BazWithNewName"))

      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](
          Field("fooBaz", OptionType(fooBazType), resolve = _ => Some(test.foo.Baz(1))),
          Field("barBaz", barBazType, resolve = _ => test.bar.Baz("2", 3.0))
        )
      )

      Schema(queryType) // Should not throw any SchemaValidationExceptions
    }
  }

  "Type System: Union types must be valid" should {
    "accepts a Union type with member types" in validSchema(graphql"""
        type Query {
          test: GoodUnion
        }

        type TypeA {
          field: String
        }

        type TypeB {
          field: String
        }

        union GoodUnion =
          | TypeA
          | TypeB
      """)

    "rejects a Union type with empty types" in invalidSchema(
      graphql"""
        type Query {
          test: BadUnion
        }
  
        union BadUnion
      """,
      "Union type 'BadUnion' must define one or more member types." -> Seq(Pos(6, 9))
    )

    "rejects a Union type with duplicated member type" in invalidSchema(
      {
        val ast1 =
          graphql"""
            type Query {
              test: BadUnion
            }

            union BadUnion =
              | TypeA
              | TypeB
          """

        val ast2 =
          graphql"""
            type TypeA {
              field: String
            }

            type TypeB {
              field: String
            }

            extend union BadUnion = TypeA
          """

        ast1 + ast2
      },
      "Union type 'BadUnion' can only include type 'TypeA' once." -> Seq(Pos(7, 17), Pos(10, 37))
    )

    "rejects a Union type with non-Object members types" in invalidSchema(
      graphql"""
        type Query {
          test: BadUnion
        }

        type TypeA {
          field: String
        }

        type TypeB {
          field: String
        }

        union BadUnion =
          | TypeA
          | String
          | TypeB
      """,
      "Type 'String' is not an object type." -> Seq(Pos(16, 13))
    )
  }

  "Type System: Input Objects must have fields" should {
    "accepts an Input Object type with fields" in validSchema(graphql"""
        type Query {
          field(arg: SomeInputObject): String
        }

        input SomeInputObject {
          field: String
        }
      """)

    "rejects an Input Object type with missing fields" in invalidSchema(
      graphql"""
        type Query {
          field(arg: SomeInputObject): String
        }

        input SomeInputObject
      """,
      "Input type 'SomeInputObject' must define one or more fields." -> Seq(Pos(6, 9))
    )

    "rejects an Input Object type with incorrectly typed fields" in invalidSchema(
      graphql"""
        type Query {
          field(arg: SomeInputObject): String
        }

        type SomeObject {
          field: String
        }

        union SomeUnion = SomeObject

        input SomeInputObject {
          badObject: SomeObject
          badUnion: SomeUnion
          goodInputObject: SomeInputObject
        }
      """,
      "Type 'SomeObject' is not an input type type." -> Seq(Pos(13, 22))
    )
  }

  "Type System: Enum types must be well defined" should {
    "rejects an Enum type without values" in invalidSchema(
      graphql"""
        type Query {
          field: SomeEnum
        }

        enum SomeEnum
      """,
      "Enum type 'SomeEnum' must define one or more values." -> Seq(Pos(6, 9))
    )

    "rejects an Enum type with duplicate values" in invalidSchema(
      graphql"""
        type Query {
          field: SomeEnum
        }

        enum SomeEnum {
          SOME_VALUE
          SOME_VALUE
        }
      """,
      "Enum type 'SomeEnum' can include value 'SOME_VALUE' only once." -> Seq(
        Pos(7, 11),
        Pos(8, 11))
    )
  }

  "Type System: Object fields must have output types" should {
    "rejects with relevant locations for a non-output type as an Object field type" in invalidSchema(
      graphql"""
        type Query {
          field: [SomeInputObject]
        }

        input SomeInputObject {
          field: String
        }
      """,
      "Type 'SomeInputObject' is not an output type type." -> Seq(Pos(3, 19))
    )
  }

  "Type System: Objects can only implement unique interfaces" should {
    "rejects an Object implementing a non-Interface type" in invalidSchema(
      graphql"""
        type Query {
          test: BadObject
        }

        input SomeInputObject {
          field: String
        }

        type BadObject implements SomeInputObject {
          field: String
        }
      """,
      "Type 'SomeInputObject' is not an output type type." -> Seq(Pos(10, 35))
    )

    "rejects an Object implementing the same interface twice" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: String
        }

        type AnotherObject implements AnotherInterface & AnotherInterface {
          field: String
        }
      """,
      "Object type 'AnotherObject' can implement interface 'AnotherInterface' only once." -> Seq(
        Pos(10, 39),
        Pos(10, 58))
    )

    "rejects an Object implementing the same interface twice due to extension" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: String
        }

        type AnotherObject implements AnotherInterface {
          field: String
        }

        extend type AnotherObject implements AnotherInterface
      """,
      "Object type 'AnotherObject' can implement interface 'AnotherInterface' only once." -> Seq(
        Pos(10, 39),
        Pos(14, 46))
    )

    "rejects an Object implementing the extended interface due to missing field (via extension)" in invalidSchema(
      buildSchema(graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: String
        }

        type AnotherObject implements AnotherInterface {
          field: String
        }
      """).extend(graphql"""
        extend type AnotherObject implements AnotherInterface
      """),
      "Object type 'AnotherObject' can implement interface 'AnotherInterface' only once." -> Seq(
        Pos(10, 39),
        Pos(2, 46))
    )
  }

  "Type System: Interface extensions should be valid" should {
    "rejects an Object implementing the extended interface due to missing field args" in invalidSchema(
      buildSchema(graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: String
        }

        type AnotherObject implements AnotherInterface {
          field: String
        }
      """).extend(graphql"""
        extend interface AnotherInterface {
          newField(test: Boolean): String
        }

        extend type AnotherObject {
          newField: String
        }
      """),
      "AnotherInterface.newField expects argument 'test', but AnotherObject.newField does not provide it." -> Seq(
        Pos(3, 20),
        Pos(7, 11))
    )

    "rejects Objects implementing the extended interface due to mismatching interface type" in invalidSchema(
      buildSchema(graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: String
        }

        type AnotherObject implements AnotherInterface {
          field: String
        }
      """).extend(graphql"""
        extend interface AnotherInterface {
          newInterfaceField: NewInterface
        }

        interface NewInterface {
          newField: String
        }

        interface MismatchingInterface {
          newField: String
        }

        extend type AnotherObject {
          newInterfaceField: MismatchingInterface
        }
      """),
      "AnotherInterface.newInterfaceField expects type 'NewInterface', but AnotherObject.newInterfaceField provides type 'MismatchingInterface'." -> Seq(
        Pos(15, 11),
        Pos(3, 11))
    )
  }

  "Type System: Interface fields must have output types" should {
    "rejects a non-output type as an Interface field type with locations" in invalidSchema(
      graphql"""
        type Query {
          test: SomeInterface
        }

        interface SomeInterface {
          field: SomeInputObject
        }

        input SomeInputObject {
          foo: String
        }
      """,
      "Type 'SomeInputObject' is not an output type type." -> Seq(Pos(7, 18))
    )
  }

  "Type System: Field arguments must have input types" should {
    "rejects a non-input type as a field arg with locations" in invalidSchema(
      graphql"""
        type Query {
          test(arg: SomeObject): String
        }

        type SomeObject {
          foo: String
        }
      """,
      "Type 'SomeObject' is not an input type type." -> Seq(Pos(3, 21))
    )
  }

  "Type System: Input Object fields must have input types" should {
    "rejects a non-input type as an input object field with locations" in invalidSchema(
      graphql"""
        type Query {
          test(arg: SomeInputObject): String
        }

        input SomeInputObject {
          foo: SomeObject
        }

        type SomeObject {
          bar: String
        }
      """,
      "Type 'SomeObject' is not an input type type." -> Seq(Pos(7, 16))
    )
  }

  "Objects must adhere to Interface they implement" should {
    "accepts an Object which implements an Interface" in validSchema(graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field(input: String): String
        }

        type AnotherObject implements AnotherInterface {
          field(input: String): String
        }
      """)

    "accepts an Object which implements an Interface along with more fields" in validSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field(input: String): String
        }

        type AnotherObject implements AnotherInterface {
          field(input: String): String
          anotherField: String
        }
      """)

    "accepts an Object which implements an Interface field along with additional optional arguments" in validSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field(input: String): String
        }

        type AnotherObject implements AnotherInterface {
          field(input: String, anotherInput: String): String
        }
      """)

    "rejects an Object with an incorrectly typed Interface field" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field(input: String): String
        }

        type AnotherObject implements AnotherInterface {
          field(input: String): Int
        }
      """,
      "AnotherInterface.field expects type 'String', but AnotherObject.field provides type 'Int'." -> Seq(
        Pos(11, 11),
        Pos(7, 11))
    )

    "rejects an Object with a differently typed Interface field" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        type A { foo: String }
        type B { foo: String }

        interface AnotherInterface {
          field: A
        }

        type AnotherObject implements AnotherInterface {
          field: B
        }
      """,
      "AnotherInterface.field expects type 'A', but AnotherObject.field provides type 'B'." -> Seq(
        Pos(14, 11),
        Pos(10, 11))
    )

    "accepts an Object with a subtyped Interface field (interface)" in validSchema(graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: AnotherInterface
        }

        type AnotherObject implements AnotherInterface {
          field: AnotherObject
        }
      """)

    "accepts an Object with a subtyped Interface field (union)" in validSchema(graphql"""
        type Query {
          test: AnotherObject
        }

        type SomeObject {
          field: String
        }

        union SomeUnionType = SomeObject

        interface AnotherInterface {
          field: SomeUnionType
        }

        type AnotherObject implements AnotherInterface {
          field: SomeObject
        }
      """)

    "rejects an Object missing an Interface argument" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field(input: String): String
        }

        type AnotherObject implements AnotherInterface {
          field: String
        }
      """,
      "AnotherInterface.field expects argument 'input', but AnotherObject.field does not provide it." -> Seq(
        Pos(7, 17),
        Pos(11, 11))
    )

    "rejects an Object with an incorrectly typed Interface argument" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field(input: String): String
        }

        type AnotherObject implements AnotherInterface {
          field(input: Int): String
        }
      """,
      "AnotherInterface.field(input) expects type 'String', but AnotherObject.field(input) provides type 'Int'." -> Seq(
        Pos(7, 17),
        Pos(11, 17))
    )

    "rejects an Object with both an incorrectly typed field and argument" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field(input: String): String
        }

        type AnotherObject implements AnotherInterface {
          field(input: Int): Int
        }
      """,
      "AnotherInterface.field expects type 'String', but AnotherObject.field provides type 'Int'." -> Seq(
        Pos(11, 11),
        Pos(7, 11))
    )

    "rejects an Object which implements an Interface field along with additional required arguments" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field(input: String): String
        }

        type AnotherObject implements AnotherInterface {
          field(input: String, anotherInput: String!): String
        }
      """,
      "AnotherObject.field(anotherInput) is of required type 'String!', but is not also provided by the interface AnotherInterface.field." -> Seq(
        Pos(11, 32),
        Pos(7, 11))
    )

    "accepts an Object with an equivalently wrapped Interface field type" in validSchema(graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: [String]!
        }

        type AnotherObject implements AnotherInterface {
          field: [String]!
        }
      """)

    "rejects an Object with a non-list Interface field list type" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: [String]
        }

        type AnotherObject implements AnotherInterface {
          field: String
        }
      """,
      "AnotherInterface.field expects type '[String]', but AnotherObject.field provides type 'String'." -> Seq(
        Pos(11, 11),
        Pos(7, 11))
    )

    "rejects an Object with a list Interface field non-list type" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: String
        }

        type AnotherObject implements AnotherInterface {
          field: [String]
        }
      """,
      "AnotherInterface.field expects type 'String', but AnotherObject.field provides type '[String]'." -> Seq(
        Pos(11, 11),
        Pos(7, 11))
    )

    "accepts an Object with a subset non-null Interface field type" in validSchema(graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: String
        }

        type AnotherObject implements AnotherInterface {
          field: String!
        }
      """)

    "rejects an Object with a superset nullable Interface field type" in invalidSchema(
      graphql"""
        type Query {
          test: AnotherObject
        }

        interface AnotherInterface {
          field: String!
        }

        type AnotherObject implements AnotherInterface {
          field: String
        }
      """,
      "AnotherInterface.field expects type 'String!', but AnotherObject.field provides type 'String'." -> Seq(
        Pos(11, 11),
        Pos(7, 11))
    )
  }

  private[this] def buildSchema(document: ast.Document) =
    Schema.buildFromAst(document)

  private[this] def validSchema(document: ast.Document) = buildSchema(document)

  private[this] def invalidSchema(
      document: ast.Document,
      expected: (String, Seq[Pos])*
  ): Unit = invalidSchema(buildSchema(document), expected: _*)

  private[this] def invalidSchema(schema: => Schema[_, _], expected: (String, Seq[Pos])*): Unit =
    (Try(schema): @unchecked) match {
      case Success(_) => fail("Schema was built successfully")
      case Failure(e: WithViolations) =>
        val violationsStr =
          "Actual violations:\n\n" + e.violations.zipWithIndex
            .map { case (v, idx) =>
              val helperStr = v match {
                case n: AstNodeLocation =>
                  "    \"" + n.simpleErrorMessage + "\" -> Seq(" + n.locations
                    .map(l => s"Pos(${l.line}, ${l.column})")
                    .mkString(", ") + ")"
                case n => n.errorMessage
              }

              s"(${idx + 1}) " + v.errorMessage + "\n\n" + helperStr
            }
            .mkString("\n\n") + "\n\n"

        withClue(violationsStr) {
          e.violations should have size expected.size

          expected.foreach { case (expected, pos) =>
            e.violations.exists { error =>
              val message = error.errorMessage

              message.contains(expected) && {
                error match {
                  case n: AstNodeLocation => n.locations.map(p => Pos(p.line, p.column)) == pos
                  case _ => false
                }
              }
            } should be(true)
          }
        }
    }
}
