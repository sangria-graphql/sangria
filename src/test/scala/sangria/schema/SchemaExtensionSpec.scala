package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.ast.{FieldDefinition, ObjectTypeDefinition}
import sangria.macros._
import sangria.parser.DeliveryScheme.Throw
import sangria.parser.QueryParser
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.util.SimpleGraphQlSupport.check
import sangria.util.{Pos, SimpleGraphQlSupport, FutureResultSupport, StringMatchers}
import sangria.validation.IntCoercionViolation
import spray.json._

class SchemaExtensionSpec extends WordSpec with Matchers with FutureResultSupport with StringMatchers {

  val SomeInterfaceType: InterfaceType[Unit, Unit] = InterfaceType("SomeInterface", () ⇒ fields(
    Field("name", OptionType(StringType), resolve = _ ⇒ None),
    Field("some", OptionType(SomeInterfaceType), resolve = _ ⇒ Some(()))
  ))

  val FooType: ObjectType[Unit, Unit] = ObjectType("Foo", interfaces = interfaces(SomeInterfaceType), () ⇒ fields(
    Field("name", OptionType(StringType), resolve = _ ⇒ None),
    Field("some", OptionType(SomeInterfaceType), resolve = _ ⇒ Some(())),
    Field("tree", ListType(OptionType(FooType)), resolve = _ ⇒ List(Some(()), None, Some(())))
  ))

  val BarType: ObjectType[Unit, Unit] = ObjectType("Bar", interfaces = interfaces(SomeInterfaceType), () ⇒ fields(
    Field("name", OptionType(StringType), resolve = _ ⇒ None),
    Field("some", OptionType(SomeInterfaceType), resolve = _ ⇒ Some(())),
    Field("foo", OptionType(FooType), resolve = _ ⇒ Some(()))
  ))

  val BizType = ObjectType("Biz", () ⇒ fields[Unit, Unit](
    Field("fizz", OptionType(StringType), resolve = _ ⇒ None)
  ))

  val SomeUnionType = UnionType("SomeUnion", types = FooType :: BizType :: Nil)

  val SomeEnumType = EnumType("SomeEnum", values = List(
    EnumValue("ONE", value = 1),
    EnumValue("TWO", value = 2)
  ))

  val schema = Schema(
    query = ObjectType("Query", fields[Unit, Unit](
      Field("foo", OptionType(FooType), resolve = _ ⇒ Some(())),
      Field("someUnion", OptionType(SomeUnionType), resolve = _ ⇒ None),
      Field("someEnum", OptionType(SomeEnumType), resolve = _ ⇒ None),
      Field("someInterface", OptionType(SomeInterfaceType),
        arguments = Argument("id", IDType) :: Nil,
        resolve = _ ⇒ None)
    )),
    additionalTypes = BarType :: Nil)

  "Type System: extendSchema" should {
    "returns the original schema when there are no type definitions" in {
      val ast = graphql"{field}"
      schema.extend(ast) should be theSameInstanceAs schema
    }

    "extends without altering original schema" in {
      val ast =
        graphql"""
          extend type Query {
            newField: String
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should include ("newField")
      SchemaRenderer.renderSchema(schema) should not include "newField"
    }

    "cannot be used for execution by default" in {
      val ast =
        graphql"""
          extend type Query {
            newField: String
          }
        """

      val query = graphql"{ newField }"

      SimpleGraphQlSupport.checkContainsErrors(
        schema = schema.extend(ast),
        data = (),
        query = "{ newField }",
        expectedData = Map("newField" → null),
        expectedErrorStrings = List(DefaultIntrospectionSchemaBuilder.MaterializedSchemaErrorMessage → List(Pos(1, 3))))
    }

    "extends objects by adding new fields" in {
      val ast =
        graphql"""
          extend type Foo {
            newField: String
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Biz {
          |  fizz: String
          |}
          |
          |type Foo implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |  newField: String
          |}
          |
          |type Query {
          |  foo: Foo
          |  someUnion: SomeUnion
          |  someEnum: SomeEnum
          |  someInterface(id: ID!): SomeInterface
          |}
          |
          |enum SomeEnum {
          |  ONE
          |  TWO
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |union SomeUnion = Foo | Biz""".stripMargin) (after being strippedOfCarriageReturns)


    }

    "extends objects by adding new unused types" in {
      val ast =
        graphql"""
          type Unused {
            someField: String
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Biz {
          |  fizz: String
          |}
          |
          |type Foo implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |}
          |
          |type Query {
          |  foo: Foo
          |  someUnion: SomeUnion
          |  someEnum: SomeEnum
          |  someInterface(id: ID!): SomeInterface
          |}
          |
          |enum SomeEnum {
          |  ONE
          |  TWO
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |union SomeUnion = Foo | Biz
          |
          |type Unused {
          |  someField: String
          |}""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "extends objects by adding new fields with arguments" in {
      val ast =
        graphql"""
          extend type Foo {
            newField(arg1: String, arg2: NewInputObj!): String
          }

          input NewInputObj {
            field1: Int
            field2: [Float]
            field3: String!
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Biz {
          |  fizz: String
          |}
          |
          |type Foo implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |  newField(arg1: String, arg2: NewInputObj!): String
          |}
          |
          |input NewInputObj {
          |  field1: Int
          |  field2: [Float]
          |  field3: String!
          |}
          |
          |type Query {
          |  foo: Foo
          |  someUnion: SomeUnion
          |  someEnum: SomeEnum
          |  someInterface(id: ID!): SomeInterface
          |}
          |
          |enum SomeEnum {
          |  ONE
          |  TWO
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |union SomeUnion = Foo | Biz""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "extends objects by adding new fields with existing types" in {
      val ast =
        graphql"""
          extend type Foo {
            newField(arg1: SomeEnum!): SomeEnum
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Biz {
          |  fizz: String
          |}
          |
          |type Foo implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |  newField(arg1: SomeEnum!): SomeEnum
          |}
          |
          |type Query {
          |  foo: Foo
          |  someUnion: SomeUnion
          |  someEnum: SomeEnum
          |  someInterface(id: ID!): SomeInterface
          |}
          |
          |enum SomeEnum {
          |  ONE
          |  TWO
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |union SomeUnion = Foo | Biz""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "extends objects by adding implemented interfaces" in {
      val ast =
        graphql"""
          extend type Biz implements SomeInterface {
            name: String
            some: SomeInterface
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Biz implements SomeInterface {
          |  fizz: String
          |  name: String
          |  some: SomeInterface
          |}
          |
          |type Foo implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |}
          |
          |type Query {
          |  foo: Foo
          |  someUnion: SomeUnion
          |  someEnum: SomeEnum
          |  someInterface(id: ID!): SomeInterface
          |}
          |
          |enum SomeEnum {
          |  ONE
          |  TWO
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |union SomeUnion = Foo | Biz""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "extends objects by including new types" in {
      val ast =
        graphql"""
          extend type Foo {
            newObject: NewObject
            newInterface: NewInterface
            newUnion: NewUnion
            newScalar: NewScalar
            newEnum: NewEnum
            newTree: [Foo]!
          }

          type NewObject implements NewInterface {
            baz: String
          }

          type NewOtherObject {
            fizz: Int
          }

          interface NewInterface {
            baz: String
          }

          union NewUnion = NewObject | NewOtherObject

          scalar NewScalar

          enum NewEnum {
            OPTION_A
            OPTION_B
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Biz {
          |  fizz: String
          |}
          |
          |type Foo implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |  newObject: NewObject
          |  newInterface: NewInterface
          |  newUnion: NewUnion
          |  newScalar: NewScalar
          |  newEnum: NewEnum
          |  newTree: [Foo]!
          |}
          |
          |enum NewEnum {
          |  OPTION_A
          |  OPTION_B
          |}
          |
          |interface NewInterface {
          |  baz: String
          |}
          |
          |type NewObject implements NewInterface {
          |  baz: String
          |}
          |
          |type NewOtherObject {
          |  fizz: Int
          |}
          |
          |scalar NewScalar
          |
          |union NewUnion = NewObject | NewOtherObject
          |
          |type Query {
          |  foo: Foo
          |  someUnion: SomeUnion
          |  someEnum: SomeEnum
          |  someInterface(id: ID!): SomeInterface
          |}
          |
          |enum SomeEnum {
          |  ONE
          |  TWO
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |union SomeUnion = Foo | Biz""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "extends objects by adding implemented new interfaces" in {
      val ast =
        graphql"""
          extend type Foo implements NewInterface {
            baz: String
          }

          interface NewInterface {
            baz: String
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Biz {
          |  fizz: String
          |}
          |
          |type Foo implements NewInterface, SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |  baz: String
          |}
          |
          |interface NewInterface {
          |  baz: String
          |}
          |
          |type Query {
          |  foo: Foo
          |  someUnion: SomeUnion
          |  someEnum: SomeEnum
          |  someInterface(id: ID!): SomeInterface
          |}
          |
          |enum SomeEnum {
          |  ONE
          |  TWO
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |union SomeUnion = Foo | Biz""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "extends objects multiple times" in {
      val ast =
        graphql"""
          extend type Biz implements NewInterface {
            buzz: String
          }

          extend type Biz implements SomeInterface {
            name: String
            some: SomeInterface
            newFieldA: Int
          }

          extend type Biz {
            newFieldB: Float
          }

          interface NewInterface {
            buzz: String
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = schema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Biz implements NewInterface, SomeInterface {
          |  fizz: String
          |  buzz: String
          |  name: String
          |  some: SomeInterface
          |  newFieldA: Int
          |  newFieldB: Float
          |}
          |
          |type Foo implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |}
          |
          |interface NewInterface {
          |  buzz: String
          |}
          |
          |type Query {
          |  foo: Foo
          |  someUnion: SomeUnion
          |  someEnum: SomeEnum
          |  someInterface(id: ID!): SomeInterface
          |}
          |
          |enum SomeEnum {
          |  ONE
          |  TWO
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |union SomeUnion = Foo | Biz""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "may extend mutations and subscriptions" in {
      val mutationSchema = Schema(
        query = ObjectType("Query", fields[Unit, Unit](
          Field("queryField", StringType, resolve = _ ⇒ ""))),
        mutation = Some(ObjectType("Mutation", fields[Unit, Unit](
          Field("mutationField", StringType, resolve = _ ⇒ "")))),
        subscription = Some(ObjectType("Subscription", fields[Unit, Unit](
          Field("subscriptionField", StringType, resolve = _ ⇒ "")))))

      val ast =
        graphql"""
          extend type Query {
            newQueryField: Int
          }

          extend type Mutation {
            newMutationField: Int
          }

          extend type Subscription {
            newSubscriptionField: Int
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schema)
      val extendedSchema = mutationSchema.extend(ast)

      extendedSchema should not be theSameInstanceAs (schema)
      SchemaRenderer.renderSchema(schema) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """schema {
          |  query: Query
          |  mutation: Mutation
          |  subscription: Subscription
          |}
          |
          |type Mutation {
          |  mutationField: String!
          |  newMutationField: Int
          |}
          |
          |type Query {
          |  queryField: String!
          |  newQueryField: Int
          |}
          |
          |type Subscription {
          |  subscriptionField: String!
          |  newSubscriptionField: Int
          |}""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "does not allow replacing an existing type" in {
      val ast =
        graphql"""
          type Bar {
            baz: String
          }
        """

      val error = intercept[SchemaMaterializationException](schema.extend(ast))

      error.message should include ("Type 'Bar' already exists in the schema.")
    }

    "does not allow replacing an existing field" in {
      val ast =
        graphql"""
          extend type Bar {
            foo: Foo
          }
        """

      val error = intercept[SchemaMaterializationException](schema.extend(ast))

      error.message should include ("Field 'Bar.foo' already exists in the schema.")
    }

    "does not allow implementing an existing interface" in {
      val ast =
        graphql"""
          extend type Foo implements SomeInterface {
            otherField: String
          }
        """

      val error = intercept[SchemaMaterializationException](schema.extend(ast))

      error.message should include ("Type 'Foo' already implements 'SomeInterface'.")
    }

    "does not allow referencing an unknown type" in {
      val ast =
        graphql"""
          extend type Bar {
            quix: Quix
          }
        """

      val error = intercept[SchemaMaterializationException](schema.extend(ast))

      error.message should include ("Invalid or incomplete schema, unknown type: Quix.")
    }

    "does not allow extending an unknown type" in {
      val ast =
        graphql"""
          extend type UnknownType {
            baz: String
          }
        """

      val error = intercept[SchemaMaterializationException](schema.extend(ast))

      error.message should include ("Cannot extend type 'UnknownType' because it does not exist.")
    }

    "does not allow extending a non-object type: not an interface" in {
      val ast =
        graphql"""
          extend type SomeInterface {
            baz: String
          }
        """

      val error = intercept[SchemaMaterializationException](schema.extend(ast))

      error.message should include ("Cannot extend non-object type 'SomeInterface'.")
    }

    "does not allow extending a non-object type: not a scalar" in {
      val ast =
        graphql"""
          extend type String {
            baz: String
          }
        """

      val error = intercept[SchemaMaterializationException](schema.extend(ast))

      error.message should include ("Cannot extend non-object type 'String'.")
    }
  }
}
