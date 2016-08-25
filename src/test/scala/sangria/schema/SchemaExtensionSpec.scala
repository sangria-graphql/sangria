package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.ast.{TypeExtensionDefinition, FieldDefinition, ObjectTypeDefinition}
import sangria.renderer.{SchemaRenderer}
import sangria.util.SimpleGraphQlSupport.check
import sangria.util.{Pos, SimpleGraphQlSupport, FutureResultSupport, StringMatchers}
import sangria.validation.IntCoercionViolation
import sangria.macros._

class SchemaExtensionSpec extends WordSpec with Matchers with FutureResultSupport with StringMatchers {

  trait SomeInterface {
    def name: Option[String]
    def some: Option[SomeInterface]
  }

  case class Foo(name: Option[String], some: Option[SomeInterface], tree: List[Option[Foo]]) extends SomeInterface
  case class Bar(name: Option[String], some: Option[SomeInterface], foo: Option[Foo]) extends SomeInterface

  val SomeInterfaceType: InterfaceType[Unit, SomeInterface] = InterfaceType("SomeInterface", () ⇒ fields(
    Field("name", OptionType(StringType), resolve = _.value.name),
    Field("some", OptionType(SomeInterfaceType), resolve = _.value.some)
  ))

  val FooType: ObjectType[Unit, Foo] = ObjectType("Foo", interfaces = interfaces(SomeInterfaceType), () ⇒ fields(
    Field("name", OptionType(StringType), resolve = _.value.name),
    Field("some", OptionType(SomeInterfaceType), resolve = _.value.some),
    Field("tree", ListType(OptionType(FooType)), resolve = _.value.tree)
  ))

  val BarType: ObjectType[Unit, Bar] = ObjectType("Bar", interfaces = interfaces(SomeInterfaceType), () ⇒ fields(
    Field("name", OptionType(StringType), resolve = _.value.name),
    Field("some", OptionType(SomeInterfaceType), resolve = _.value.some),
    Field("foo", OptionType(FooType), resolve = _.value.foo)
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
      Field("foo", OptionType(FooType), resolve = _ ⇒ Some(Foo(Some("foo"), None, Nil))),
      Field("someUnion", OptionType(SomeUnionType), resolve = _ ⇒ None),
      Field("someEnum", OptionType(SomeEnumType), resolve = _ ⇒ None),
      Field("someInterface", OptionType(SomeInterfaceType),
        arguments = Argument("id", IDType) :: Nil,
        resolve = _ ⇒ Some(Foo(Some("a"), Some(Bar(Some("b"), None, Some(Foo(Some("c"), None, Nil)))), List(None, Some(Foo(Some("d"), None, Nil))))))
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
        """type Bar implements SomeInterface {
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
        """type Bar implements SomeInterface {
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
        """type Bar implements SomeInterface {
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

    "extends objects by adding new fields with not yet used types" in {
      val ProductType = InterfaceType("Product", fields[Unit, Unit](
        Field("name", StringType, resolve = _ ⇒ "some name")
      ))

      val MagicPotionType = ObjectType("MagicPotion", interfaces[Unit, Unit](ProductType), fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1)
      ))

      val schemaWithPotion = Schema(
        query = ObjectType("Query", fields[Unit, Unit](
          Field("foo", OptionType(FooType), resolve = _ ⇒ Some(Foo(Some("foo"), None, Nil))))),
        additionalTypes = BarType :: MagicPotionType :: ProductType :: Nil)

      val ast =
        graphql"""
          extend type Foo {
            something: Anything
          }

          interface Anything {
            id: String
          }

          type Something implements Anything {
            f1: MagicPotion
            f2: [Product!]
          }
        """

      val originalRender = SchemaRenderer.renderSchema(schemaWithPotion)
      val extendedSchema = schemaWithPotion.extend(ast)

      extendedSchema should not be theSameInstanceAs (schemaWithPotion)
      SchemaRenderer.renderSchema(schemaWithPotion) should be (originalRender)
      SchemaRenderer.renderSchema(extendedSchema) should equal (
        """interface Anything {
          |  id: String
          |}
          |
          |type Bar implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  foo: Foo
          |}
          |
          |type Foo implements SomeInterface {
          |  name: String
          |  some: SomeInterface
          |  tree: [Foo]!
          |  something: Anything
          |}
          |
          |type MagicPotion implements Product {
          |  size: Int!
          |  name: String!
          |}
          |
          |interface Product {
          |  name: String!
          |}
          |
          |type Query {
          |  foo: Foo
          |}
          |
          |interface SomeInterface {
          |  name: String
          |  some: SomeInterface
          |}
          |
          |type Something implements Anything {
          |  f1: MagicPotion
          |  f2: [Product!]
          |  id: String
          |}""".stripMargin) (after being strippedOfCarriageReturns)
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
        """type Bar implements SomeInterface {
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
        """type Bar implements SomeInterface {
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
        """type Bar implements SomeInterface {
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
        """type Bar implements SomeInterface {
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
        """type Bar implements SomeInterface {
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
        """type Mutation {
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

    "be able to resolve existing fields and use builder logic for new fields" in {
      val schemaAst =
        graphql"""
          extend type Foo {
            animal1: Animal!
            animal2: Animal!
          }

          type Hello implements SomeInterface {
            name: String
            some: SomeInterface
            custom: Custom!
          }

          scalar Custom

          interface Animal {
            name: String!
          }

          type Dog implements Animal {
            name: String!
            nickname: String
          }

          type Cat implements Animal {
            name: String!
            age: Int
          }

          extend type Query {
            special: SomeInterface
          }
        """

      val customBuilder = new DefaultAstSchemaBuilder[Unit] {
        override def resolveField(typeDefinition: ast.TypeDefinition, definition: FieldDefinition) =
          if (definition.name == "animal1")
            _ ⇒ Map("type" → "Cat", "name" → "foo", "age" → Some(10))
          else if (definition.name == "animal2")
            _ ⇒ Map("type" → "Dog", "name" → "bar", "nickname" → Some("baz"))
          else if (definition.name == "special")
            _ ⇒ Map("name" → "Fooo", "some" → None, "custom" → 123)
          else
            _.value.asInstanceOf[Map[String, Any]](definition.name)

        override def objectTypeInstanceCheck(definition: ObjectTypeDefinition, extensions: List[ast.TypeExtensionDefinition]) =
          Some((value, clazz) ⇒ value match {
            case v: Map[_, _] if definition.name == "Hello" ⇒ true
            case v : Map[String, _] @unchecked if v contains "type" ⇒ value.asInstanceOf[Map[String, Any]]("type") == definition.name
            case _ ⇒ false
          })

        override def extendedObjectTypeInstanceCheck(tpe: ObjectType[Unit, _], extensions: List[TypeExtensionDefinition]) =
          Some((value, clazz) ⇒ value match {
            case v: Map[_, _] if tpe.name == "Hello" ⇒ true
            case v if clazz.isAssignableFrom(v.getClass) ⇒ true
            case _ ⇒ false
          })

        override def scalarCoerceUserInput(definition: ast.ScalarTypeDefinition) =
          value ⇒ definition.name match {
            case "Custom" ⇒ value match {
              case i: Int ⇒ Right(i)
              case i: BigInt ⇒ Right(i.intValue)
              case _ ⇒ Left(IntCoercionViolation)
            }
            case _ ⇒ Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)
          }

        override def scalarCoerceInput(definition: ast.ScalarTypeDefinition) =
          value ⇒ definition.name match {
            case "Custom" ⇒ value match {
              case ast.IntValue(i, _, _) ⇒ Right(i)
              case ast.BigIntValue(i, _, _) ⇒ Right(i.intValue)
              case _ ⇒ Left(IntCoercionViolation)
            }
            case _ ⇒ Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)
          }

        override def scalarCoerceOutput(definition: ast.ScalarTypeDefinition) =
          (coerced, _) ⇒ definition.name match {
            case "Custom" ⇒ ast.IntValue(coerced.asInstanceOf[Int])
            case _ ⇒ throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException
          }
      }

      val extendedSchema = schema.extend(schemaAst, customBuilder)

      check(extendedSchema, (),
        """
          query {
            foo { name }
            someInterface(id: "123") {
              ...MyInt
            }

            special {
              __typename
              ...MyInt
            }
          }

          fragment MyInt on SomeInterface {
            name
            some {name}

            ... on Foo {
              tree {name}
              animal1 {__typename name}
              animal2 {__typename name}
            }

            ... on Hello {
              custom
            }
          }
        """,
        Map("data" →
          Map(
            "foo" → Map("name" → "foo"),
            "someInterface" → Map(
              "name" → "a",
              "some" → Map("name" → "b"),
              "tree" → Vector(null, Map("name" → "d")),
              "animal1" → Map("__typename" → "Cat", "name" → "foo"),
              "animal2" → Map("__typename" → "Dog", "name" → "bar")),
            "special" → Map(
              "__typename" → "Hello",
              "name" → "Fooo",
              "some" → null,
              "custom" → 123))))
    }
  }
}
