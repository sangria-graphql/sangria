package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.ast.{FieldDefinition, ObjectTypeDefinition, TypeDefinition}
import sangria.parser.QueryParser
import sangria.renderer.SchemaRenderer
import sangria.util.{FutureResultSupport, Pos, StringMatchers}
import sangria.parser.DeliveryScheme.Throw
import sangria.macros._
import sangria.validation.IntCoercionViolation
import sangria.util.SimpleGraphQlSupport.{check, checkContainsErrors}
import spray.json._

class AstSchemaMaterializerSpec extends WordSpec with Matchers with FutureResultSupport with StringMatchers {

  // This function does a full cycle of going from a
  // string with the contents of the DSL, parsed
  // in a schema AST, materializing that schema AST
  // into an in-memory GraphQL Schema, and then finally
  // printing that GraphQL into the DSL
  def cycleOutput(schemaDefinition: String): String = {
    val ast = QueryParser.parse(schemaDefinition)
    val schema = Schema.buildFromAst(ast)

    SchemaRenderer.renderSchema(schema)
  }

  "Type System: build schema from AST" when {
    "Schema Builder" should {
      "Simple type" in {
        val schema =
          """schema {
            |  query: HelloScalars
            |}
            |
            |type HelloScalars {
            |  str: String
            |  int: Int
            |  float: Float
            |  id: ID
            |  bool: Boolean
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "With directives" in {
        val schema =
          """schema {
            |  query: Hello
            |}
            |
            |type Hello {
            |  str: String
            |}
            |
            |directive @foo(arg: Int) on FIELD""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Maintains built-in directives" in {
        val ast =
          graphql"""
            schema {
              query: Hello
            }

            type Hello {
              str: String
            }
          """

        val schema = Schema.buildFromAst(ast)

        schema.directives should have size 3

        schema.directivesByName("skip") should be theSameInstanceAs SkipDirective
        schema.directivesByName("include") should be theSameInstanceAs IncludeDirective
        schema.directivesByName("deprecated") should be theSameInstanceAs DeprecatedDirective
      }

      "Overriding directives excludes specified" in {
        val ast =
          graphql"""
            schema {
              query: Hello
            }

            directive @skip on FIELD
            directive @include on FIELD
            directive @deprecated on FIELD_DEFINITION

            type Hello {
              str: String
            }
          """

        val schema = Schema.buildFromAst(ast)

        schema.directives should have size 3

        // We don't allow to override the built-in directives, since it's too dangerous
        schema.directivesByName("skip") should be theSameInstanceAs SkipDirective
        schema.directivesByName("include") should be theSameInstanceAs IncludeDirective
        schema.directivesByName("deprecated") should be theSameInstanceAs DeprecatedDirective
      }

      "Adding directives maintains built-in one" in {
        val ast =
          graphql"""
            schema {
              query: Hello
            }

            directive @foo(arg: Int) on FIELD

            type Hello {
              str: String
            }
          """

        val schema = Schema.buildFromAst(ast)

        schema.directives should have size 4

        schema.directivesByName("skip") should be theSameInstanceAs SkipDirective
        schema.directivesByName("include") should be theSameInstanceAs IncludeDirective
        schema.directivesByName("deprecated") should be theSameInstanceAs DeprecatedDirective
      }

      "Type modifiers" in {
        val schema =
          """schema {
            |  query: HelloScalars
            |}
            |
            |type HelloScalars {
            |  nonNullStr: String!
            |  listOfStrs: [String]
            |  listOfNonNullStrs: [String!]
            |  nonNullListOfStrs: [String]!
            |  nonNullListOfNonNullStrs: [String!]!
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Recursive type" in {
        val schema =
          """schema {
            |  query: Recurse
            |}
            |
            |type Recurse {
            |  str: String
            |  recurse: Recurse
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Two types circular" in {
        val schema =
          """schema {
            |  query: TypeOne
            |}
            |
            |type TypeOne {
            |  str: String
            |  typeTwo: TypeTwo
            |}
            |
            |type TypeTwo {
            |  str: String
            |  typeOne: TypeOne
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Single argument field" in {
        val schema =
          """schema {
            |  query: Hello
            |}
            |
            |type Hello {
            |  str(int: Int): String
            |  floatToStr(float: Float): String
            |  idToStr(id: ID): String
            |  booleanToStr(bool: Boolean): String
            |  strToStr(bool: String): String
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Simple type with multiple arguments" in {
        val schema =
          """schema {
            |  query: Hello
            |}
            |
            |type Hello {
            |  str(int: Int, bool: Boolean): String
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Simple type with interface" in {
        val schema =
          """schema {
            |  query: Hello
            |}
            |
            |type Hello implements WorldInterface {
            |  str: String
            |}
            |
            |interface WorldInterface {
            |  str: String
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Simple output enum" in {
        val schema =
          """schema {
            |  query: OutputEnumRoot
            |}
            |
            |enum Hello {
            |  WORLD
            |}
            |
            |type OutputEnumRoot {
            |  hello: Hello
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Simple input enum" in {
        val schema =
          """schema {
            |  query: InputEnumRoot
            |}
            |
            |enum Hello {
            |  WORLD
            |}
            |
            |type InputEnumRoot {
            |  str(hello: Hello): String
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Multiple value enum" in {
        val schema =
          """schema {
            |  query: OutputEnumRoot
            |}
            |
            |enum Hello {
            |  WO
            |  RLD
            |}
            |
            |type OutputEnumRoot {
            |  hello: Hello
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Simple Union" in {
        val schema =
          """schema {
            |  query: Root
            |}
            |
            |union Hello = World
            |
            |type Root {
            |  hello: Hello
            |}
            |
            |type World {
            |  str: String
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Multiple Union" in {
        val schema =
          """schema {
            |  query: Root
            |}
            |
            |union Hello = WorldOne | WorldTwo
            |
            |type Root {
            |  hello: Hello
            |}
            |
            |type WorldOne {
            |  str: String
            |}
            |
            |type WorldTwo {
            |  str: String
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Custom Scalar" in {
        val schema =
          """schema {
            |  query: Root
            |}
            |
            |scalar CustomScalar
            |
            |type Root {
            |  customScalar: CustomScalar
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Input Object" in {
        val schema =
          """schema {
            |  query: Root
            |}
            |
            |input Input {
            |  int: Int
            |}
            |
            |type Root {
            |  field(in: Input): String
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Simple argument field with default" in {
        val schema =
          """schema {
            |  query: Hello
            |}
            |
            |type Hello {
            |  str(int: Int = 2): String
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Simple type with mutation" in {
        val schema =
          """schema {
            |  query: HelloScalars
            |  mutation: Mutation
            |}
            |
            |type HelloScalars {
            |  str: String
            |  int: Int
            |  bool: Boolean
            |}
            |
            |type Mutation {
            |  addHelloScalars(str: String, int: Int, bool: Boolean): HelloScalars
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Unreferenced type implementing referenced interface" in {
        val schema =
          """type Concrete implements Iface {
            |  key: String
            |}
            |
            |interface Iface {
            |  key: String
            |}
            |
            |type Query {
            |  iface: Iface
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Unreferenced type implementing referenced union" in {
        val schema =
          """type Concrete {
            |  key: String
            |}
            |
            |type Query {
            |  union: Union
            |}
            |
            |union Union = Concrete""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }

      "Supports @deprecated" in {
        val schema =
          """enum MyEnum {
            |  VALUE
            |  OLD_VALUE @deprecated
            |  OTHER_VALUE @deprecated(reason: "Terrible reasons")
            |}
            |
            |type Query {
            |  field1: String @deprecated
            |  field2: Int @deprecated(reason: "Because I said so")
            |  enum: MyEnum
            |}""".stripMargin

        cycleOutput(schema) should equal (schema) (after being strippedOfCarriageReturns)
      }
    }

    "Failures" should {
      "Requires a schema definition" in {
        val ast =
          graphql"""
            type Hello {
              bar: Bar
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Must provide schema definition with query type or a type named Query.")
      }

      "Allows only a single schema definition" in {
        val ast =
          graphql"""
            schema {
              query: Hello
            }

            schema {
              query: Hello
            }

            type Hello {
              bar: Bar
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Must provide only one schema definition.")
      }

      "Requires a query type" in {
        val ast =
          graphql"""
            schema {
              mutation: Hello
            }

            type Hello {
              bar: Bar
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Must provide only one query type in schema.")
      }

      "Allows only a single query type" in {
        val ast =
          graphql"""
            schema {
              query: Hello
              query: Yellow
            }

            type Hello {
              bar: Bar
            }

            type Yellow {
              isColor: Boolean
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Must provide only one query type in schema.")
      }

      "Allows only a single mutation type" in {
        val ast =
          graphql"""
            schema {
              query: Hello
              mutation: Hello
              mutation: Yellow
            }

            type Hello {
              bar: Bar
            }

            type Yellow {
              isColor: Boolean
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Must provide only one mutation type in schema.")
      }

      "Allows only a single subscription type" in {
        val ast =
          graphql"""
            schema {
              query: Hello
              subscription: Hello
              subscription: Yellow
            }

            type Hello {
              bar: Bar
            }

            type Yellow {
              isColor: Boolean
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Must provide only one subscription type in schema.")
      }

      "Unknown type in interface list" in {
        val ast =
          graphql"""
            schema {
              query: Hello
            }

            type Hello implements Bar { }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Invalid or incomplete schema, unknown type: Bar.")
      }

      "Unknown type in union list" in {
        val ast =
          graphql"""
            schema {
              query: Hello
            }

            union TestUnion = Bar
            type Hello { testUnion: TestUnion }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Invalid or incomplete schema, unknown type: Bar.")
      }

      "Unknown query type" in {
        val ast =
          graphql"""
            schema {
              query: Wat
            }

            type Hello {
              str: String
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Invalid or incomplete schema, unknown type: Wat.")
      }

      "Unknown mutation type" in {
        val ast =
          graphql"""
            schema {
              query: Hello
              mutation: Wat
            }

            type Hello {
              str: String
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Invalid or incomplete schema, unknown type: Wat.")
      }

      "Unknown subscription type" in {
        val ast =
          graphql"""
            schema {
              query: Hello
              mutation: Wat
              subscription: Awesome
            }

            type Hello {
              str: String
            }

            type Wat {
              str: String
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Invalid or incomplete schema, unknown type: Awesome.")
      }

      "Does not consider operation names" in {
        val ast =
          graphql"""
            schema {
              query: Foo
            }

            query Foo { field }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Invalid or incomplete schema, unknown type: Foo.")
      }

      "Does not consider fragment names" in {
        val ast =
          graphql"""
            schema {
              query: Foo
            }

            fragment Foo on Type { field }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Invalid or incomplete schema, unknown type: Foo.")
      }
    }

    "Schema Builder contains extension types" should {
      "allow extending fields and interfaces" in {
        val schemaDef =
          """
            schema {
              query: Query
            }

            interface Foo {
              field1: String
            }

            interface Bar {
              field3: String
            }

            interface Baz {
              add: Int
            }

            type Query implements Foo {
              field1: String
              field2: Int
            }

            extend type Query implements Bar {
              field3: String
              field4: String
            }
          """

        val schema = Schema.buildFromAst(QueryParser.parse(schemaDef))

        val query = schema.outputTypes("Query").asInstanceOf[ObjectType[_, _]]

        query.ownFields.map(_.name) should be (List("field1", "field2", "field3", "field4"))
        query.interfaces.map(_.name) should be (List("Foo", "Bar"))
      }

      "don't allow to extend the same interface twice" in {
        val ast =
          graphql"""
            schema {
              query: Query
            }

            interface Foo {
              field1: String
            }

            type Query implements Foo {
              field1: String
              field2: Int
            }

            extend type Query implements Foo {
              field3: String
              field4: String
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Type 'Query' already implements 'Foo'. It cannot also be implemented in this type extension.")
      }

      "don't allow to have duplicate fields in the extension" in {
        val ast =
          graphql"""
            schema {
              query: Query
            }

            type Query {
              field1: String
              field2: Int
            }

            extend type Query {
              field1: String
              field4: String
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Field 'Query.field1' already exists in the schema. It cannot also be defined in this type extension.")
      }

      "don't allow to have extensions on non-existing types" in {
        val ast =
          graphql"""
            schema {
              query: Query
            }

            type Query {
              field1: String
              field2: Int
            }

            extend type Foo {
              field1: String
              field4: String
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Cannot extend type 'Foo' because it does not exist.")
      }

      "don't allow to have extensions on non-object types" in {
        val ast =
          graphql"""
            schema {
              query: Query
            }

            interface Foo {
              foo: Int
            }

            type Query {
              field1: String
              field2: Int
            }

            extend type Foo {
              field1: String
              field4: String
            }
          """

        val error = intercept [SchemaMaterializationException] (Schema.buildFromAst(ast))

        error.getMessage should be ("Cannot extend non-object type 'Foo'.")
      }
    }

    "Schema Builder contains descriptions" should {
      "Use comments for descriptions" in {
        val schemaDef =
          """# fooo bar
            |# baz
            |enum MyEnum {
            |  # value1
            |  VALUE
            |
            |  # value 2
            |  # line 2
            |  OLD_VALUE @deprecated
            |
            |  # value 3
            |  OTHER_VALUE @deprecated(reason: "Terrible reasons")
            |}
            |
            |# My super query!
            |type Query {
            |  field1: String @deprecated
            |
            |  # the second field!
            |  field2: Int @deprecated(reason: "Because I said so")
            |  enum: MyEnum
            |}""".stripMargin

        cycleOutput(schemaDef) should equal (schemaDef) (after being strippedOfCarriageReturns)

        val schema = Schema.buildFromAst(QueryParser.parse(schemaDef))

        val myEnum = schema.inputTypes("MyEnum").asInstanceOf[EnumType[_]]

        myEnum.description should be (Some("fooo bar\nbaz"))
        myEnum.values(0).description should be (Some("value1"))
        myEnum.values(1).description should be (Some("value 2\nline 2"))
        myEnum.values(2).description should be (Some("value 3"))

        val query = schema.outputTypes("Query").asInstanceOf[ObjectType[_, _]]

        query.description should be (Some("My super query!"))
        query.fields(1).description should be (Some("the second field!"))
      }

      "Use comments for descriptions on arguments" in {
        val schemaDef =
          """schema {
            |  query: Query
            |}
            |
            |# My super query!
            |type Query {
            |
            |  # not a description!
            |
            |  field1(
            |    # first arg
            |    arg1: Int = 101,
            |
            |    # This should not
            |    # be part of the description
            |
            |    # secnd arg
            |    # line 2
            |    arg1: String!
            |  ): String
            |}""".stripMargin

        val schema = Schema.buildFromAst(QueryParser.parse(schemaDef))

        val query = schema.outputTypes("Query").asInstanceOf[ObjectType[_, _]]

        query.description should be (Some("My super query!"))

        val field = query.fields(0)

        field.description should be (None)
        field.arguments(0).description should be (Some("first arg"))
        field.arguments(1).description should be (Some("secnd arg\nline 2"))
      }
    }

    "Execution" should {
      "can use client schema for general execution with custom materializer logic" in {
        val schemaAst =
          graphql"""
            schema {
              query: Root
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

            type Root {
              add(a: Custom!, b: Custom = 10): Custom!
              animal1: Animal @returnDog
              animal2: Animal @returnCat
            }
          """

        val ReturnCat = Directive("returnCat", locations = Set(DirectiveLocation.FieldDefinition), shouldInclude = _ ⇒ true)
        val ReturnDog = Directive("returnDog", locations = Set(DirectiveLocation.FieldDefinition), shouldInclude = _ ⇒ true)

        val customBuilder = new DefaultAstSchemaBuilder[Unit] {
          override def resolveField(typeDefinition: ast.TypeDefinition, definition: FieldDefinition) =
            if (definition.directives.exists(_.name == ReturnCat.name))
              _ ⇒ Map("type" → "Cat", "name" → "foo", "age" → Some(10))
            else if (definition.directives.exists(_.name == ReturnDog.name))
              _ ⇒ Map("type" → "Dog", "name" → "bar", "nickname" → Some("baz"))
            else if (definition.name == "add")
              ctx ⇒ ctx.arg[Int]("a") + ctx.arg[Int]("b")
            else
              _.value.asInstanceOf[Map[String, Any]](definition.name)

          override def objectTypeInstanceCheck(definition: ObjectTypeDefinition, extensions: List[ast.TypeExtensionDefinition]) =
            Some((value, _) ⇒ value.asInstanceOf[Map[String, Any]]("type") == definition.name)

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

        val schema = Schema.buildFromAst(schemaAst, customBuilder)

        check(schema, (),
          """
            query Yay($v: Custom!) {
              add1: add(a: 123)
              add2: add(a: 123, b: 30)
              add3: add(a: $v, b: $v)

              a1: animal1 {name, __typename}
              a2: animal2 {name, __typename}

              a3: animal1 {
                __typename
                name

                ... on Cat {
                  age
                }

                ... on Dog {
                  nickname
                }
              }

              a4: animal2 {
                __typename
                name

                ... on Cat {
                  age
                }

                ... on Dog {
                  nickname
                }
              }
            }
          """,
          Map("data" →
            Map(
              "add1" → 133,
              "add2" → 153,
              "add3" → 912,
              "a1" → Map(
                "name" → "bar",
                "__typename" → "Dog"),
              "a2" → Map(
                "name" → "foo",
                "__typename" → "Cat"),
              "a3" → Map(
                "__typename" → "Dog",
                "name" → "bar",
                "nickname" → "baz"),
              "a4" → Map(
                "__typename" → "Cat",
                "name" → "foo",
                "age" → 10))),
          """{"v": 456}""".parseJson
        )
      }

      "executor should properly handle undefined values (like `null` and `None`) even if GraphQL type is not null" in {
        val schemaAst =
          graphql"""
            schema {
              query: Root
            }

            enum Color {RED, GREEN, BLUE}

            interface Letter {char: String}
            type A implements Letter {char: String}
            type B implements Letter {char: String}

            type Foo {
              barNone: Bar!
              barNull: Bar!
              listNone: [Bar]!
              listNull: [Bar]!
              intNone: Letter!
              intNull: Letter!
              scalarNone: String!
              scalarNull: String!
              enumNone: Color!
              enumNull: Color!
            }

            type Bar {
              baz: String
            }

            type Root {
              foo: Foo
            }
          """

        val customBuilder = new DefaultAstSchemaBuilder[Unit] {
          override def resolveField(typeDefinition: ast.TypeDefinition, definition: FieldDefinition) =
            if (definition.name == "foo")
              _ ⇒ Some(())
            else if (definition.name endsWith "None")
              _ ⇒ Value(None)
            else if (definition.name endsWith "Null")
              _ ⇒ Value(null)
            else
              _ ⇒ Value(None)
        }

        val schema = Schema.buildFromAst(schemaAst, customBuilder)

        checkContainsErrors(schema, (),
          """
            {
              foo {
                barNone {baz}
                barNull {baz}
                listNone {baz}
                listNull {baz}
                intNone {char}
                intNull {char}
                scalarNone
                scalarNull
                enumNone
                enumNull
              }
            }
          """,
          Map("foo" → null),
          List(
            """Cannot return null for non-nullable type""" → List(Pos(4, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(5, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(6, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(7, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(8, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(9, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(10, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(11, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(12, 17)),
            """Cannot return null for non-nullable type""" → List(Pos(13, 17)))
        )
      }
    }
  }
}
