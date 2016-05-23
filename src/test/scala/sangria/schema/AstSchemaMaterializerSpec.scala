package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.renderer.SchemaRenderer
import sangria.util.{StringMatchers, FutureResultSupport}
import sangria.parser.DeliveryScheme.Throw
import sangria.macros._

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
          """schema {
            |  query: Query
            |}
            |
            |type Concrete implements Iface {
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
          """schema {
            |  query: Query
            |}
            |
            |type Concrete {
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
          """schema {
            |  query: Query
            |}
            |
            |enum MyEnum {
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

        error.getMessage should be ("Must provide a schema definition.")
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

        error.getMessage should be ("Must provide one query type in schema.")
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

        error.getMessage should be ("Must provide one query type in schema.")
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

    "Schema Builder contains descriptions" should {
      "Use comments for descriptions" in {
        val schemaDef =
          """schema {
            |  query: Query
            |}
            |
            |## fooo bar
            |## baz
            |enum MyEnum {
            |  ## value1
            |  VALUE
            |
            |  ## value 2
            |  ## line 2
            |  OLD_VALUE @deprecated
            |
            |  ## value 3
            |  OTHER_VALUE @deprecated(reason: "Terrible reasons")
            |}
            |
            |## My super query!
            |type Query {
            |  field1: String @deprecated
            |
            |  ## the second field!
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
            |## My super query!
            |type Query {
            |
            |  # not a description!
            |  field1(
            |    ## first arg
            |    arg1: Int = 101,
            |
            |    ## secnd arg
            |    ## line 2
            |
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
  }
}
