package sangria.schema

import sangria.ast
import sangria.ast.{FieldDefinition, ObjectTypeDefinition, ObjectTypeExtensionDefinition, TypeDefinition}
import sangria.execution.{Executor, MaterializedSchemaValidationError}
import sangria.parser.QueryParser
import sangria.renderer.SchemaRenderer
import sangria.util.{DebugUtil, FutureResultSupport, Pos, StringMatchers}
import sangria.parser.DeliveryScheme.Throw
import sangria.macros._
import sangria.macros.derive._
import sangria.validation.{IntCoercionViolation, UnknownDirectiveViolation}
import sangria.util.SimpleGraphQlSupport.{check, checkContainsErrors}
import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class AstSchemaMaterializerSpec extends AnyWordSpec with Matchers with FutureResultSupport with StringMatchers {

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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Must provide schema definition with query type or a type named Query.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Must provide only one schema definition.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Must provide only one query type in schema.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Must provide only one query type in schema.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Must provide only one mutation type in schema.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Must provide only one subscription type in schema.")
      }

      "Unknown type in interface list" in {
        val ast =
          graphql"""
            schema {
              query: Hello
            }

            type Hello implements Bar {
              foo: String
            }
          """

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Unknown type 'Bar'.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Unknown type 'Bar'.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Unknown type 'Wat'.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Unknown type 'Wat'.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Unknown type 'Awesome'.")
      }

      "Does not consider operation names" in {
        val ast =
          graphql"""
            schema {
              query: Foo
            }

            query Foo { field }
          """

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Unknown type 'Foo'.")
      }

      "Does not consider fragment names" in {
        val ast =
          graphql"""
            schema {
              query: Foo
            }

            fragment Foo on Type { field }
          """

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Unknown type 'Foo'.")
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

            extend type Query implements Bar & Baz {
              field3: String
              field4: String
              add: Int
            }
          """

        val schema = Schema.buildFromAst(QueryParser.parse(schemaDef))

        val query = schema.outputTypes("Query").asInstanceOf[ObjectType[_, _]]

        query.ownFields.map(_.name) should be (List("field1", "field2", "field3", "field4", "add"))
        query.interfaces.map(_.name) should be (List("Foo", "Bar", "Baz"))
      }

      "allow extending static schema" in {
        import sangria.marshalling.sprayJson._

        case class Article(id: String, title: String, text: String, author: Option[String])

        class Repo {
          def loadArticle(id: String): Option[Article] =
            Some(Article(id, s"Test Article #$id", "blah blah blah...", Some("Bob")))

          def loadComments: List[JsValue] =
            List(JsObject(
              "text" -> JsString("First!"),
              "author" -> JsObject(
                "name" -> JsString("Jane"),
                "lastComment" -> JsObject(
                  "text" -> JsString("Boring...")))))
        }

        val ArticleType = deriveObjectType[Repo, Article]()

        val IdArg = Argument("id", StringType)

        val QueryType = ObjectType("Query", fields[Repo, Unit](
          Field("article", OptionType(ArticleType),
            arguments = IdArg :: Nil,
            resolve = c => c.ctx.loadArticle(c arg IdArg))))

        val staticSchema = Schema(QueryType)

        val extensions =
          gql"""
            extend type Article {
              comments: [Comment]! @loadComments
            }

            type Comment {
              text: String!
              author: CommentAuthor!
            }

            type CommentAuthor {
              name: String!
              lastComment: Comment
            }
          """

        val builder = new DefaultAstSchemaBuilder[Repo] {
          override def resolveField(
              origin: MatOrigin,
              typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Repo, _]],
              extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
              definition: ast.FieldDefinition,
              mat: AstSchemaMaterializer[Repo]) =
            if (definition.directives.exists(_.name == "loadComments"))
              c => c.ctx.loadComments
            else
              c => resolveJson(c.field.name, c.field.fieldType, c.value.asInstanceOf[JsValue])

          def resolveJson(name: String, tpe: OutputType[_], json: JsValue): Any = tpe match {
            case OptionType(ofType) => resolveJson(name, ofType, json)
            case ListType(ofType) => json.asInstanceOf[JsArray].elements.map(resolveJson(name, ofType, _))
            case StringType => json.asJsObject.fields(name).asInstanceOf[JsString].value
            case _ if json.asJsObject.fields(name).isInstanceOf[JsObject] => json.asJsObject.fields(name)
            case t => throw new IllegalStateException(s"Type ${SchemaRenderer.renderTypeName(t)} is not supported")
          }
        }

        val schema = staticSchema.extend(extensions, builder)

        val query =
          gql"""
            {
              article(id: "42") {
                title
                text
                comments {
                  text
                  author {
                    name
                    lastComment {
                      text
                    }
                  }
                }
              }
            }
          """

        Executor.execute(schema, query, new Repo).await should be (
          """
            {
              "data": {
                "article": {
                  "title": "Test Article #42",
                  "text": "blah blah blah...",
                  "comments": [{
                    "text": "First!",
                    "author": {
                      "name": "Jane",
                      "lastComment": {
                        "text": "Boring..."
                      }
                    }
                  }]
                }
              }
            }
          """.parseJson)
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

        val error = intercept [SchemaValidationException] (Schema.buildFromAst(ast))

        error.getMessage should include ("Object type 'Query' can implement interface 'Foo' only once.")
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

        val error = intercept [SchemaValidationException] (Schema.buildFromAst(ast))

        error.getMessage should include ("Object type 'Query' can include field 'field1' only once.")
      }

      "accepts an Input Object with breakable circular reference" in {
        val ast =
          graphql"""
            schema {
              query: Query
            }

            type Query {
              field(arg: SomeInputObject): String
            }

            input SomeInputObject {
              self: SomeInputObject
              arrayOfSelf: [SomeInputObject]
              nonNullArrayOfSelf: [SomeInputObject]!
              nonNullArrayOfNonNullSelf: [SomeInputObject!]!
              intermediateSelf: AnotherInputObject
            }

            input AnotherInputObject {
              parent: SomeInputObject
            }
          """

        noException should be thrownBy (Schema.buildFromAst(ast))
      }

      "rejects an Input Object with non-breakable circular reference" in {
        val ast =
          graphql"""
            schema {
              query: Query
            }

            type Query {
              field(arg: SomeInputObject): String
            }

            input SomeInputObject {
              nonNullSelf: SomeInputObject!
            }
          """

        val error = intercept [SchemaValidationException] (Schema.buildFromAst(ast))

        error.getMessage should include ("Cannot reference InputObjectType 'SomeInputObject' within itself through a series of non-null fields: 'nonNullSelf'.")
      }

      "rejects Input Objects with non-breakable circular reference spread across them" in {
        val ast =
          graphql"""
            schema {
              query: Query
            }

            type Query {
              field(arg: SomeInputObject): String
            }

            input SomeInputObject {
              startLoop: AnotherInputObject!
            }

            input AnotherInputObject {
              nextInLoop: YetAnotherInputObject!
            }

            input YetAnotherInputObject {
              closeLoop: SomeInputObject!
            }
          """

        val error = intercept [SchemaValidationException] (Schema.buildFromAst(ast))

        error.getMessage should include ("Cannot reference InputObjectType 'SomeInputObject' within itself through a series of non-null fields: 'startLoop.nextInLoop.closeLoop'.")
      }

      "rejects Input Objects with multiple non-breakable circular reference" in {
        val ast =
          graphql"""
            schema {
              query: Query
            }

            type Query {
              field(arg: SomeInputObject): String
            }

            input SomeInputObject {
              startLoop: AnotherInputObject!
            }

            input AnotherInputObject {
              closeLoop: SomeInputObject!
              startSecondLoop: YetAnotherInputObject!
            }

            input YetAnotherInputObject {
              closeSecondLoop: AnotherInputObject!
              nonNullSelf: YetAnotherInputObject!
            }
          """

        val error = intercept [SchemaValidationException] (Schema.buildFromAst(ast))

        error.getMessage should include ("Cannot reference InputObjectType 'SomeInputObject' within itself through a series of non-null fields: 'startLoop.closeLoop'.")
        error.getMessage should include ("Cannot reference InputObjectType 'AnotherInputObject' within itself through a series of non-null fields: 'closeLoop.startLoop'.")
        error.getMessage should include ("Cannot reference InputObjectType 'YetAnotherInputObject' within itself through a series of non-null fields: 'nonNullSelf'.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Cannot extend type 'Foo' because it does not exist.")
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

        val error = intercept [MaterializedSchemaValidationError] (Schema.buildFromAst(ast))

        error.getMessage should include ("Cannot extend non-object type 'Foo'.")
      }
    }

    "Schema Builder contains descriptions" should {
      val quotes = "\"\"\""

      "Use SDL descriptions" in {
        val schemaDef =
          s"""$quotes
             |test schema
             |descr
             |$quotes
             |schema {
             |  query: Query
             |}
             |
             |$quotes
             |fooo bar
             |baz
             |$quotes
             |enum MyEnum {
             |  "value1"
             |  VALUE
             |
             |  $quotes
             |  value 2
             |  line 2
             |  $quotes
             |  OLD_VALUE @deprecated
             |
             |  "value 3"
             |  OTHER_VALUE @deprecated(reason: "Terrible reasons")
             |}
             |
             |"My super query!"
             |type Query {
             |  field1: String @deprecated
             |
             |  "the second field!"
             |  field2: Int @deprecated(reason: "Because I said so")
             |  enum: MyEnum
             |}""".stripMargin

        cycleOutput(schemaDef) should equal (schemaDef) (after being strippedOfCarriageReturns)

        val schema = Schema.buildFromAst(QueryParser.parse(schemaDef))

        schema.description should be (Some("test schema\ndescr"))

        val myEnum = schema.inputTypes("MyEnum").asInstanceOf[EnumType[_]]

        myEnum.description should be (Some("fooo bar\nbaz"))
        myEnum.values(0).description should be (Some("value1"))
        myEnum.values(1).description should be (Some("value 2\nline 2"))
        myEnum.values(2).description should be (Some("value 3"))

        val query = schema.outputTypes("Query").asInstanceOf[ObjectType[_, _]]

        query.description should be (Some("My super query!"))
        query.fields(1).description should be (Some("the second field!"))
      }

      "Support legacy comment-based SDL descriptions" in {
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

        val schema = Schema.buildFromAst(QueryParser.parse(schemaDef),
          AstSchemaBuilder.defaultWithLegacyCommentDescriptions)

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
          s"""schema {
             |  query: Query
             |}
             |
             |"My super query!"
             |type Query {
             |
             |  # not a description!
             |  field1(
             |    "first arg"
             |    arg1: Int = 101,
             |
             |    # This should not
             |    # be part of the description
             |
             |    $quotes
             |    secnd arg
             |    line 2
             |    $quotes
             |    arg2: String!
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

      "support type extensions" in {
        val schemaDef =
          graphql"""
            schema {
              query: Query
            }

            "My super query!"
            type Query

            input Complex {
              name: String = "test"
            }

            enum Color {
             Red, Green
            }

            interface Cool

            interface Versioned {
              id: ID!
            }

            union Pet @noTypesHere

            extend type Query implements Cool @coolDir {
              # not a description!
              field1(
                "first arg"
                arg1: Int = 101,
                arg2: String!
              ): String
            }

            extend type Query implements Versioned @hello {
              id: ID!
              version: Long!
              field2(a: Complex): Int
            }

            extend input Complex @advanced {
              color: Color = Blue
            }

            extend enum Color @extra(id: 123) {
              "forgot"
              Blue
            }

            extend interface Versioned @test {
              version: Long!
            }

            extend interface Cool {
              field1(arg1: Int = 101, arg2: String!): String
              pet: Pet
            }

            type Dog {name: String}
            type Cat {cute: Boolean, size: PositiveInt!}

            extend union Pet @yay = Dog | Cat

            extend scalar PositiveInt @intConstraint(min: 0)
            scalar PositiveInt
          """

        val errors = ResolverBasedAstSchemaBuilder().validateSchema(schemaDef)

        errors should have size 8

        errors.foreach(_.isInstanceOf[UnknownDirectiveViolation] should be (true))

        val schema = Schema.buildFromAst(schemaDef)
        
        ("\n" + schema.renderPretty + "\n") should equal("""
          |type Cat {
          |  cute: Boolean
          |  size: PositiveInt!
          |}
          |
          |enum Color @extra(id: 123) {
          |  Red
          |  Green
          |
          |  "forgot"
          |  Blue
          |}
          |
          |input Complex @advanced {
          |  name: String = "test"
          |  color: Color = Blue
          |}
          |
          |interface Cool {
          |  field1(arg1: Int = 101, arg2: String!): String
          |  pet: Pet
          |}
          |
          |type Dog {
          |  name: String
          |}
          |
          |union Pet @noTypesHere @yay = Dog | Cat
          |
          |scalar PositiveInt @intConstraint(min: 0)
          |
          |"My super query!"
          |type Query implements Cool & Versioned @coolDir @hello {
          |  field1(
          |    "first arg"
          |    arg1: Int = 101, arg2: String!): String
          |  id: ID!
          |  version: Long!
          |  field2(a: Complex): Int
          |  pet: Pet
          |}
          |
          |interface Versioned @test {
          |  id: ID!
          |  version: Long!
          |}
          |""".stripMargin) (after being strippedOfCarriageReturns)
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

        val ReturnCat = Directive("returnCat", locations = Set(DirectiveLocation.FieldDefinition), shouldInclude = _ => true)
        val ReturnDog = Directive("returnDog", locations = Set(DirectiveLocation.FieldDefinition), shouldInclude = _ => true)

        val customBuilder = new DefaultAstSchemaBuilder[Unit] {
          override def resolveField(
              origin: MatOrigin,
              typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Unit, _]],
              extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
              definition: ast.FieldDefinition,
              mat: AstSchemaMaterializer[Unit]) =
            if (definition.directives.exists(_.name == ReturnCat.name))
              _ => Map("type" -> "Cat", "name" -> "foo", "age" -> Some(10))
            else if (definition.directives.exists(_.name == ReturnDog.name))
              _ => Map("type" -> "Dog", "name" -> "bar", "nickname" -> Some("baz"))
            else if (definition.name == "add")
              ctx => ctx.arg[Int]("a") + ctx.arg[Int]("b")
            else
              _.value.asInstanceOf[Map[String, Any]](definition.name)

          override def objectTypeInstanceCheck(origin: MatOrigin, definition: ObjectTypeDefinition, extensions: List[ast.ObjectTypeExtensionDefinition]) =
            Some((value, _) => value.asInstanceOf[Map[String, Any]]("type") == definition.name)

          override def scalarCoerceUserInput(definition: ast.ScalarTypeDefinition) =
            value => definition.name match {
              case "Custom" => value match {
                case i: Int => Right(i)
                case i: BigInt => Right(i.intValue)
                case _ => Left(IntCoercionViolation)
              }
              case _ => Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)
            }

          override def scalarCoerceInput(definition: ast.ScalarTypeDefinition) =
            value => definition.name match {
              case "Custom" => value match {
                case ast.IntValue(i, _, _) => Right(i)
                case ast.BigIntValue(i, _, _) => Right(i.intValue)
                case _ => Left(IntCoercionViolation)
              }
              case _ => Left(DefaultIntrospectionSchemaBuilder.MaterializedSchemaViolation)
            }

          override def scalarCoerceOutput(definition: ast.ScalarTypeDefinition) =
            (coerced, _) => definition.name match {
              case "Custom" => ast.IntValue(coerced.asInstanceOf[Int])
              case _ => throw DefaultIntrospectionSchemaBuilder.MaterializedSchemaException
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
          Map("data" ->
            Map(
              "add1" -> 133,
              "add2" -> 153,
              "add3" -> 912,
              "a1" -> Map(
                "name" -> "bar",
                "__typename" -> "Dog"),
              "a2" -> Map(
                "name" -> "foo",
                "__typename" -> "Cat"),
              "a3" -> Map(
                "__typename" -> "Dog",
                "name" -> "bar",
                "nickname" -> "baz"),
              "a4" -> Map(
                "__typename" -> "Cat",
                "name" -> "foo",
                "age" -> 10))),
          """{"v": 456}""".parseJson
        )
      }

      "allows schema extensions" in {
        val schemaAst =
          graphql"""
            extend schema @fromExt(test: true) {
              mutation: Mut
            }

            schema {
              query: Root
            }

            type Root {
              test1: String
            }

            type Mut {
              addUser(name: String!): String
            }
          """

        val customBuilder = new DefaultAstSchemaBuilder[Unit] {
          override def resolveField(
            origin: MatOrigin,
            typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Unit, _]],
            extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
            definition: ast.FieldDefinition,
            mat: AstSchemaMaterializer[Unit]) = c => "test " + c.arg[String]("name")
        }

        val schema = Schema.buildFromAst(schemaAst, customBuilder)

        check(schema, (),
          """
            mutation {
              addUser(name: "Bob")
            }
          """,
          Map("data" -> Map("addUser" -> "test Bob")))

        schema.astDirectives.exists(_.name == "fromExt") should be (true)
      }

      "allows schema extensions without an explicit schema def" in {
        val schemaAst =
          graphql"""
            extend schema @fromExt(test: true) {
              mutation: Mut
            }

            type Query {
              test1: String
            }

            type Mut {
              addUser(name: String!): String
            }
          """

        val customBuilder = new DefaultAstSchemaBuilder[Unit] {
          override def resolveField(
            origin: MatOrigin,
            typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Unit, _]],
            extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
            definition: ast.FieldDefinition,
            mat: AstSchemaMaterializer[Unit]) = c => "test " + c.arg[String]("name")
        }

        val schema = Schema.buildFromAst(schemaAst, customBuilder)

        check(schema, (),
          """
            mutation {
              addUser(name: "Bob")
            }
          """,
          Map("data" -> Map("addUser" -> "test Bob")))

        schema.astDirectives.exists(_.name == "fromExt") should be (true)
      }

      "allows schema extensions for existing schema" in {
        val schemaAst =
          graphql"""
            extend schema @fromExt(test: true) {
              mutation: Mut
            }

            type Mut {
              addUser(name: String!): String
            }
          """

        val existingSchema =
          Schema(ObjectType("Query", fields[Unit, Unit](
            Field("test", StringType, resolve = _ => "test"))))

        val customBuilder = new DefaultAstSchemaBuilder[Unit] {
          override def resolveField(
            origin: MatOrigin,
            typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Unit, _]],
            extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
            definition: ast.FieldDefinition,
            mat: AstSchemaMaterializer[Unit]) = c => "test " + c.arg[String]("name")
        }

        val schema = existingSchema.extend(schemaAst, customBuilder)

        check(schema, (),
          """
            mutation {
              addUser(name: "Bob")
            }
          """,
          Map("data" -> Map("addUser" -> "test Bob")))

        schema.astDirectives.exists(_.name == "fromExt") should be (true)
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
          override def resolveField(
              origin: MatOrigin,
              typeDefinition: Either[ast.TypeDefinition, ObjectLikeType[Unit, _]],
              extensions: Vector[ast.ObjectLikeTypeExtensionDefinition],
              definition: ast.FieldDefinition,
              mat: AstSchemaMaterializer[Unit]) =
            if (definition.name == "foo")
              _ => Some(())
            else if (definition.name endsWith "None")
              _ => Value(None)
            else if (definition.name endsWith "Null")
              _ => Value(null)
            else
              _ => Value(None)
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
          Map("foo" -> null),
          List(
            """Cannot return null for non-nullable type""" -> List(Pos(4, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(5, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(6, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(7, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(8, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(9, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(10, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(11, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(12, 17)),
            """Cannot return null for non-nullable type""" -> List(Pos(13, 17)))
        )
      }
    }
  }
}
