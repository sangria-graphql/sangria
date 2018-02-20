package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.ScalarTypeDefinition
import sangria.macros._
import sangria.ast
import sangria.marshalling.ScalaInput.scalaInput
import sangria.marshalling.sprayJson._
import sangria.parser.DeliveryScheme.Throw
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.{Pos, StringMatchers}
import sangria.validation.QueryValidator
import spray.json.{DefaultJsonProtocol, JsValue, pimpString}
import sangria.util.SimpleGraphQlSupport._

class InputDocumentMaterializerSpec extends WordSpec with Matchers with StringMatchers {
  case class Comment(author: String, text: Option[String])
  case class Article(title: String, text: Option[String], tags: Option[Vector[String]], comments: Vector[Option[Comment]])

  object MyJsonProtocol extends DefaultJsonProtocol {
    implicit val commentFormat = jsonFormat2(Comment.apply)
    implicit val articleFormat = jsonFormat4(Article.apply)
  }

  import MyJsonProtocol._
  
  val CommentType = InputObjectType[Comment]("Comment", List(
    InputField("author", OptionInputType(StringType), defaultValue = "anonymous"),
    InputField("text", OptionInputType(StringType))
  ))

  val ArticleType = InputObjectType[Article]("Article", List(
    InputField("title", StringType),
    InputField("text", OptionInputType(StringType), defaultValue = "Hello World!"),
    InputField("tags", OptionInputType(ListInputType(StringType))),
    InputField("comments", ListInputType(OptionInputType(CommentType)))))

  val ConfigType = InputObjectType[JsValue]("Config", List(
    InputField("hosts", ListInputType(StringType)),
    InputField("port", OptionInputType(IntType), 1234)))

  val anyValueBuilder = new DefaultAstSchemaBuilder[Any] {
    override def buildScalarType(
        origin: MatOrigin,
        extensions: Vector[ast.ScalarTypeExtensionDefinition],
        definition: ast.ScalarTypeDefinition,
        mat: AstSchemaMaterializer[Any]) =
      if (definition.directives.exists(_.name == "anyValue"))
        Some(ScalarType[Any](
          name = typeName(definition),
          description = typeDescription(definition),
          coerceUserInput = v ⇒ Right(v),
          coerceOutput = (v, _) ⇒ v,
          coerceInput = v ⇒ Right(v),
          complexity = scalarComplexity(definition),
          scalarInfo = scalarValueInfo(definition),
          astDirectives = definition.directives))
      else
        super.buildScalarType(origin, extensions, definition, mat)
  }

  "InputDocument materializer and validator" should {
    "validate input document" in {
      val schema = Schema.buildStubFromAst(
        gql"""
          enum Color {
            RED
            GREEN
            BLUE
          }

          input Foo {
            baz: Color!
          }

          input Config {
            foo: String
            bar: Int
            list: [Foo]
          }
        """)

      val inp =
        gqlInpDoc"""
          {
            foo: "bar"
            bar: "foo"
            list: [
              {baz: RED}
              {baz: FOO_BAR}
              {test: 1}
              {}
            ]
          }

          {
            doo: "hello"
          }
        """

      val errors = QueryValidator.default.validateInputDocument(schema, inp, "Config")

      assertViolations(errors,
        "Expected type 'Int', found '\"foo\"'. Int value expected" → Seq(Pos(4, 18)),
        "Expected type 'Color!', found 'FOO_BAR'. Enum value 'FOO_BAR' is undefined in enum type 'Color'. Known values are: RED, GREEN, BLUE." → Seq(Pos(7, 21)),
        "Field 'Foo.baz' of required type 'Color!' was not provided." → Seq(Pos(8, 15)),
        "Field 'test' is not defined by type 'Foo'." → Seq(Pos(8, 16)),
        "Field 'Foo.baz' of required type 'Color!' was not provided." → Seq(Pos(9, 15)),
        "Field 'doo' is not defined by type 'Config'; Did you mean foo?" → Seq(Pos(14, 13)))
    }

    "support `Any` value" in {
      val schema = Schema.buildStubFromAst(
        gql"""
          enum Color {
            RED
            GREEN
            BLUE
          }

          input Foo {
            baz: Color!
          }

          input Config {
            foo: String
            bar: Int
            test: Any!
          }

          # Any valid GraphQL value
          scalar Any @anyValue
        """, anyValueBuilder)

      val inp =
        gqlInpDoc"""
          {
            foo: "bar"
            bar: "foo"
          }

          {
            test: [
              {hello: "world"}
            ]
          }
        """

      val errors = QueryValidator.default.validateInputDocument(schema, inp, "Config")

      assertViolations(errors,
        "Field 'Config.test' of required type 'Any!' was not provided." → Seq(Pos(2, 11)),
        "Expected type 'Int', found '\"foo\"'. Int value expected" → Seq(Pos(4, 18)))
    }

    "support `to` with `FromInput` type class" in {
      val document = QueryParser.parseInputDocumentWithVariables(
        """
          {
            title: "foo",
            tags: null,
            comments: []
          }

          {
            title: "Article 2",
            text: "contents 2",
            tags: ["spring", "guitars"],
            comments: [{
              author: "Me"
              text: $comm
            }]
          }
        """)

      val vars = scalaInput(Map(
        "comm" → "from variable"
      ))

      document.to(ArticleType, vars) should be (
        Vector(
          Article("foo", Some("Hello World!"), None, Vector.empty),
          Article("Article 2", Some("contents 2"),
            Some(Vector("spring", "guitars")),
            Vector(Some(Comment("Me", Some("from variable")))))))
    }

    "support `to` with `FromInput` type class (raw json value)" in {
      val document = QueryParser.parseInputDocumentWithVariables(
        """{hosts: ["localhost", "127.0.0.1"]}""")

      val res = document to ConfigType

      res should have size 1

      res(0) should be (
        """
          {
            "hosts": ["localhost", "127.0.0.1"],
            "port": 1234
          }
        """.parseJson)
    }
  }
}
