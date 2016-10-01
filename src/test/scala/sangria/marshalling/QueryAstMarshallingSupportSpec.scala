package sangria.marshalling

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.marshalling.testkit.{InputHandlingBehaviour, MarshallingBehaviour, ParsingBehaviour}
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.starWars.TestSchema._
import sangria.ast
import sangria.marshalling.queryAst._
import sangria.macros._
import sangria.util.FutureResultSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class QueryAstMarshallingSupportSpec extends WordSpec with Matchers with FutureResultSupport with MarshallingBehaviour with InputHandlingBehaviour with ParsingBehaviour {
  "QueryAstMarshalling" should {
    behave like `value (un)marshaller` (queryAstResultMarshaller)

    behave like `AST-based input unmarshaller` (queryAstFromInput[ast.Value])
    behave like `AST-based input marshaller` (queryAstResultMarshaller)

    behave like `input parser` (ParseTestSubjects(
      complex = "{a: [null, 123, [{foo: \"bar\"}]], b: {c: true, d: null}}",
      simpleString = "\"bar\"",
      simpleInt = "12345",
      simpleNull = "null",
      list = "[\"bar\", 1, null, true, [1, 2, 3]]",
      syntaxError = List("[123, FOO BAR")
    ))

    "marshal and unmarshal" in {
      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
            appearsIn
            friends {
              id
              name
            }
          }
        }
        """)

      val args = graphqlInput"""{someId: "1000"}"""

      val result = Executor.execute(StarWarsSchema, query, new CharacterRepo,
        variables = args,
        deferredResolver = new FriendsResolver).await

      QueryRenderer.render(result, QueryRenderer.PrettyInput) should be (
        """{
          |  data: {
          |    human: {
          |      name: "Luke Skywalker"
          |      appearsIn: [NEWHOPE, EMPIRE, JEDI]
          |      friends: [{
          |        id: "1002"
          |        name: "Han Solo"
          |      }, {
          |        id: "1003"
          |        name: "Leia Organa"
          |      }, {
          |        id: "2000"
          |        name: "C-3PO"
          |      }, {
          |        id: "2001"
          |        name: "R2-D2"
          |      }]
          |    }
          |  }
          |}""".stripMargin)
    }

    val toRender = ast.ObjectValue(List(
      ast.ObjectField("a", ast.ListValue(List(ast.NullValue(), ast.IntValue(123), ast.ListValue(List(ast.ObjectValue(List(ast.ObjectField("foo", ast.StringValue("bar"))))))))),
      ast.ObjectField("b", ast.ObjectValue(List(
        ast.ObjectField("c", ast.BooleanValue(true)),
        ast.ObjectField("d", ast.NullValue()))))))

    "InputUnmarshaller" should {
      "throw an exception on invalid scalar values" in {
        an [IllegalStateException] should be thrownBy
            queryAstInputUnmarshaller.getScalaScalarValue(ast.NullValue())
      }

      "handle variable names" in {
        queryAstInputUnmarshaller.getVariableName(ast.VariableValue("foo")) should be ("foo")
      }

      "render JSON values" in {
        val rendered = queryAstInputUnmarshaller.render(toRender)

        rendered should be ("""{a:[null,123,[{foo:"bar"}]],b:{c:true,d:null}}""")
      }
    }

    "ResultMarshaller" should {
      "render pretty JSON values" in {
        val rendered = queryAstResultMarshaller.renderPretty(toRender)

        rendered should be ("""{a: [null, 123, [{foo: "bar"}]], b: {c: true, d: null}}""")
      }

      "render compact JSON values" in {
        val rendered = queryAstResultMarshaller.renderCompact(toRender)

        rendered should be ("""{a:[null,123,[{foo:"bar"}]],b:{c:true,d:null}}""")
      }
    }
  }
}
