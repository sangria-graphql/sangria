package sangria.marshalling

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class QueryAstMarshallingSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "QueryAstMarshalling" should {
    "Marshal and Unmarshal" in {
      import sangria.marshalling.queryAst._
      import sangria.macros._

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

      val result = Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, variables = args).await

      QueryRenderer.render(result, QueryRenderer.PrettyInput) should be (
        """{
          |  data: {
          |    human: {
          |      name: "Luke Skywalker"
          |      appearsIn: ["NEWHOPE", "EMPIRE", "JEDI"]
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
  }
}
