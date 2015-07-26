package sangria.integration

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.starWars.TestData.{FriendsResolver, CharacterRepo}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class Json4sSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "Json4sSupport" should {
    "Marshal and Unmarshal" in {
      import Json4sSupport._
      import org.json4s.native.JsonMethods._
      import org.json4s.JsonAST._

      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = JObject("someId" -> JString("1000"))

      val result = Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, arguments = Some(args)).await

      pretty(render(result)) should be (
        """{
          |  "data":{
          |    "human":{
          |      "name":"Luke Skywalker"
          |    }
          |  }
          |}""".stripMargin)
    }
  }
}
