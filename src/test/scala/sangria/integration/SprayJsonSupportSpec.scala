package sangria.integration

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class SprayJsonSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "Json4sSupport" should {
    "Marshal and Unmarshal" in {
      import spray.json._
      import sangria.integration.SprayJsonSupport._

      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = JsObject("someId" -> JsString("1000"))

      val result = Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, arguments = Some(args)).await

      result.prettyPrint should be (
        """{
          |  "data": {
          |    "human": {
          |      "name": "Luke Skywalker"
          |    }
          |  }
          |}""".stripMargin)
    }
  }
}
