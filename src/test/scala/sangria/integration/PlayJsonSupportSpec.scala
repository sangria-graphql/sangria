package sangria.integration

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class PlayJsonSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "Json4sSupport" should {
    "Marshal and Unmarshal" in {
      import sangria.integration.PlayJsonSupport._
      import play.api.libs.json._

      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = JsObject(Seq("someId" -> JsString("1000")))

      val result = Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, arguments = Some(args)).await

      Json.prettyPrint(result).replace("\r", "") should be (
        """{
          |  "data" : {
          |    "human" : {
          |      "name" : "Luke Skywalker"
          |    }
          |  }
          |}""".stripMargin)
    }
  }
}
