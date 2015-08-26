package sangria.integration

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class CirceSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "CircleSupport" should {
    "Marshal and Unmarshal" in {
      import io.circe._
      import sangria.integration.CirceSupport._

      val Success(query) = QueryParser.parse("""
        query NestedQuery {
          hero {
            name
            friends {
              name
              appearsIn
              friends {
                name
              }
            }
          }
        }
        """)

      val args = Json.obj("someId" -> Json.string("1000"))

      val result = Executor.execute(StarWarsSchema, query,
        arguments = Some(args),
        userContext = new CharacterRepo,
        deferredResolver = new FriendsResolver).await

      println(result.spaces2)

      result.spaces2 should be (
        """{
          |  "data" : {
          |    "hero" : {
          |      "name" : "R2-D2",
          |      "friends" : [
          |        {
          |          "name" : "Luke Skywalker",
          |          "appearsIn" : [
          |            "NEWHOPE",
          |            "EMPIRE",
          |            "JEDI"
          |          ],
          |          "friends" : [
          |            {
          |              "name" : "Han Solo"
          |            },
          |            {
          |              "name" : "Leia Organa"
          |            },
          |            {
          |              "name" : "C-3PO"
          |            },
          |            {
          |              "name" : "R2-D2"
          |            }
          |          ]
          |        },
          |        {
          |          "name" : "Han Solo",
          |          "appearsIn" : [
          |            "NEWHOPE",
          |            "EMPIRE",
          |            "JEDI"
          |          ],
          |          "friends" : [
          |            {
          |              "name" : "Luke Skywalker"
          |            },
          |            {
          |              "name" : "Leia Organa"
          |            },
          |            {
          |              "name" : "R2-D2"
          |            }
          |          ]
          |        },
          |        {
          |          "name" : "Leia Organa",
          |          "appearsIn" : [
          |            "NEWHOPE",
          |            "EMPIRE",
          |            "JEDI"
          |          ],
          |          "friends" : [
          |            {
          |              "name" : "Luke Skywalker"
          |            },
          |            {
          |              "name" : "Han Solo"
          |            },
          |            {
          |              "name" : "C-3PO"
          |            },
          |            {
          |              "name" : "R2-D2"
          |            }
          |          ]
          |        }
          |      ]
          |    }
          |  }
          |}""".stripMargin)
    }
  }
}
