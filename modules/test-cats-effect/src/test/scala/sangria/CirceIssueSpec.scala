package sangria

import sangria.schema._
import sangria.macros._
import sangria.marshalling.circe._
import io.circe.Json
import io.circe.literal._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.execution.Executor
import sangria.util.FutureResultSupport

class CirceIssueSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  "circe issue" must {
    "be reproduced" in {
      implicit val ec: scala.concurrent.ExecutionContext =
        scala.concurrent.ExecutionContext.global

      val schemaAst =
        gql"""
          type Account {
            id: String!
            name: String!
          }

          type Query {
            accounts: [Account!]
          }
        """

      val query =
        gql"""
              query {
                accounts {
                  id
                  name
                }
              }
            """

      val response =
        json"""
          [
            {
              "id": "1000",
              "name": "Luke Skywalker"
            },
            {
              "id": "1001",
              "name": "Darth Vader"
            }]"""

      val builder = AstSchemaBuilder.resolverBased[Unit](
        FieldResolver.map[Unit](
          "Query" -> Map(
            "accounts" -> (_ => response.asArray)
          )
        ),
        AnyFieldResolver.defaultInput[Unit, Json]
      )

      val schema = Schema.buildFromAst(schemaAst, builder)

      val result = Executor.execute(schema, query)
      println(result.await)
    }
  }

}
