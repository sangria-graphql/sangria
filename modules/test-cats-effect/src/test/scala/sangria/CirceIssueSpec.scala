package sangria

import sangria.schema._
import sangria.macros._
import sangria.marshalling.circe._
import io.circe.Json
import io.circe.literal._
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.execution.Executor
import sangria.marshalling.InputUnmarshaller
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
            accounts: [Account!]!
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

      val response2 = List(
        json"""
      {
        "id": "1000",
        "name": "Luke Skywalker"
      }""",
        json"""
      {
        "id": "1001",
        "name": "Darth Vader"
      }"""
      )

      val builder = AstSchemaBuilder.resolverBased[Unit](
        FieldResolver.map[Unit](
          "Query" -> Map(
            "accounts" -> (_ => response)
          )
        ),
        AnyFieldResolver[Unit] {
          case origin if !origin.isInstanceOf[ExistingSchemaOrigin[_, _]] =>
            extractFieldValue[Unit, Json]
        }
      )

      val schema = Schema.buildFromAst(schemaAst, builder)

      val result = Executor.execute(schema, query)
      println(result.await)
    }
  }

  def extractFieldValue[Ctx, In](context: Context[Ctx, _])(implicit
      iu: InputUnmarshaller[In]): Any = {
    val parentType = context.parentType
    val field = context.field
    val value = context.value.asInstanceOf[In]
    // there is not way to get a complete json object from iu
    ResolverBasedAstSchemaBuilder.extractValue(field.fieldType, iu.getMapValue(value, field.name))
  }

}
