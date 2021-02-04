package sangria.federation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.util.FutureResultSupport
import sangria.macros._
import sangria.execution.{ExecutionScheme, Executor}
import sangria.marshalling.InputUnmarshaller
import sangria.parser.QueryParser
import sangria.schema.{AdditionalTypes, Argument, AstSchemaBuilder, FieldResolver, Schema}
import sangria.validation.Violation

import scala.util.Success

class FederationSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  import FederationSpec._

  "To support Apollo Federation, an unmarshaller" should {
    "accept a map value as a scalar" in {
      val AST =
        graphql"""
          schema {
            query: Query
          }

          type Query

          type State @key(fields : "id") {
            id: Int!
            value: String!
          }
        """

      val stateResolver = EntityResolver[Any, State, StateArg](
         __typeName = "State",
        arg => Some(State(arg.id, arg.id.toString)))

      val (schema, um) = Federation.federate(
        Schema.buildFromAst(AST),
        sangria.marshalling.json4s.jackson.Json4sJacksonInputUnmarshaller)

      val Success(query) = QueryParser.parse("""
        query FetchState($representation: [_Any!]!) {
          entities(representation: $representation) {
            __typename
            ... on State {
              id
              value
            }
          }
        }
        """)

      val args: org.json4s.JValue = org.json4s.jackson.JsonMethods
        .parse(""" { "representation": [{ "__typename": "State", "id": 1 }] } """)

      val result = Executor
        .execute(schema = schema, queryAst = query, variables = args)(
          executionContext = scala.concurrent.ExecutionContext.global,
          marshaller = sangria.marshalling.queryAst.queryAstResultMarshaller,
          um = um,
          scheme = ExecutionScheme.Default
        )
        .await

      result.renderPretty should be("""{
        |  data: {
        |    entities: "{\"__typename\":\"State\",\"id\":1,\"value\":\"1\"}"
        |  }
        |}""".stripMargin)
    }
  }
}

object FederationSpec {

  case class State(
    id: Int,
    key: String)

  case class StateArg(id: Int)
}
