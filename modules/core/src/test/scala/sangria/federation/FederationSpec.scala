package sangria.federation

import scala.util.Success

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.json4s.JValue
import sangria.util.FutureResultSupport
import sangria.macros._
import sangria.execution.{ExecutionScheme, Executor}
import sangria.parser.QueryParser
import sangria.schema.{AstSchemaBuilder, FieldResolver, Schema}

class FederationSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  import FederationSpec._

  "To support Apollo Federation, an unmarshaller" should {
    "accept a map value as a scalar" in {
      val AST =
        graphql"""
          schema {
            query: Query
          }

          type Query {
            states: [State]
          }

          type State @key(fields : "id") {
            id: Int!
            value: String!
          }
        """

      val (schema, um) = Federation.federate[Any, JValue](
        Schema.buildFromAst(
          AST,
          AstSchemaBuilder.resolverBased[Any](
            FieldResolver.map(
              "Query" -> Map(
                "states" -> (_ => State(1, "one") :: Nil)
              )
            ),
            FieldResolver.map(
              "State" -> Map(
                "id" -> (ctx => ctx.value.asInstanceOf[State].id),
                "value" -> (ctx => ctx.value.asInstanceOf[State].value)
              )),
          )),
        sangria.marshalling.json4s.jackson.Json4sJacksonInputUnmarshaller,
        stateResolver)

      val Success(query) = QueryParser.parse("""
        query FetchState($representations: [_Any!]!) {
          _entities(representations: $representations) {
            __typename
            ... on State {
              id
              value
           }
          }
        }
        """)

      val args: org.json4s.JValue = org.json4s.jackson.JsonMethods
        .parse(""" { "representations": [{ "__typename": "State", "id": 1 }] } """)

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
        |    _entities: [{
        |      __typename: "State"
        |      id: 0
        |      value: "initial"
        |    }]
        |  }
        |}""".stripMargin)
    }
  }
}

object FederationSpec {

  case class State(
    id: Int,
    value: String)

  case class StateArg(id: Int)

  implicit val decoder = new Decoder[JValue, StateArg] {
    override def decode(node: JValue): Either[Exception, StateArg] = Right(StateArg(0))
  }

  val stateResolver = EntityResolver[Any, JValue, State, StateArg](
    __typeName = "State",
    decoder,
    _ => Some(State(0, "initial")))
}
