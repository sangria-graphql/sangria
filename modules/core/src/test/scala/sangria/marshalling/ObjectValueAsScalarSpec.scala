package sangria.marshalling

import scala.util.Success

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.execution.Executor
import sangria.macros._
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer
import sangria.util.FutureResultSupport

class ObjectValueAsScalarSpec extends AnyWordSpec with Matchers with FutureResultSupport {

  import sangria.schema._

  "Unmarshaller" should {
    "accept an object value as a scalar parameter" in {
      val ast =
        graphql"""
          schema {
            query: Query
          }

          type Query {
            entities(representation: _Any!): _Any
          }

          union _Entity = State

          type State {
            id: Int!
            value: String!
          }
        """

      val representationArg = Argument("representation", _Any.Type)

      val schema: Schema[Any, Any] = Schema.buildFromAst(
        ast,
        AstSchemaBuilder.resolverBased[Any](
          FieldResolver.map(
            "Query" -> Map(
              "entities" -> (ctx => ctx.withArgs(representationArg)(arg => arg))
            )
          ),
          AdditionalTypes(_Any.Type)))

      val Success(query) = QueryParser.parse("""
        query FetchState {
          entities(representation: { __typename: "State", id: 1 })
        }
        """)

      import sangria.marshalling.queryAst._
      import scala.concurrent.ExecutionContext.Implicits.global

      val result = Executor
        .execute(
          schema,
          query)
        .await

      QueryRenderer.render(result, QueryRenderer.PrettyInput) should be(
        """{
        |  data: {
        |    entities: "ListMap(__typename -> State, id -> 1)"
        |  }
        |}""".stripMargin)
    }
  }
}

case class _Any(fields: Map[String, Any])

object _Any {

  import sangria.validation.ValueCoercionViolation

  case object AnyCoercionViolation extends
    ValueCoercionViolation("_Any value expected")

  import sangria.ast._
  import sangria.marshalling.MarshallingUtil._
  import sangria.util.tag.@@
  import sangria.marshalling.scalaMarshalling._
  import sangria.schema.ScalarType
  import queryAst._

  val Type = ScalarType[_Any](
    name = "_Any",
    coerceOutput = {
      case (_Any(output), _) => output.toString
    },
    coerceUserInput = {
      case _ => Left(AnyCoercionViolation)
    },
    coerceInput = {
      case obj: ObjectValue =>
        Right(_Any(convert[Value, Any @@ ScalaInput](obj).asInstanceOf[Map[String, Any]]))
      case _ => Left(AnyCoercionViolation)
    })
}