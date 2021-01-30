package sangria.marshalling

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.ast
import sangria.execution.{ExecutionScheme, Executor}
import sangria.util.FutureResultSupport
import sangria.macros._
import sangria.parser.QueryParser
import sangria.schema._

import scala.util.Success

class FederationQuerySupportSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  import FederationQuerySupportSpec._

  "Unmarshaller" should {
    "accept an object value as a scalar parameter" in {
      val AST =
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

      implicit val inputUnmarshaller: InputUnmarshaller[sangria.ast.Value] =
        new QueryAstInputUnmarshaller {
          override def isScalarNode(node: ast.Value): Boolean =
            node.isInstanceOf[ast.ObjectValue] || super.isScalarNode(node)
        }

      import sangria.marshalling.scalaMarshalling._
      val AnyType = _Any.Type
      val representationArg = Argument("representation", AnyType)

      val schema: Schema[Any, Any] = Schema.buildFromAst(
        AST,
        AstSchemaBuilder.resolverBased[Any](
          FieldResolver.map(
            "Query" -> Map(
              "entities" -> (ctx => ctx.withArgs(representationArg)(arg => arg))
            )
          ),
          AdditionalTypes(AnyType))
      )

      val Success(query) = QueryParser.parse("""
        query FetchState {
          entities(representation: { __typename: "State", id: 1 })
        }
        """)

      val result = Executor
        .execute(schema, query)(
          executionContext = scala.concurrent.ExecutionContext.global,
          marshaller = sangria.marshalling.queryAst.queryAstResultMarshaller,
          um = scalaInputUnmarshaller,
          astUm = inputUnmarshaller,
          scheme = ExecutionScheme.Default
        )
        .await

      result.renderPretty should be("""{
        |  data: {
        |    entities: "(__typename -> State, id -> 1)"
        |  }
        |}""".stripMargin)
    }
  }
}

object FederationQuerySupportSpec {
  case class _Any(obj: ast.ObjectValue)

  object _Any {

    import sangria.validation.ValueCoercionViolation

    case object AnyCoercionViolation extends ValueCoercionViolation("_Any value expected")

    def toString(v: ast.Value): String =
      v match {
        case obj: ast.ObjectValue =>
          obj.fieldsByName.map { case (k, v) => s"$k -> ${toString(v)}" }.mkString("(", ", ", ")")
        case s: ast.StringValue => s.value
        case s: ast.BigIntValue => s.value.toString()
        case other => other.toString
      }

    val Type = ScalarType[_Any](
      name = "_Any",
      coerceOutput = { case (_Any(output), _) =>
        toString(output)
      },
      coerceUserInput = { _ =>
        Left(AnyCoercionViolation)
      },
      coerceInput = {
        case obj: ast.ObjectValue => Right(_Any(obj))
        case _ => Left(AnyCoercionViolation)
      }
    )
  }
}
