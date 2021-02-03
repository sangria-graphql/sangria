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

           type Query {
             entities(representation: _Any!): _Any
           }

            union _Entity = State

            type State {
             id: Int!
             value: String!
           }
         """

      val federationSupport =
        new FederationSupport(sangria.marshalling.json4s.jackson.Json4sJacksonInputUnmarshaller)

      val schema: Schema[Any, Any] = Schema.buildFromAst(
        AST,
        AstSchemaBuilder.resolverBased[Any](
          FieldResolver.map(
            "Query" -> Map(
              "entities" -> (ctx => ctx.withArgs(federationSupport.representationArg)(arg => arg))
            )
          ),
          AdditionalTypes(federationSupport._Any.Type)
        )
      )

      val Success(query) = QueryParser.parse("""
        query FetchState($representation: _Any!) {
          entities(representation: $representation)
        }
        """)

      val args: org.json4s.JValue = org.json4s.jackson.JsonMethods
        .parse(""" { "representation": { "__typename": "State", "id": 1 } } """)

      val result = Executor
        .execute(schema = schema, queryAst = query, variables = args)(
          executionContext = scala.concurrent.ExecutionContext.global,
          marshaller = sangria.marshalling.queryAst.queryAstResultMarshaller,
          um = federationSupport.inputMarshallerForFederation,
          scheme = ExecutionScheme.Default
        )
        .await

      result.renderPretty should be("""{
        |  data: {
        |    entities: "{\"__typename\":\"State\",\"id\":1}"
        |  }
        |}""".stripMargin)
    }
  }
}

object FederationSpec {

  class FederationSupport[Node](default: InputUnmarshaller[Node]) {

    trait NodeObject {
      def render: String
    }
    object NodeObject {
      def apply(node: Node): NodeObject = new NodeObject {
        override def render: String = default.render(node)
      }
    }

    case class _Any(fields: NodeObject)

    object _Any {

      import sangria.validation.ValueCoercionViolation

      case object AnyCoercionViolation extends ValueCoercionViolation("_Any value expected")

      import sangria.schema.ScalarType

      val Type: ScalarType[_Any] = ScalarType[_Any](
        name = "_Any",
        coerceOutput = { case (_Any(output), _) =>
          output.render
        },
        coerceUserInput = {
          case n: NodeObject => Right(_Any(n))
          case _ => Left(AnyCoercionViolation)
        },
        coerceInput = { _ => Left(AnyCoercionViolation) }
      )
    }

    val representationArg: Argument[_Any] = Argument("representation", _Any.Type)

    val inputMarshallerForFederation: InputUnmarshaller[Node] =
      new InputUnmarshaller[Node] {
        override def getRootMapValue(node: Node, key: String): Option[Node] =
          default.getRootMapValue(node, key)
        override def isMapNode(node: Node): Boolean = default.isMapNode(node)
        override def getMapValue(node: Node, key: String): Option[Node] =
          default.getMapValue(node, key)
        override def getMapKeys(node: Node): Traversable[String] = default.getMapKeys(node)
        override def isListNode(node: Node): Boolean = default.isListNode(node)
        override def getListValue(node: Node): Seq[Node] = default.getListValue(node)
        override def isDefined(node: Node): Boolean = default.isDefined(node)
        override def isEnumNode(node: Node): Boolean = default.isEnumNode(node)
        override def isVariableNode(node: Node): Boolean = default.isVariableNode(node)
        override def getScalaScalarValue(node: Node): Any =
          default.getScalaScalarValue(node)
        override def getVariableName(node: Node): String = default.getVariableName(node)
        override def render(node: Node): String = default.render(node)

        override def isScalarNode(node: Node): Boolean =
          default.isMapNode(node) || default.isScalarNode(node)

        override def getScalarValue(node: Node): Any =
          if (default.isMapNode(node)) NodeObject(node)
          else default.getScalarValue(node)
      }
  }
}
