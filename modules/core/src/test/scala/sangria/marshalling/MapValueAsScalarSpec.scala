package sangria.marshalling

import scala.util.Success

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import org.json4s._
import org.json4s.JsonAST.JNumber
import org.json4s.jackson.JsonMethods.{compact, parse, render => jsonRender}
import sangria.execution.Executor
import sangria.macros._
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer
import sangria.util.FutureResultSupport

class MapValueAsScalarSpec extends AnyWordSpec with Matchers with FutureResultSupport {

  import MapValueAsScalarSpec._

  import sangria.schema._

  "Unmarshaller" should {
    "accept a map value as a scalar" in {
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
        query FetchState($representation: _Any!) {
          entities(representation: $representation)
        }
        """)

      val args: JValue = parse(""" { "representation": { "__typename": "State", "id": 1 } } """)

      import scala.concurrent.ExecutionContext.Implicits.global
      import sangria.marshalling.queryAst.queryAstResultMarshaller

      val result = Executor
        .execute(
          schema,
          query,
          variables = args)
        .await

      QueryRenderer.render(result, QueryRenderer.PrettyInput) should be("""{
        |  data: {
        |    entities: "{\"__typename\":\"State\",\"id\":1}"
        |  }
        |}""".stripMargin)
    }
  }
}

object MapValueAsScalarSpec {

  implicit val um = new InputUnmarshaller[JValue] {
    def getRootMapValue(node: JValue, key: String) =
      node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)

    def isMapNode(node: JValue) = node.isInstanceOf[JObject]
    def getMapValue(node: JValue, key: String) =
      node.asInstanceOf[JObject].obj.find(_._1 == key).map(_._2)
    def getMapKeys(node: JValue) = node.asInstanceOf[JObject].obj map (_._1)

    def isListNode(node: JValue) = node.isInstanceOf[JArray]
    def getListValue(node: JValue) = node.asInstanceOf[JArray].arr

    def isDefined(node: JValue) = node != JNull && node != JNothing
    def getScalarValue(node: JValue) = node match {
      case JBool(b) => b
      case JInt(i) => i
      case JDouble(d) => d
      case JLong(l) => l
      case JDecimal(d) => d
      case JString(s) => s
      case obj: JObject => obj
      case _ => throw new IllegalStateException(s"$node is not a scalar value")
    }

    import sangria.marshalling.MarshallingUtil._
    import sangria.marshalling.scalaMarshalling._
    import sangria.util.tag._

    def getScalaScalarValue(node: JValue) = node match {
      case obj: JObject => convert[JValue, Any @@ ScalaInput](node)(this, ScalaMarshallerForType)
      case _ => getScalarValue(node)
    }

    def isEnumNode(node: JValue) = node.isInstanceOf[JString]

    def isScalarNode(node: JValue) = node match {
      case _: JBool | _: JNumber | _: JString | _:JObject => true
      case _ => false
    }

    def isVariableNode(node: JValue) = false
    def getVariableName(node: JValue) = throw new IllegalArgumentException("variables are not supported")

    def render(node: JValue) = compact(jsonRender(node))
  }

  case class _Any(fields: JObject)

  object _Any {

    import sangria.validation.ValueCoercionViolation

    case object AnyCoercionViolation extends
      ValueCoercionViolation("_Any value expected")

    import sangria.schema.ScalarType

    val Type = ScalarType[_Any](
      name = "_Any",
      coerceOutput = {
        case (_Any(output), _) => compact(jsonRender(output))
      },
      coerceUserInput = {
        case obj: JObject => Right(_Any(obj))
        case _ => Left(AnyCoercionViolation)
      },
      coerceInput = {
        case _ => Left(AnyCoercionViolation)
      })
  }
}
