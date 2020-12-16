package sangria.marshalling

import java.util.concurrent.Executor

import sangria.marshalling.sprayJson.SprayJsonInputUnmarshaller
import sangria.parser.QueryParser
import sangria.schema.{
  Argument,
  EnumType,
  EnumValue,
  Field,
  InputField,
  InputObjectType,
  ObjectType
}
import spray.json.{JsObject, JsString, JsValue, JsonReader}

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext}
import scala.util.Success
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class EnumInputTypeSpec extends AnyWordSpec with Matchers {
  implicit val inputUnmarshaller: SprayJsonInputUnmarshaller.type = SprayJsonInputUnmarshaller

  implicit def sprayJsonFromInput[T <: JsValue]: FromInput[T] = sprayJson.sprayJsonFromInput[T]

  implicit def sprayJsonReaderFromInput[T: JsonReader]: FromInput[T] =
    sprayJson.sprayJsonReaderFromInput[T]

  private val enumWithCustomValueType = EnumType[String](
    "CustomEnum",
    Some("Some custom enum values"),
    List(
      EnumValue("TOP", value = "TOP_VALUE"),
      EnumValue("OTHER", value = "OTHER_VALUE")
    )
  )

  private val complexInputType = InputObjectType[JsObject](
    "customInput",
    fields = List(InputField("enumValue", enumWithCustomValueType)))
  private val complexArgument = Argument("complexArgument", complexInputType)
  private val enumArgument = Argument("simpleArgument", enumWithCustomValueType)

  private val valueFromComplexField: Field[Unit, JsValue] =
    Field(
      "valueFromComplex",
      sangria.schema.StringType,
      arguments = List(complexArgument),
      resolve = { ctx =>
        ctx.arg(complexArgument).getFields("enumValue").toString()
      }
    )

  private val valueFromEnumField: Field[Unit, JsValue] =
    Field(
      "valueFromEnum",
      sangria.schema.StringType,
      arguments = List(enumArgument),
      resolve = { ctx =>
        JsString(ctx.arg(enumArgument)).value
      }
    )

  val queryType: ObjectType[Unit, JsValue] = ObjectType(
    "query",
    sangria.schema.fields(valueFromComplexField, valueFromEnumField)
  )

  val schema = new sangria.schema.Schema(queryType)

  implicit val synchronousExecutionContext: ExecutionContext =
    ExecutionContext.fromExecutor(new Executor {
      override def execute(command: Runnable): Unit =
        command.run()
    })

  private def runQuery(query: String) = {
    val Success(queryAst) = QueryParser.parse(query)

    val fut = sangria.execution.Executor
      .execute(
        schema,
        queryAst,
        userContext = (),
        root = JsObject.empty
      )

    Await.result(fut, 2.seconds)
  }

  // Ignored for the moment being. Related discussion: https://github.com/sangria-graphql/sangria/issues/300
  "correctly unmarshals an enum in a complex input type" ignore {
    val query =
      """
        | query GetTheComplexThing {
        |   valueFromComplex(complexArgument: { enumValue: TOP })
        | }
      """.stripMargin

    runQuery(query) should be(Map("data" -> Map("valueFromComplex" -> "TOP_VALUE")))
  }

  "correctly unmarshals an enum passed as an argument" in {
    val query =
      """
        | query GetTheSimpleThing {
        |   valueFromEnum(simpleArgument: TOP)
        | }
      """.stripMargin

    runQuery(query) should be(Map("data" -> Map("valueFromEnum" -> "TOP_VALUE")))
  }
}
