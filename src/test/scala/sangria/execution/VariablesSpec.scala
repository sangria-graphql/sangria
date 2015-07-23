package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.{GraphQlSupport, AwaitSupport}
import sangria.integration.SprayJsonSupport.{SprayJsonInputUnmarshaller}
import spray.json._
import DefaultJsonProtocol._

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class VariablesSpec extends WordSpec with Matchers with AwaitSupport with GraphQlSupport {
  val TestInputObject = InputObjectType("TestInputObject", List(
    InputField("a", OptionInputType(StringType)),
    InputField("b", OptionInputType(ListInputType(OptionInputType(StringType)))),
    InputField("c", StringType)))

  val TestType = ObjectType("TestType", {
    import sangria.integration.SprayJsonSupport.{SprayJsonResultMarshaller => SJM}

    List[Field[Unit, Unit]](
      Field("fieldWithObjectInput", OptionType(StringType),
        arguments = Argument("input", OptionInputType(TestInputObject)) :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, OptionInputType(TestInputObject), SJM))),
      Field("fieldWithObjectInput", OptionType(StringType),
        arguments = Argument("input", OptionInputType(StringType)) :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, OptionInputType(StringType), SJM)))
    )
  })

  def schema = Schema(TestType)

  "Execute: Handles inputs" when {
    "Handles objects and nullability" when {
      "using inline structs" when {
        "executes with complex input" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {a: "foo", b: ["bar"], c: "baz"})
            }
          """,
          Map("data" -> Map(
            "fieldWithObjectInput" -> """{"a":"foo","b":["bar"],"c":"baz"}"""
          ))
        )

        "properly coerces single value to array" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {a: "foo", b: "bar", c: "baz"})
            }
          """,
          Map("data" -> Map(
            "fieldWithObjectInput" -> """{"a":"foo","b":["bar"],"c":"baz"}"""
          ))
        )

        "does not use incorrect value" in checkContainsErrors(
          (),
          """
            {
              fieldWithObjectInput(input: ["foo", "bar", "baz"])
            }
          """,
          Map("fieldWithObjectInput" -> null),
          List("""Value '["foo", "bar", "baz"]' of wrong type was provided to the field of type 'TestInputObject!' at path 'input'.""")
        )
      }

      "using variables" when {
        val Success(testQuery) =
          QueryParser.parse("""
            query q($input: TestInputObject) {
              fieldWithObjectInput(input: $input)
            }
          """)

        "executes with complex input (scala input)" in {
          val args = Map("input" -> Map("a" -> "foo", "b" -> List("bar"), "c" -> "baz"))

          Executor(schema).execute(testQuery, arguments = Some(args)).await should be (Map("data" -> Map(
            "fieldWithObjectInput" -> """{"a":"foo","b":["bar"],"c":"baz"}"""
          )))
        }

        "executes with complex input (json input)" in {
          val args = """{"input": {"a": "foo", "b": ["bar"], "c": "baz"}}""".parseJson

          Executor(schema).execute(testQuery, arguments = Some(args)).await should be (Map("data" -> Map(
            "fieldWithObjectInput" -> """{"a":"foo","b":["bar"],"c":"baz"}"""
          )))
        }
      }
    }
  }
}
