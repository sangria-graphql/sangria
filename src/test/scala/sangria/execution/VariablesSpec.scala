package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.{Pos, GraphQlSupport, AwaitSupport}
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
      Field("fieldWithNullableStringInput", OptionType(StringType),
        arguments = Argument("input", OptionInputType(StringType)) :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, OptionInputType(StringType), SJM))),
      Field("fieldWithNonNullableStringInput", OptionType(StringType),
        arguments = Argument("input", StringType) :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, StringType, SJM))),
      Field("fieldWithDefaultArgumentValue", OptionType(StringType),
        arguments = Argument("input", OptionInputType(StringType), defaultValue = "Hello World") :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, OptionInputType(StringType), SJM))),
      Field("list", OptionType(StringType),
        arguments = Argument("input", OptionInputType(ListInputType(OptionInputType(StringType)))) :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, OptionInputType(ListInputType(OptionInputType(StringType))), SJM))),
      Field("nnList", OptionType(StringType),
        arguments = Argument("input", ListInputType(OptionInputType(StringType))) :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, ListInputType(OptionInputType(StringType)), SJM))),
      Field("listNN", OptionType(StringType),
        arguments = Argument("input", OptionInputType(ListInputType(StringType))) :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, OptionInputType(ListInputType(StringType)), SJM))),
      Field("nnListNN", OptionType(StringType),
        arguments = Argument("input", ListInputType(StringType)) :: Nil,
        resolve = ctx => ctx.argOpt[Any]("input") map (ctx.renderInputValueCompact(_, ListInputType(StringType), SJM)))
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
          List("""Value '["foo", "bar", "baz"]' of wrong type was provided to the field of type 'TestInputObject!' at path 'input'.""" -> Some(Pos(3, 43)))
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

        "uses default value when not provided" in check(
          (),
          """
            query q($input: TestInputObject = {a: "foo", b: ["bar"], c: "baz"}) {
              fieldWithObjectInput(input: $input)
            }
          """,
          Map("data" -> Map(
            "fieldWithObjectInput" -> """{"a":"foo","b":["bar"],"c":"baz"}"""
          ))
        )

        "properly coerces single value to array (scala input)" in {
          val args = Map("input" -> Map("a" -> "foo", "b" -> "bar", "c" -> "baz"))

          Executor(schema).execute(testQuery, arguments = Some(args)).await should be (Map("data" -> Map(
            "fieldWithObjectInput" -> """{"a":"foo","b":["bar"],"c":"baz"}"""
          )))
        }

        "properly coerces single value to array (json input)" in {
          val args = """{"input": {"a": "foo", "b": "bar", "c": "baz"}}""".parseJson

          Executor(schema).execute(testQuery, arguments = Some(args)).await should be (Map("data" -> Map(
            "fieldWithObjectInput" -> """{"a":"foo","b":["bar"],"c":"baz"}"""
          )))
        }

        def assertErrorResult[T: InputUnmarshaller](args: T, expectedError: String) = {
          val result = Executor(schema).execute(testQuery, arguments = Some(args)).await.asInstanceOf[Map[String, AnyRef]]

          result("data") should equal (null)

          val errors = result("errors").asInstanceOf[List[Map[String, Any]]]

          errors should have size 1

          withClue("Wrong error message: " + errors(0)("message")) {
            errors(0)("message").asInstanceOf[String] contains expectedError should be (true)
          }
        }

        "errors on null for nested non-null" in assertErrorResult(
          """{"input": {"a": "foo", "b": "bar", "c": null}}""".parseJson,
          """Variable $input expected value of type TestInputObject but got: {"a":"foo","b":"bar","c":null}""")

        "errors on incorrect type" in  assertErrorResult(
          """{"input": "foo bar"}""".parseJson,
          """Variable $input expected value of type TestInputObject but got: "foo bar"""")

        "errors on omission of nested non-null" in  assertErrorResult(
          """{"input": {"a": "foo", "b": "bar"}}""".parseJson,
          """Variable $input expected value of type TestInputObject but got: {"a":"foo","b":"bar"}""")

        "errors on addition of unknown input field" in  assertErrorResult(
          """{"input": {"a": "foo", "b": "bar", "c": "baz", "d": "dog"}}""".parseJson,
          """Variable $input expected value of type TestInputObject but got: {"a":"foo","b":"bar","c":"baz","d":"dog"}""")
      }
    }

    "Handles nullable scalars" when {
      "allows nullable inputs to be omitted" in check(
        (),
        """
          {
            fieldWithNullableStringInput
          }
        """,
        Map("data" -> Map(
          "fieldWithNullableStringInput" -> null
        ))
      )

      "allows nullable inputs to be omitted in a variable" in check(
        (),
        """
          query SetsNullable($value: String) {
            fieldWithNullableStringInput(input: $value)
          }
        """,
        Map("data" -> Map(
          "fieldWithNullableStringInput" -> null
        ))
      )

      "allows nullable inputs to be omitted in an unlisted variable" in check(
        (),
        """
          query SetsNullable {
            fieldWithNullableStringInput(input: $value)
          }
        """,
        Map("data" -> Map(
          "fieldWithNullableStringInput" -> null
        ))
      )

      "allows nullable inputs to be set to null in a variable" in {
        val args = Map("value" -> null)

        val Success(query) = QueryParser.parse(
          """
            query SetsNullable($value: String) {
              fieldWithNullableStringInput(input: $value)
            }
          """)

        Executor(schema).execute(query, arguments = Some(args)).await should be (Map("data" -> Map(
          "fieldWithNullableStringInput" -> null
        )))
      }

      "allows nullable inputs to be set to a value in a variable" in {
        val args = Map("value" -> "a")

        val Success(query) = QueryParser.parse(
          """
            query SetsNullable($value: String) {
              fieldWithNullableStringInput(input: $value)
            }
          """)

        Executor(schema).execute(query, arguments = Some(args)).await should be (Map("data" -> Map(
          "fieldWithNullableStringInput" -> "\"a\""
        )))
      }

      "allows nullable inputs to be set to a value directly" in check(
        (),
        """
          query SetsNullable {
            fieldWithNullableStringInput(input: "a")
          }
        """,
        Map("data" -> Map(
          "fieldWithNullableStringInput" -> "\"a\""
        ))
      )
    }

    "Handles non-nullable scalars" when {
      "does not allow non-nullable inputs to be omitted in a variable" in  checkContainsErrors(
        (),
        """
          query SetsNonNullable($value: String!) {
            fieldWithNonNullableStringInput(input: $value)
          }
        """,
        null,
        List("""Variable $value expected value of type String! but value is undefined.""" -> Some(Pos(2, 33)))
      )

      "does not allow non-nullable inputs to be set to null in a variable" in  checkContainsErrors(
        (),
        """
          query SetsNonNullable($value: String!) {
            fieldWithNonNullableStringInput(input: $value)
          }
        """,
        null,
        List("""Variable $value expected value of type String! but got: null.""" -> Some(Pos(2, 33))),
        Some("""{"value": null}""".parseJson)
      )

      "allows non-nullable inputs to be set to a value in a variable" in  check(
        (),
        """
          query SetsNonNullable($value: String!) {
            fieldWithNonNullableStringInput(input: $value)
          }
        """,
        Map("data" -> Map(
          "fieldWithNonNullableStringInput" -> "\"a\""
        )),
        Some("""{"value": "a"}""".parseJson)
      )

      "allows non-nullable inputs to be set to a value directly" in  check(
        (),
        """
          {
            fieldWithNonNullableStringInput(input: "a")
          }
        """,
        Map("data" -> Map(
          "fieldWithNonNullableStringInput" -> "\"a\""
        ))
      )

      "passes along null for non-nullable inputs if explcitly set in the query" in  checkContainsErrors(
        (),
        """
          {
            fieldWithNonNullableStringInput
          }
        """,
        Map("fieldWithNonNullableStringInput" -> null),
        List("""Null value was provided for the NotNull Type 'String!' at path 'input'.""" -> None)
      )
    }

    "Handles lists and nullability" when {
      "allows lists to be null" in  check(
        (),
        """
          query q($input: [String]) {
            list(input: $input)
          }
        """,
        Map("data" -> Map("list" -> null)),
        Some("""{"input": null}""".parseJson)
      )

      "allows lists to contain values" in  check(
        (),
        """
          query q($input: [String]) {
            list(input: $input)
          }
        """,
        Map("data" -> Map("list" -> "[\"A\"]")),
        Some("""{"input": ["A"]}""".parseJson)
      )

      "allows lists to contain null" in  check(
        (),
        """
          query q($input: [String]) {
            list(input: $input)
          }
        """,
        Map("data" -> Map("list" -> "[\"A\",null,\"B\"]")),
        Some("""{"input": ["A", null, "B"]}""".parseJson)
      )

      "does not allow non-null lists to be null" in  checkContainsErrors(
        (),
        """
          query q($input: [String]!) {
            nnList(input: $input)
          }
        """,
        null,
        List("""Variable $input expected value of type [String]! but got: null.""" -> Some(Pos(2, 19))),
        Some("""{"input": null}""".parseJson)
      )

      "allows non-null lists to contain values" in  check(
        (),
        """
          query q($input: [String]!) {
            nnList(input: $input)
          }
        """,
        Map("data" -> Map("nnList" -> "[\"A\"]")),
        Some("""{"input": ["A"]}""".parseJson)
      )

      "allows non-null lists to contain null" in  check(
        (),
        """
          query q($input: [String]!) {
            nnList(input: $input)
          }
        """,
        Map("data" -> Map("nnList" -> "[\"A\",null,\"B\"]")),
        Some("""{"input": ["A",null,"B"]}""".parseJson)
      )

      "allows lists of non-nulls to be null" in  check(
        (),
        """
          query q($input: [String!]) {
            listNN(input: $input)
          }
        """,
        Map("data" -> Map("listNN" -> null)),
        Some("""{"input": null}""".parseJson)
      )

      "allows lists of non-nulls to contain values" in  check(
        (),
        """
          query q($input: [String!]) {
            listNN(input: $input)
          }
        """,
        Map("data" -> Map("listNN" -> "[\"A\"]")),
        Some("""{"input": ["A"]}""".parseJson)
      )

      "does not allow lists of non-nulls to contain null" in  checkContainsErrors(
        (),
        """
          query q($input: [String!]) {
            listNN(input: $input)
          }
        """,
        null,
        List("""Variable $input expected value of type [String!] but got: ["A",null,"B"].""" -> Some(Pos(2, 19))),
        Some("""{"input": ["A",null,"B"]}""".parseJson)
      )

      "does not allow non-null lists of non-nulls to be null" in  checkContainsErrors(
        (),
        """
          query q($input: [String!]) {
            nnListNN(input: $input)
          }
        """,
        Map("nnListNN" -> null),
        List("""Null value was provided for the NotNull Type '[String!]!' at path 'input'.""" -> None),
        Some("""{"input": null}""".parseJson)
      )

      "allows non-null lists of non-nulls to contain values" in  check(
        (),
        """
          query q($input: [String!]!) {
            nnListNN(input: $input)
          }
        """,
        Map("data" -> Map("nnListNN" -> "[\"A\"]")),
        Some("""{"input": ["A"]}""".parseJson)
      )

      "does not allow non-null lists of non-nulls to contain null" in  checkContainsErrors(
        (),
        """
          query q($input: [String!]!) {
            nnListNN(input: $input)
          }
        """,
        null,
        List("""Variable $input expected value of type [String!]! but got: ["A",null,"B"].""" -> Some(Pos(2, 19))),
        Some("""{"input": ["A",null,"B"]}""".parseJson)
      )

      "does not allow invalid types to be used as values" in  checkContainsErrors(
        (),
        """
          query q($input: TestType!) {
            fieldWithObjectInput(input: $input)
          }
        """,
        null,
        List("""Variable 'TestType!' expected value of type '$input' which cannot be used as an input type.""" -> Some(Pos(2, 19))),
        Some("""{"input": ["A", "B"]}""".parseJson)
      )

      "does not allow unknown types to be used as values" in  checkContainsErrors(
        (),
        """
          query q($input: UnknownType!) {
            fieldWithObjectInput(input: $input)
          }
        """,
        null,
        List("""Variable 'UnknownType!' expected value of type '$input' which cannot be used as an input type.""" -> Some(Pos(2, 19))),
        Some("""{"input": "whoknows"}""".parseJson)
      )
    }

    "Execute: Uses argument default values" when {
      "when no argument provided" in  check(
        (),
        """
          {
            fieldWithDefaultArgumentValue
          }
        """,
        Map("data" -> Map("fieldWithDefaultArgumentValue" -> "\"Hello World\""))
      )

      "when nullable variable provided" in  check(
        (),
        """
          query optionalVariable($optional: String) {
            fieldWithDefaultArgumentValue(input: $optional)
          }
        """,
        Map("data" -> Map("fieldWithDefaultArgumentValue" -> "\"Hello World\""))
      )

      "when argument provided cannot be coerced" in  check(
        (),
        """
          {
            fieldWithDefaultArgumentValue(input: WRONG_TYPE)
          }
        """,
        Map("data" -> Map("fieldWithDefaultArgumentValue" -> "\"Hello World\""))
      )
    }
  }
}
