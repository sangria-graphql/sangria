package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.marshalling.InputUnmarshaller
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.{Pos, GraphQlSupport}
import sangria.marshalling.sprayJson.SprayJsonInputUnmarshaller

import InputUnmarshaller.mapVars

import spray.json._
import DefaultJsonProtocol._

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class VariablesSpec extends WordSpec with Matchers with GraphQlSupport {
  val TestInputObject = InputObjectType("TestInputObject", List(
    InputField("a", OptionInputType(StringType)),
    InputField("b", OptionInputType(ListInputType(OptionInputType(StringType)))),
    InputField("c", StringType),
    InputField("d", OptionInputType(ListInputType(StringType)))))

  val TestType = ObjectType("TestType", {
    fields[Unit, Unit](
      Field("fieldWithObjectInput", OptionType(StringType),
        arguments = Argument("input", OptionInputType(TestInputObject)) :: Nil,
        resolve = ctx ⇒ ctx.argOpt[Any]("input") map (DefaultValueRenderer.renderCoercedInputValueCompact(_, OptionInputType(TestInputObject)))),
      Field("fieldWithNullableStringInput", OptionType(StringType),
        arguments = Argument("input", OptionInputType(StringType)) :: Nil,
        resolve = ctx ⇒ ctx.argOpt[Any]("input") map (DefaultValueRenderer.renderCoercedInputValueCompact(_, OptionInputType(StringType)))),
      Field("fieldWithNonNullableStringInput", OptionType(StringType),
        arguments = Argument("input", StringType) :: Nil,
        resolve = ctx ⇒ DefaultValueRenderer.renderCoercedInputValueCompact(ctx.arg[Any]("input"), StringType)),
      Field("fieldWithDefaultArgumentValue", OptionType(StringType),
        arguments = Argument("input", OptionInputType(StringType), defaultValue = "Hello World") :: Nil,
        resolve = ctx ⇒ DefaultValueRenderer.renderCoercedInputValueCompact(ctx.arg[Any]("input"), OptionInputType(StringType))),
      Field("list", OptionType(StringType),
        arguments = Argument("input", OptionInputType(ListInputType(OptionInputType(StringType)))) :: Nil,
        resolve = ctx ⇒ ctx.argOpt[Any]("input") map (DefaultValueRenderer.renderCoercedInputValueCompact(_, OptionInputType(ListInputType(OptionInputType(StringType)))))),
      Field("nnList", OptionType(StringType),
        arguments = Argument("input", ListInputType(OptionInputType(StringType))) :: Nil,
        resolve = ctx ⇒ DefaultValueRenderer.renderCoercedInputValueCompact(ctx.arg[Any]("input"), ListInputType(OptionInputType(StringType)))),
      Field("listNN", OptionType(StringType),
        arguments = Argument("input", OptionInputType(ListInputType(StringType))) :: Nil,
        resolve = ctx ⇒ ctx.argOpt[Any]("input") map (DefaultValueRenderer.renderCoercedInputValueCompact(_, OptionInputType(ListInputType(StringType))))),
      Field("nnListNN", OptionType(StringType),
        arguments = Argument("input", ListInputType(StringType)) :: Nil,
        resolve = ctx ⇒ DefaultValueRenderer.renderCoercedInputValueCompact(ctx.arg[Any]("input"), ListInputType(StringType)))
    )
  })

  def schema = Schema(TestType)

  "Execute: Handles inputs" when {
    "Handles objects and nullability" when {
      "using inline structs" when {
        "executes with null input" in check(
          (),
          """
            {
              fieldWithObjectInput(input: null)
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → null
          ))
        )

        "executes with complex input" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {a: "foo", b: ["bar"], c: "baz"})
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → """{a:"foo",b:["bar"],c:"baz"}"""
          ))
        )

        "executes with complex input containing nulls in object fields" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {a: null, b: ["bar"], c: "baz"})
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → """{a:null,b:["bar"],c:"baz"}"""
          ))
        )

        "executes with complex input containing nulls in list values inside of complex objects" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {a: "foo", b: ["bar", null, "test"], c: "baz"})
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → """{a:"foo",b:["bar",null,"test"],c:"baz"}"""
          ))
        )

        "executes with complex input containing nulls in list values" in check(
          (),
          """
            {
              nnList(input: ["a1", null, "b1"])
            }
          """,
          Map("data" → Map(
            "nnList" → """["a1",null,"b1"]"""
          ))
        )

        "does not allow null literals in not-null lists" in checkContainsErrors(
          (),
          """
            {
              nnListNN(input: ["a1", null, "b1"])
            }
          """,
          null,
          List("""Argument 'input' expected type '[String!]!' but got: ["a1", null, "b1"]. Reason: [at index #1] String value expected""" → List(Pos(3, 31)))
        )

        "does not allow null literals in not-null fields in complex objects" in checkContainsErrors(
          (),
          """
            {
              fieldWithObjectInput(input: {a: "foo", c: null})
            }
          """,
          null,
          List("""Argument 'input' expected type 'TestInputObject' but got: {a: "foo", c: null}. Reason: [in field 'c'] String value expected""" → List(Pos(3, 43), Pos(3, 54)))
        )

        "does not allow null literals in not-null arguments" in checkContainsErrors(
          (),
          """
            {
              nnListNN(input: null)
            }
          """,
          null,
          List("""Argument 'input' expected type '[String!]!' but got: null. Reason: [at index #0] String value expected""" → List(Pos(3, 31)))
        )

        "does not allow null literals in not-null lists inside of complex objects" in checkContainsErrors(
          (),
          """
            {
              fieldWithObjectInput(input: {a: "foo", c: "baz", d: ["aa", null]})
            }
          """,
          null,
          List("""Argument 'input' expected type 'TestInputObject' but got: {a: "foo", c: "baz", d: ["aa", null]}. Reason: [in field 'd'] [at index #1] String value expected""" → List(Pos(3, 43), Pos(3, 67)))
        )

        "executes with complex input containing undefined object fields" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {b: ["bar"], c: "baz"})
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → """{b:["bar"],c:"baz"}"""))
        )

        "properly coerces single value to array" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {a: "foo", b: "bar", c: "baz"})
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → """{a:"foo",b:["bar"],c:"baz"}"""))
        )

        "properly parses null value to null" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {a: null, b: null, c: "C", d: null})
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → """{a:null,b:null,c:"C",d:null}"""))
        )

        "properly parses null value in list" in check(
          (),
          """
            {
              fieldWithObjectInput(input: {b: ["A",null,"C"], c: "C"})
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → """{b:["A",null,"C"],c:"C"}"""))
        )

        "does not use incorrect value" in checkContainsErrors(
          (),
          """
            {
              fieldWithObjectInput(input: ["foo", "bar", "baz"])
            }
          """,
          Map("fieldWithObjectInput" → null),
          List("""Value '["foo","bar","baz"]' of wrong type was provided to the field of type 'TestInputObject!' at path 'input'.""" → List(Pos(3, 43))),
          validateQuery = false
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
          val args = Map("input" → Map("a" → "foo", "b" → List("bar"), "c" → "baz"))

          Executor.execute(schema, testQuery, variables = mapVars(args)).await should be (Map("data" → Map(
            "fieldWithObjectInput" → """{a:"foo",b:["bar"],c:"baz"}"""
          )))
        }

        "executes with complex input (json input)" in {
          val args = """{"input": {"a": "foo", "b": ["bar"], "c": "baz"}}""".parseJson

          Executor.execute(schema, testQuery, variables = args).await should be (Map("data" → Map(
            "fieldWithObjectInput" → """{a:"foo",b:["bar"],c:"baz"}"""
          )))
        }

        "uses default value when not provided" in check(
          (),
          """
            query q($input: TestInputObject = {a: "foo", b: ["bar"], c: "baz"}) {
              fieldWithObjectInput(input: $input)
            }
          """,
          Map("data" → Map(
            "fieldWithObjectInput" → """{a:"foo",b:["bar"],c:"baz"}"""
          ))
        )

        "properly coerces single value to array (scala input)" in {
          val args = Map("input" → Map("a" → "foo", "b" → "bar", "c" → "baz"))

          Executor.execute(schema, testQuery, variables = mapVars(args)).await should be (Map("data" → Map(
            "fieldWithObjectInput" → """{a:"foo",b:["bar"],c:"baz"}"""
          )))
        }

        "properly coerces single value to array (json input)" in {
          val args = """{"input": {"a": "foo", "b": "bar", "c": "baz"}}""".parseJson

          Executor.execute(schema, testQuery, variables = args).await should be (Map("data" → Map(
            "fieldWithObjectInput" → """{a:"foo",b:["bar"],c:"baz"}"""
          )))
        }

        def assertErrorResult[T: InputUnmarshaller](args: T, expectedError: String) = {
          val result = Executor.execute(schema, testQuery, variables = args).awaitAndRecoverQueryAnalysisScala.asInstanceOf[Map[String, AnyRef]]

          result("data") should equal (null)

          val errors = result("errors").asInstanceOf[Seq[Map[String, Any]]]

          errors should have size 1

          withClue("Wrong error message: " + errors(0)("message")) {
            errors(0)("message").asInstanceOf[String] contains expectedError should be (true)
          }
        }

        "errors on null for nested non-null" in assertErrorResult(
          """{"input": {"a": "foo", "b": "bar", "c": null}}""".parseJson,
          """Variable '$input' expected value of type 'TestInputObject' but got: {"a":"foo","b":"bar","c":null}""")

        "errors on incorrect type" in  assertErrorResult(
          """{"input": "foo bar"}""".parseJson,
          """Variable '$input' expected value of type 'TestInputObject' but got: "foo bar"""")

        "errors on omission of nested non-null" in  assertErrorResult(
          """{"input": {"a": "foo", "b": "bar"}}""".parseJson,
          """Variable '$input' expected value of type 'TestInputObject' but got: {"a":"foo","b":"bar"}. Reason: [in field 'c'] Expected non-null value, found null""")

        "errors on addition of unknown input field" in  assertErrorResult(
          """{"input": {"a": "foo", "b": "bar", "c": "baz", "z": "dog"}}""".parseJson,
          """Variable '$input' expected value of type 'TestInputObject' but got: {"a":"foo","b":"bar","c":"baz","z":"dog"}. Reason: Unknown field 'z' is not defined in the input type 'TestInputObject'.""")
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
        Map("data" → Map(
          "fieldWithNullableStringInput" → null
        ))
      )

      "allows nullable inputs to be omitted in a variable" in check(
        (),
        """
          query SetsNullable($value: String) {
            fieldWithNullableStringInput(input: $value)
          }
        """,
        Map("data" → Map(
          "fieldWithNullableStringInput" → null
        ))
      )

      "allows nullable inputs to be omitted in an unlisted variable" in check(
        (),
        """
          query SetsNullable {
            fieldWithNullableStringInput(input: $value)
          }
        """,
        Map("data" → Map(
          "fieldWithNullableStringInput" → null
        )),
        validateQuery = false
      )

      "allows nullable inputs to be set to null in a variable" in {
        val args = mapVars("value" → null)

        val Success(query) = QueryParser.parse(
          """
            query SetsNullable($value: String) {
              fieldWithNullableStringInput(input: $value)
            }
          """)

        Executor.execute(schema, query, variables = args).await should be (Map("data" → Map(
          "fieldWithNullableStringInput" → null
        )))
      }

      "allows nullable inputs to be set to a value in a variable" in {
        val args = mapVars("value" → "a")

        val Success(query) = QueryParser.parse(
          """
            query SetsNullable($value: String) {
              fieldWithNullableStringInput(input: $value)
            }
          """)

        Executor.execute(schema, query, variables = args).await should be (Map("data" → Map(
          "fieldWithNullableStringInput" → "\"a\""
        )))
      }

      "allows nullable inputs to be set to a value directly" in check(
        (),
        """
          query SetsNullable {
            fieldWithNullableStringInput(input: "a")
          }
        """,
        Map("data" → Map(
          "fieldWithNullableStringInput" → "\"a\""
        ))
      )
    }

    "Handles non-nullable scalars" when {
      "allows non-nullable inputs to be omitted given a default" in  check(
        (),
        """
          query SetsNonNullable($value: String = "default") {
           fieldWithNonNullableStringInput(input: $value)
          }
        """,
        Map("data" → Map(
          "fieldWithNonNullableStringInput" → "\"default\""
        ))
      )

      "does not allow non-nullable inputs to be omitted in a variable" in  checkContainsErrors(
        (),
        """
          query SetsNonNullable($value: String!) {
            fieldWithNonNullableStringInput(input: $value)
          }
        """,
        null,
        List("""Variable '$value' expected value of type 'String!' but value is undefined.""" → List(Pos(2, 33)))
      )

      "does not allow non-nullable inputs to be set to null in a variable" in  checkContainsErrors(
        (),
        """
          query SetsNonNullable($value: String!) {
            fieldWithNonNullableStringInput(input: $value)
          }
        """,
        null,
        List("""Variable '$value' expected value of type 'String!' but got: null.""" → List(Pos(2, 33))),
        """{"value": null}""".parseJson
      )

      "allows non-nullable inputs to be set to a value in a variable" in  check(
        (),
        """
          query SetsNonNullable($value: String!) {
            fieldWithNonNullableStringInput(input: $value)
          }
        """,
        Map("data" → Map(
          "fieldWithNonNullableStringInput" → "\"a\""
        )),
        """{"value": "a"}""".parseJson
      )

      "allows non-nullable inputs to be set to a value directly" in  check(
        (),
        """
          {
            fieldWithNonNullableStringInput(input: "a")
          }
        """,
        Map("data" → Map(
          "fieldWithNonNullableStringInput" → "\"a\""
        ))
      )

      "passes along null for non-nullable inputs if explicitly set in the query" in  checkContainsErrors((),
        """
          {
            fieldWithNonNullableStringInput
          }
        """,
        Map("fieldWithNonNullableStringInput" → null),
        List("""Null value was provided for the NotNull Type 'String!' at path 'input'.""" → Nil),
        validateQuery = false)

      // Note: this test would typically fail validation before encountering
      // this execution error, however for queries which previously validated
      // and are being run against a new schema which have introduced a breaking
      // change to make a formerly non-required argument required, this asserts
      // failure before allowing the underlying code to receive a non-null value.
      "reports error for non-provided variables for non-nullable inputs" in  checkContainsErrors((),
        """
          {
            fieldWithNonNullableStringInput(input: $foo)
          }
        """,
        Map("fieldWithNonNullableStringInput" → null),
        List("""Null value was provided for the NotNull Type 'String!' at path 'input'.""" → Nil),
        validateQuery = false)
    }

    "Handles lists and nullability" when {
      "allows lists to be null" in  check(
        (),
        """
          query q($input: [String]) {
            list(input: $input)
          }
        """,
        Map("data" → Map("list" → null)),
        """{"input": null}""".parseJson
      )

      "allows lists to contain values" in  check(
        (),
        """
          query q($input: [String]) {
            list(input: $input)
          }
        """,
        Map("data" → Map("list" → "[\"A\"]")),
        """{"input": ["A"]}""".parseJson
      )

      "allows lists to contain null" in  check(
        (),
        """
          query q($input: [String]) {
            list(input: $input)
          }
        """,
        Map("data" → Map("list" → "[\"A\",null,\"B\"]")),
        """{"input": ["A", null, "B"]}""".parseJson
      )

      "does not allow non-null lists to be null" in  checkContainsErrors(
        (),
        """
          query q($input: [String]!) {
            nnList(input: $input)
          }
        """,
        null,
        List("""Variable '$input' expected value of type '[String]!' but got: null.""" → List(Pos(2, 19))),
        """{"input": null}""".parseJson
      )

      "allows non-null lists to contain values" in  check(
        (),
        """
          query q($input: [String]!) {
            nnList(input: $input)
          }
        """,
        Map("data" → Map("nnList" → "[\"A\"]")),
        """{"input": ["A"]}""".parseJson
      )

      "allows non-null lists to contain null" in  check(
        (),
        """
          query q($input: [String]!) {
            nnList(input: $input)
          }
        """,
        Map("data" → Map("nnList" → "[\"A\",null,\"B\"]")),
        """{"input": ["A",null,"B"]}""".parseJson
      )

      "allows lists of non-nulls to be null" in  check(
        (),
        """
          query q($input: [String!]) {
            listNN(input: $input)
          }
        """,
        Map("data" → Map("listNN" → null)),
        """{"input": null}""".parseJson
      )

      "allows lists of non-nulls to contain values" in  check(
        (),
        """
          query q($input: [String!]) {
            listNN(input: $input)
          }
        """,
        Map("data" → Map("listNN" → "[\"A\"]")),
        """{"input": ["A"]}""".parseJson
      )

      "does not allow lists of non-nulls to contain null" in  checkContainsErrors(
        (),
        """
          query q($input: [String!]) {
            listNN(input: $input)
          }
        """,
        null,
        List("""Variable '$input' expected value of type '[String!]' but got: ["A",null,"B"].""" → List(Pos(2, 19))),
        """{"input": ["A",null,"B"]}""".parseJson
      )

      "does not allow non-null lists of non-nulls to be null" in  checkContainsErrors(
        (),
        """
          query q($input: [String!]) {
            nnListNN(input: $input)
          }
        """,
        Map("nnListNN" → null),
        List("""Null value was provided for the NotNull Type '[String!]!' at path 'input'.""" → Nil),
        """{"input": null}""".parseJson,
        validateQuery = false
      )

      "allows non-null lists of non-nulls to contain values" in  check(
        (),
        """
          query q($input: [String!]!) {
            nnListNN(input: $input)
          }
        """,
        Map("data" → Map("nnListNN" → "[\"A\"]")),
        """{"input": ["A"]}""".parseJson
      )

      "does not allow non-null lists of non-nulls to contain null" in  checkContainsErrors(
        (),
        """
          query q($input: [String!]!) {
            nnListNN(input: $input)
          }
        """,
        null,
        List("""Variable '$input' expected value of type '[String!]!' but got: ["A",null,"B"].""" → List(Pos(2, 19))),
        """{"input": ["A",null,"B"]}""".parseJson
      )

      "does not allow invalid types to be used as values" in  checkContainsErrors(
        (),
        """
          query q($input: TestType!) {
            fieldWithObjectInput(input: $input)
          }
        """,
        null,
        List("""Variable 'TestType!' expected value of type '$input' which cannot be used as an input type.""" → List(Pos(2, 19))),
        """{"input": ["A", "B"]}""".parseJson,
        validateQuery = false
      )

      "does not allow unknown types to be used as values" in  checkContainsErrors(
        (),
        """
          query q($input: UnknownType!) {
            fieldWithObjectInput(input: $input)
          }
        """,
        null,
        List("""Variable 'UnknownType!' expected value of type '$input' which cannot be used as an input type.""" → List(Pos(2, 19))),
        """{"input": "whoknows"}""".parseJson,
        validateQuery = false
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
        Map("data" → Map("fieldWithDefaultArgumentValue" → "\"Hello World\""))
      )

      "when nullable variable provided" in  check(
        (),
        """
          query optionalVariable($optional: String) {
            fieldWithDefaultArgumentValue(input: $optional)
          }
        """,
        Map("data" → Map("fieldWithDefaultArgumentValue" → "\"Hello World\"")) ,
        validateQuery = false
      )

      "when argument provided cannot be coerced" in  check(
        (),
        """
          {
            fieldWithDefaultArgumentValue(input: WRONG_TYPE)
          }
        """,
        Map("data" → Map("fieldWithDefaultArgumentValue" → "\"Hello World\"")),
        validateQuery = false
      )
    }
  }
}
