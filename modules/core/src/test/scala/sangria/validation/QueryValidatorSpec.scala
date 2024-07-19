package sangria.validation

import org.scalatest.wordspec.AnyWordSpec
import sangria.parser.QueryParser
import sangria.schema._

import scala.util.Success

class QueryValidatorSpec extends AnyWordSpec {
  "QueryValidator" when {
    val validator = new RuleBasedQueryValidator(QueryValidator.allRules)

    "testing RuleBasedQueryValidator" should {
      val TestInputType = InputObjectType(
        name = "TestInput",
        fields = List(
          InputField("field1", StringType),
          InputField("field2", StringType)
        )
      )

      val QueryType = ObjectType(
        name = "Query",
        fields = fields[Unit, Unit](
          Field(
            name = "testField",
            fieldType = OptionType(StringType),
            arguments = List(Argument("testArg", ListInputType(TestInputType))),
            resolve = _ => None
          )
        )
      )

      val schema = Schema(QueryType)

      val invalidQuery =
        """
          query {
            testField(testArg: [{}, {}, {}, {}, {}])
          }
        """

      "not limit number of errors returned if the limit is not provided" in {
        val Success(doc) = QueryParser.parse(invalidQuery)
        val result = validator.validateQuery(schema, doc, Map.empty, None)

        // 10 errors are expected because there are 5 input objects in the list with 2 missing fields each
        assertResult(10)(result.length)
      }
      "limit number of errors returned if the limit is provided" in {
        val errorsLimit = 5

        val Success(doc) = QueryParser.parse(invalidQuery)
        val result = validator.validateQuery(schema, doc, Map.empty, Some(errorsLimit))

        assertResult(errorsLimit)(result.length)
      }
    }
  }
}
