package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class DefaultValuesOfCorrectTypeSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new DefaultValuesOfCorrectType)

  "Validate: Variable default values of correct type" should {
    "variables with no default values" in expectPasses(
      """
        query NullableValues($a: Int, $b: String, $c: ComplexInput) {
          dog { name }
        }
      """)

    "required variables without default values" in expectPasses(
      """
        query RequiredValues($a: Int!, $b: String!) {
          dog { name }
        }
      """)

    "variables with valid default values" in expectPasses(
      """
        query WithDefaultValues(
          $a: Int = 1,
          $b: String = "ok",
          $c: ComplexInput = { requiredField: true, intField: 3 }
        ) {
          dog { name }
        }
      """)

    "variables with valid default null values" in expectPasses(
      """
        query WithDefaultValues(
          $a: Int = null,
          $b: String = null,
          $c: ComplexInput = { requiredField: true, intField: null }
        ) {
          dog { name }
        }
      """)

    "variables with invalid default null values" in expectFailsPosList(
      """
        query WithDefaultValues(
          $a: Int! = null,
          $b: String! = null,
          $c: ComplexInput = { requiredField: null, intField: null }
        ) {
          dog { name }
        }
      """,
      List(
        "Variable '$a' of type 'Int!' is required and will never use the default value. Perhaps you meant to use type 'Int'." → List(Pos(3, 22)),
        "Variable '$b' of type 'String!' is required and will never use the default value. Perhaps you meant to use type 'String'." → List(Pos(4, 25)),
        "Variable '$c' of type 'ComplexInput' has invalid default value: {requiredField: null, intField: null}." → List(Pos(5, 30), Pos(5, 32)),
        "Variable '$a' of type 'Int!' is required and will never use the default value. Perhaps you meant to use type 'Int'." → List(Pos(3, 22)),
        "Variable '$b' of type 'String!' is required and will never use the default value. Perhaps you meant to use type 'String'." → List(Pos(4, 25))
      ))

    "no required variables with default values" in expectFails(
      """
        query UnreachableDefaultValues($a: Int! = 3, $b: String! = "default") {
          dog { name }
        }
      """,
      List(
        "Variable '$a' of type 'Int!' is required and will never use the default value. Perhaps you meant to use type 'Int'." → Some(Pos(2, 51)),
        "Variable '$b' of type 'String!' is required and will never use the default value. Perhaps you meant to use type 'String'." → Some(Pos(2, 68))
      ))

    "variables with invalid default values" in expectFails(
      """
        query InvalidDefaultValues(
          $a: Int = "one",
          $b: String = 4,
          $c: ComplexInput = "notverycomplex"
        ) {
          dog { name }
        }
      """,
      List(
        "Variable '$a' of type 'Int' has invalid default value: \"one\". Reason: Int value expected" → Some(Pos(3, 21)),
        "Variable '$b' of type 'String' has invalid default value: 4. Reason: String value expected" → Some(Pos(4, 24)),
        "Variable '$c' of type 'ComplexInput' has invalid default value: \"notverycomplex\". Reason: Expected 'ComplexInput', found not an object." → Some(Pos(5, 30))
      ))

    "complex variables missing required field" in expectFails(
      """
        query MissingRequiredField($a: ComplexInput = {intField: 3}) {
          dog { name }
        }
      """,
      List(
        "Variable '$a' of type 'ComplexInput' has invalid default value: {intField: 3}. Reason: The NotNull field 'requiredField' defined in the input type 'Boolean' is missing." → Some(Pos(2, 55))
      ))

    "list variables with invalid item" in expectFails(
      """
        query InvalidItem($a: [String] = ["one", 2]) {
          dog { name }
        }
      """,
      List(
        "Variable '$a' of type '[String]' has invalid default value: [\"one\", 2]. Reason: [at index #1] String value expected" → Some(Pos(2, 42))
      ))
  }
}
