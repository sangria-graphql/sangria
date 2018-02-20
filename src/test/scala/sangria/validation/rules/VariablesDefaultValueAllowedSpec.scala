package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class VariablesDefaultValueAllowedSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new VariablesDefaultValueAllowed)

  "Validate: Variable default value is allowed" should {
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

    "variables with invalid default null values" in expectFails(
      """
        query WithDefaultValues($a: Int! = null, $b: String! = null) {
          dog { name }
        }
      """,
      List(
        "Variable '$a' of type 'Int!' is required and will never use the default value. Perhaps you meant to use type 'Int'." → Some(Pos(2, 44)),
        "Variable '$b' of type 'String!' is required and will never use the default value. Perhaps you meant to use type 'String'." → Some(Pos(2, 64))
      ))
  }
}
