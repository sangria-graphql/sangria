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

    "no required variables with default values" in expectFails(
      """
        query UnreachableDefaultValues($a: Int! = 3, $b: String! = "default") {
          dog { name }
        }
      """,
      List(
        "Variable $a of type Int! is required and will never use the default value. Perhaps you meant to use type Int." -> Some(Pos(2, 51)),
        "Variable $b of type String! is required and will never use the default value. Perhaps you meant to use type String." -> Some(Pos(2, 68))
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
        "Variable $a of type Int has invalid default value: \"one\"." -> Some(Pos(3, 21)),
        "Variable $b of type String has invalid default value: 4." -> Some(Pos(4, 24)),
        "Variable $c of type ComplexInput has invalid default value: \"notverycomplex\"." -> Some(Pos(5, 30))
      ))

    "complex variables missing required field" in expectFails(
      """
        query MissingRequiredField($a: ComplexInput = {intField: 3}) {
          dog { name }
        }
      """,
      List(
        "Variable $a of type ComplexInput has invalid default value: {intField: 3}." -> Some(Pos(2, 55))
      ))

    "list variables with invalid item" in expectFails(
      """
        query InvalidItem($a: [String] = ["one", 2]) {
          dog { name }
        }
      """,
      List(
        "Variable $a of type [String] has invalid default value: [\"one\", 2]." -> Some(Pos(2, 42))
      ))
  }
}
