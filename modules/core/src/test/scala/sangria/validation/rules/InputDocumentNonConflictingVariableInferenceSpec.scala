package sangria.validation.rules

import sangria.util.{Pos, ValidationSupport}
import org.scalatest.wordspec.AnyWordSpec

class InputDocumentNonConflictingVariableInferenceSpec extends AnyWordSpec with ValidationSupport {

  override val defaultRule = Some(new InputDocumentNonConflictingVariableInference)

  "InputDocumentNonConflictingVariableInference" should {
    "variable used multiple times in the right position" in expectInputPasses("ComplexInput",
      """
        {
          requiredField: true
          stringField: $foo
          stringListField: [$foo]
        }
      """)

    "variable used 2 times with incompatible types" in expectInputFails("ComplexInput",
      """
        {
          requiredField: $foo
          stringField: "hello world"
          stringListField: [$foo]
        }
      """,
      List(
        "Inferred variable '$foo' is used with two conflicting types: 'Boolean!' and 'String'." -> List(Pos(5, 29), Pos(3, 26))
      ))

    "variable used multiple times with incompatible types" in expectInputFails("ComplexInput",
      """
        {
          requiredField: $foo
          intField: $foo
          stringField: $foo
          stringListField: [$foo]
        }
      """,
      List(
        "Inferred variable '$foo' is used with two conflicting types: 'Boolean!' and 'Int'." -> List(Pos(4, 21), Pos(3, 26)),
        "Inferred variable '$foo' is used with two conflicting types: 'Boolean!' and 'String'." -> List(Pos(5, 24), Pos(3, 26)),
        "Inferred variable '$foo' is used with two conflicting types: 'Boolean!' and 'String'." -> List(Pos(6, 29), Pos(3, 26))
      ))
  }
}
