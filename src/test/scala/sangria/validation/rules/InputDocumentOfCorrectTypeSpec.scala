package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class InputDocumentOfCorrectTypeSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new InputDocumentOfCorrectType)

  "InputDocumentOfCorrectType" should {
    "successfully validates" in expectInputPasses("ComplexInput",
      """
        {
          requiredField: true
          stringField: "hello world"
          stringListField: ["test"]
        }
      """)

    "fails in presence of validation errors" in expectInputFails("ComplexInput",
      """
        {
          requiredField: ["test"]
          stringField: true
          stringListField: [123]
          bestField: true
        }
      """,
      List(
        "At path 'requiredField' Boolean value expected" → List(Pos(3, 11)),
        "At path 'stringField' String value expected" → List(Pos(4, 11)),
        "At path 'stringListField[0]' String value expected" → List(Pos(5, 11), Pos(5, 28)),
        "At path 'bestField' Field 'bestField' is not defined in the input type 'ComplexInput'." → List(Pos(6, 11))
      ))
  }
}
