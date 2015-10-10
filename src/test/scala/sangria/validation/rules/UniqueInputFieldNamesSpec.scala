package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class UniqueInputFieldNamesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new UniqueInputFieldNames)

  "Validate: Unique input field names" should {
    "input object with fields" in expectPasses(
      """
        {
          field(arg: { f: true })
        }
      """)

    "same input object within two args" in expectPasses(
      """
        {
          field(arg1: { f: true }, arg2: { f: true })
        }
      """)

    "multiple input object fields" in expectPasses(
      """
        {
          field(arg: { f1: "value", f2: "value", f3: "value" })
        }
      """)

    "duplicate input object fields" in expectFailsPosList(
      """
        {
          field(arg: { f1: "value", f1: "value" })
        }
      """,
      List(
        "There can be only one input field named 'f1'." → List(Pos(3, 24), Pos(3, 37))
      ))

    "many duplicate input object fields" in expectFailsPosList(
      """
        {
          field(arg: { f1: "value", f1: "value", f1: "value" })
        }
      """,
      List(
        "There can be only one input field named 'f1'." → List(Pos(3, 24), Pos(3, 37)),
        "There can be only one input field named 'f1'." → List(Pos(3, 24), Pos(3, 50))
      ))
  }
}
