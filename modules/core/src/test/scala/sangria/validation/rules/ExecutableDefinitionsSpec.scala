package sangria.validation.rules

import sangria.util.{Pos, ValidationSupport}
import org.scalatest.wordspec.AnyWordSpec

class ExecutableDefinitionsSpec extends AnyWordSpec with ValidationSupport {

  override val defaultRule = Some(new ExecutableDefinitions)

  "Validate: Executable definitions" should {
    "with only operation" in expectPasses(
      """
        query Foo {
          dog {
            name
          }
        }
      """)

    "with operation and fragment" in expectPasses(
      """
        query Foo {
          dog {
            name
            ...Frag
          }
        }

        fragment Frag on Dog {
          name
        }
      """)

    "with type definition" in expectFails(
      """
        query Foo {
          dog {
            name
          }
        }

        type Cow {
          name: String
        }

        extend type Dog {
          color: String
        }
      """,
      List(
        "The 'Cow' definition is not executable." -> Some(Pos(8, 9)),
        "The 'Dog' definition is not executable" -> Some(Pos(12, 9))
      ))
  }
}
