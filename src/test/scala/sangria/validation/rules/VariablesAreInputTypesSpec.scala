package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class VariablesAreInputTypesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new VariablesAreInputTypes)

  "Validate: Variables are input types" should {
    "input types are valid" in expectPasses(
      """
        query Foo($a: String, $b: [Boolean!]!, $c: ComplexInput) {
          field(a: $a, b: $b, c: $c)
        }
      """)

    "output types are invalid" in expectFails(
      """
        query Foo($a: Dog, $b: [[CatOrDog!]]!, $c: Pet) {
          field(a: $a, b: $b, c: $c)
        }
      """,
      List(
        "Variable '$a' cannot be non input type 'Dog'." → Some(Pos(2, 23)),
        "Variable '$b' cannot be non input type '[[CatOrDog!]]!'." → Some(Pos(2, 32)),
        "Variable '$c' cannot be non input type 'Pet'." → Some(Pos(2, 52))
      ))
  }
}
