package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class KnownTypeNamesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new KnownTypeNames)

  "Validate: Known type names" should {
    "known type names are valid" in expectPasses(
      """
        query Foo($var: String, $required: [String!]!) {
          user(id: 4) {
            pets { ... on Pet { name }, ...PetFields, ... { name }}
          }
        }
        fragment PetFields on Pet {
          name
        }
      """)

    "unknown type names are invalid" in expectFails(
      """
        query Foo($var: JumbledUpLetters) {
          user(id: 4) {
            name
            pets { ... on Badger { name }, ...PetFields }
          }
        }
        fragment PetFields on Peettt {
          name
        }
      """,
      List(
        "Unknown type 'JumbledUpLetters'." → Some(Pos(2, 25)),
        "Unknown type 'Badger'." → Some(Pos(5, 27)),
        "Unknown type 'Peettt'. Did you mean 'Pet'?" → Some(Pos(8, 31))
      ))
  }
}
