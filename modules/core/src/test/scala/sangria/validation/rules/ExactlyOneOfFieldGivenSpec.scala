package sangria.validation.rules

import sangria.util.{Pos, ValidationSupport}
import org.scalatest.wordspec.AnyWordSpec

class ExactlyOneOfFieldGivenSpec extends AnyWordSpec with ValidationSupport {

  override val defaultRule = Some(new ExactlyOneOfFieldGiven)

  "Validate: exactly oneOf field given" should {
    "with exactly one non-null field given" in expectPasses("""
          query OneOfQuery {
            oneOfQuery(input: {
                catName: "Gretel"
            }) {
                ... on Cat {
                    name
                }
            }
          }
        """)

    "with exactly one null field given" in expectFails(
      """
          query OneOfQuery {
            oneOfQuery(input: {
                catName: null
            }) {
                ... on Cat {
                    name
                }
            }
          }
        """,
      List("Exactly one key must be specified for oneOf type 'OneOfInput'." -> Some(Pos(3, 31)))
    )

    "with no fields given" in expectFails(
      """
          query OneOfQuery {
            oneOfQuery(input: {}) {
                ... on Cat {
                    name
                }
            }
          }
        """,
      List("Exactly one key must be specified for oneOf type 'OneOfInput'." -> Some(Pos(3, 31)))
    )

    "with more than one non-null fields given" in expectFails(
      """
          query OneOfQuery {
            oneOfQuery(input: {
                catName: "Gretel",
                dogId: 123
            }) {
                ... on Cat {
                    name
                }
                ... on Dog {
                    name
                }
            }
          }
        """,
      List("Exactly one key must be specified for oneOf type 'OneOfInput'." -> Some(Pos(3, 31)))
    )
  }
}
