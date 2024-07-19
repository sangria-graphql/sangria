package sangria.validation.rules

import sangria.util.{Pos, ValidationSupport}
import org.scalatest.wordspec.AnyWordSpec

class ExactlyOneOfFieldGivenSpec extends AnyWordSpec with ValidationSupport {

  override val defaultRule = Some(new ExactlyOneOfFieldGiven)

  "Validate: exactly oneOf field given" should {
    "pass with exactly one non-null field given" in expectPasses("""
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

    "fail with exactly one null field given" in expectFails(
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

    "fail with no fields given" in expectFails(
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

    "fail with more than one non-null fields given" in expectFails(
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

    "pass with a null variable and non-null arg given" in expectPasses(
      """
      query OneOfQuery($catName: String) {
        oneOfQuery(input: {
          catName: $catName,
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
      "$catName: String" -> """{"catName": null}"""
    )

    "fail with a non-null variable and non-null arg given" in expectFails(
      """
      query OneOfQuery($catName: String) {
        oneOfQuery(input: {
          catName: $catName,
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
      List("Exactly one key must be specified for oneOf type 'OneOfInput'." -> Some(Pos(3, 27))),
      "$catName: String" -> """{"catName": "Gretel"}"""
    )

    "pass with a variable object with only one non-null value" in expectPasses(
      """
      query OneOfQuery($input: OneOfInput!) {
        oneOfQuery(input: $input) {
            ... on Cat {
              name
            }
            ... on Dog {
              name
            }
          }
        }
    """,
      "$input: OneOfInput!" -> """{"input":{"catName": "Gretel", "dogId": null}}"""
    )

    "fail with a variable object with only null values" in expectFails(
      """
      query OneOfQuery($input: OneOfInput!) {
        oneOfQuery(input: $input) {
            ... on Cat {
              name
            }
            ... on Dog {
              name
            }
          }
        }
    """,
      List("Exactly one key must be specified for oneOf type 'OneOfInput'." -> Some(Pos(3, 27))),
      "$input: OneOfInput!" -> """{"input":{"catName": null}}"""
    )

    "fail with a variable object with more than one non-null values" in expectFails(
      """
      query OneOfQuery($input: OneOfInput!) {
        oneOfQuery(input: $input) {
            ... on Cat {
              name
            }
            ... on Dog {
              name
            }
          }
        }
    """,
      List("Exactly one key must be specified for oneOf type 'OneOfInput'." -> Some(Pos(3, 27))),
      "$input: OneOfInput!" -> """{"input":{"catName": "Gretel", "dogId": 123}}"""
    )

    "pass with a variable object with exactly one non-null values" in expectPasses(
      """
      query OneOfQuery($input: OneOfInput!) {
        oneOfQuery(input: $input) {
            ... on Cat {
              name
            }
            ... on Dog {
              name
            }
          }
        }
    """,
      "$input: OneOfInput!" -> """{"input":{"dogId": 123}}"""
    )

    "pass with a variables with default value but only one resolved to non-null" in expectPasses(
      """
      query OneOfQuery($catName: String = "Gretel", $dogId: Int) {
        oneOfQuery(input: {
          catName: $catName,
          dogId: $dogId
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
      """$catName: String = "Gretel", $dogId: Int""" -> """{}"""
    )

    "fail with a variable object with default value that causes there to be more than one non-null value" in expectFails(
      """
      query OneOfQuery($catName: String = "Gretel", $dogId: Int) {
        oneOfQuery(input: {
          catName: $catName,
          dogId: $dogId
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
      List("Exactly one key must be specified for oneOf type 'OneOfInput'." -> Some(Pos(3, 27))),
      """$catName: String = "Gretel", $dogId: Int""" -> """{"dogId":123}"""
    )
  }
}
