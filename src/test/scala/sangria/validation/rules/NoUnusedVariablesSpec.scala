package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class NoUnusedVariablesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new NoUnusedVariables)

  "Validate: No unused variables" should {
    "uses all variables" in expectPasses(
      """
        query Foo($a: String, $b: String, $c: String) {
          field(a: $a, b: $b, c: $c)
        }
      """)

    "uses all variables deeply" in expectPasses(
      """
        query Foo($a: String, $b: String, $c: String) {
          field(a: $a) {
            field(b: $b) {
              field(c: $c)
            }
          }
        }
      """)

    "uses all variables deeply in inline fragments" in expectPasses(
      """
        query Foo($a: String, $b: String, $c: String) {
          ... on Type {
            field(a: $a) {
              field(b: $b) {
                ... on Type {
                  field(c: $c)
                }
              }
            }
          }
        }
      """)

    "uses all variables in fragments" in expectPasses(
      """
        query Foo($a: String, $b: String, $c: String) {
          ...FragA
        }
        fragment FragA on Type {
          field(a: $a) {
            ...FragB
          }
        }
        fragment FragB on Type {
          field(b: $b) {
            ...FragC
          }
        }
        fragment FragC on Type {
          field(c: $c)
        }
      """)

    "variable used by fragment in multiple operations" in expectPasses(
      """
        query Foo($a: String) {
          ...FragA
        }
        query Bar($b: String) {
          ...FragB
        }
        fragment FragA on Type {
          field(a: $a)
        }
        fragment FragB on Type {
          field(b: $b)
        }
      """)

    "variable used by recursive fragment" in expectPasses(
      """
        query Foo($a: String) {
          ...FragA
        }
        fragment FragA on Type {
          field(a: $a) {
            ...FragA
          }
        }
      """)

    "variable not used" in expectFails(
      """
        query Foo($a: String, $b: String, $c: String) {
          field(a: $a, b: $b)
        }
      """,
      List(
        "Variable '$c' is not used in operation Foo." → Some(Pos(2, 43))
      ))

    "multiple variables not used" in expectFails(
      """
        query Foo($a: String, $b: String, $c: String) {
          field(b: $b)
        }
      """,
      List(
        "Variable '$a' is not used in operation Foo." → Some(Pos(2, 19)),
        "Variable '$c' is not used in operation Foo." → Some(Pos(2, 43))
      ))

    "variable not used in fragments" in expectFails(
      """
        query Foo($a: String, $b: String, $c: String) {
          ...FragA
        }
        fragment FragA on Type {
          field(a: $a) {
            ...FragB
          }
        }
        fragment FragB on Type {
          field(b: $b) {
            ...FragC
          }
        }
        fragment FragC on Type {
          field
        }
      """,
      List(
        "Variable '$c' is not used in operation Foo." → Some(Pos(2, 43))
      ))

    "multiple variables not used 1" in expectFails(
      """
        query Foo($a: String, $b: String, $c: String) {
          ...FragA
        }
        fragment FragA on Type {
          field {
            ...FragB
          }
        }
        fragment FragB on Type {
          field(b: $b) {
            ...FragC
          }
        }
        fragment FragC on Type {
          field
        }
      """,
      List(
        "Variable '$a' is not used in operation Foo." → Some(Pos(2, 19)),
        "Variable '$c' is not used in operation Foo." → Some(Pos(2, 43))
      ))

    "variable not used by unreferenced fragment" in expectFails(
      """
        query Foo($b: String) {
          ...FragA
        }
        fragment FragA on Type {
          field(a: $a)
        }
        fragment FragB on Type {
          field(b: $b)
        }
      """,
      List(
        "Variable '$b' is not used in operation Foo." → Some(Pos(2, 19))
      ))

    "variable not used by fragment used by other operation" in expectFails(
      """
        query Foo($b: String) {
          ...FragA
        }
        query Bar($a: String) {
          ...FragB
        }
        fragment FragA on Type {
          field(a: $a)
        }
        fragment FragB on Type {
          field(b: $b)
        }
      """,
      List(
        "Variable '$b' is not used in operation Foo." → Some(Pos(2, 19)),
        "Variable '$a' is not used in operation Bar." → Some(Pos(5, 19))
      ))
  }
}
