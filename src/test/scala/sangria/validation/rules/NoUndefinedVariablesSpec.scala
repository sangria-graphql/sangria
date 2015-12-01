package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class NoUndefinedVariablesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new NoUndefinedVariables)

  "Validate: No undefined variables" should {
    "all variables defined" in expectPasses(
      """
        query Foo($a: String, $b: String, $c: String) {
          field(a: $a, b: $b, c: $c)
        }
      """)

    "all variables deeply defined" in expectPasses(
      """
        query Foo($a: String, $b: String, $c: String) {
          field(a: $a) {
            field(b: $b) {
              field(c: $c)
            }
          }
        }
      """)

    "all variables deeply in inline fragments defined" in expectPasses(
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

    "all variables in fragments deeply defined" in expectPasses(
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

    "variable within single fragment defined in multiple operations" in expectPasses(
      """
        query Foo($a: String) {
          ...FragA
        }
        query Bar($a: String) {
          ...FragA
        }
        fragment FragA on Type {
          field(a: $a)
        }
      """)

    "variable within fragments defined in operations" in expectPasses(
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

    "variable within recursive fragment defined" in expectPasses(
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

    "variable not defined" in expectFailsPosList(
      """
        query Foo($a: String, $b: String, $c: String) {
          field(a: $a, b: $b, c: $c, d: $d)
        }
      """,
      List(
        "Variable '$d' is not defined by operation 'Foo'." → List(Pos(3, 41), Pos(2, 9))
      ))

    "variable not defined by un-named query" in expectFailsPosList(
      """
        {
          field(a: $a)
        }
      """,
      List(
        "Variable '$a' is not defined." → List(Pos(3, 20), Pos(2, 9))
      ))

    "multiple variables not defined" in expectFailsPosList(
      """
        query Foo($b: String) {
          field(a: $a, b: $b, c: $c)
        }
      """,
      List(
        "Variable '$a' is not defined by operation 'Foo'." → List(Pos(3, 20), Pos(2, 9)),
        "Variable '$c' is not defined by operation 'Foo'." → List(Pos(3, 34), Pos(2, 9))
      ))

    "variable in fragment not defined by un-named query" in expectFailsPosList(
      """
        {
          ...FragA
        }
        fragment FragA on Type {
          field(a: $a)
        }
      """,
      List(
        "Variable '$a' is not defined." → List(Pos(6, 20), Pos(2, 9))
      ))

    "variable in fragment not defined by operation" in expectFailsPosList(
      """
        query Foo($a: String, $b: String) {
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
      """,
      List(
        "Variable '$c' is not defined by operation 'Foo'." → List(Pos(16, 20), Pos(2, 9))
      ))

    "multiple variables in fragments not defined" in expectFailsPosList(
      """
        query Foo($b: String) {
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
      """,
      List(
        "Variable '$a' is not defined by operation 'Foo'." → List(Pos(6, 20), Pos(2, 9)),
        "Variable '$c' is not defined by operation 'Foo'." → List(Pos(16, 20), Pos(2, 9))
      ))

    "single variable in fragment not defined by multiple operations" in expectFailsPosList(
      """
        query Foo($a: String) {
          ...FragAB
        }
        query Bar($a: String) {
          ...FragAB
        }
        fragment FragAB on Type {
          field(a: $a, b: $b)
        }
      """,
      List(
        "Variable '$b' is not defined by operation 'Foo'." → List(Pos(9, 27), Pos(2, 9)),
        "Variable '$b' is not defined by operation 'Bar'." → List(Pos(9, 27), Pos(5, 9))
      ))

    "variables in fragment not defined by multiple operations" in expectFailsPosList(
      """
        query Foo($b: String) {
          ...FragAB
        }
        query Bar($a: String) {
          ...FragAB
        }
        fragment FragAB on Type {
          field(a: $a, b: $b)
        }
      """,
      List(
        "Variable '$a' is not defined by operation 'Foo'." → List(Pos(9, 20), Pos(2, 9)),
        "Variable '$b' is not defined by operation 'Bar'." → List(Pos(9, 27), Pos(5, 9))
      ))

    "variable in fragment used by other operation" in expectFailsPosList(
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
        "Variable '$a' is not defined by operation 'Foo'." → List(Pos(9, 20), Pos(2, 9)),
        "Variable '$b' is not defined by operation 'Bar'." → List(Pos(12, 20), Pos(5, 9))
      ))

    "multiple undefined variables produce multiple errors" in expectFailsPosList(
      """
        query Foo($b: String) {
          ...FragAB
        }
        query Bar($a: String) {
          ...FragAB
        }
        fragment FragAB on Type {
          field1(a: $a, b: $b)
          ...FragC
          field3(a: $a, b: $b)
        }
        fragment FragC on Type {
          field2(c: $c)
        }
      """,
      List(
        "Variable '$a' is not defined by operation 'Foo'." → List(Pos(9, 21), Pos(2, 9)),
        "Variable '$c' is not defined by operation 'Foo'." → List(Pos(14, 21), Pos(2, 9)),
        "Variable '$a' is not defined by operation 'Foo'." → List(Pos(11, 21), Pos(2, 9)),
        "Variable '$b' is not defined by operation 'Bar'." → List(Pos(9, 28), Pos(5, 9)),
        "Variable '$c' is not defined by operation 'Bar'." → List(Pos(14, 21), Pos(5, 9)),
        "Variable '$b' is not defined by operation 'Bar'." → List(Pos(11, 28), Pos(5, 9))
      ))
  }
}
