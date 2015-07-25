package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class KnownDirectivesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new KnownDirectives)

  "Validate: Known directives" should {
    "with no directives" in expectPasses(
      """
        query Foo {
          name
          ...Frag
        }

        fragment Frag on Dog {
          name
        }
      """)

    "with known directives" in expectPasses(
      """
        {
          dog @include(if: true) {
            name
          }
          human @skip(if: false) {
            name
          }
        }
      """)

    "with unknown directive" in expectFails(
      """
        {
          dog @unknown(directive: "value") {
            name
          }
        }
      """,
      List(
        "Unknown directive unknown." -> Some(Pos(3, 15))
      ))

    "with many unknown directives" in expectFails(
      """
        {
          dog @unknown(directive: "value") {
            name
          }
          human @unknown(directive: "value") {
            name
            pets @unknown(directive: "value") {
              name
            }
          }
        }
      """,
      List(
        "Unknown directive unknown." -> Some(Pos(3, 15)),
        "Unknown directive unknown." -> Some(Pos(6, 17)),
        "Unknown directive unknown." -> Some(Pos(8, 18))
      ))

    "with well placed directives" in expectPasses(
      """
        query Foo {
          name @include(if: true)
          ...Frag @include(if: true)
          skippedField @skip(if: true)
          ...SkippedFrag @skip(if: true)
        }
      """)

    "with misplaced directives" in expectFails(
      """
        query Foo @include(if: true) {
          name
          ...Frag
        }
      """,
    List(
      "Directive include may not be used on operation." -> Some(Pos(2, 19))
    ))
  }
}
