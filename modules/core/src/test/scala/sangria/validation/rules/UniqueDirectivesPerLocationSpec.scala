package sangria.validation.rules

import sangria.util.{Pos, ValidationSupport}
import org.scalatest.wordspec.AnyWordSpec

class UniqueDirectivesPerLocationSpec extends AnyWordSpec with ValidationSupport {

  override val defaultRule = Some(new UniqueDirectivesPerLocation)

  "Validate: Directives Are Unique Per Location" should {
    "no directives" in expectPasses("""
        fragment Test on Type {
          field
        }
      """)

    "unique directives in different locations" in expectPasses("""
        fragment Test on Type @genericDirectiveA {
          field @genericDirectiveB
        }
      """)

    "unique directives in same locations" in expectPasses("""
        fragment Test on Type @genericDirectiveA @genericDirectiveB {
          field @genericDirectiveA @genericDirectiveB
        }
      """)

    "same directives in different locations" in expectPasses("""
        fragment Test on Type @genericDirectiveA {
          field @genericDirectiveA
        }
      """)

    "same directives in similar locations" in expectPasses("""
        fragment Test on Type {
          field @genericDirectiveA
          field @genericDirectiveA
        }
      """)

    "repeatable directives in same location" in expectPasses("""
        type Test @repeatableDirective(id: 1) @repeatableDirective(id: 2) {
          field: String!
        }
      """)

    "repeatable directives in similar locations" in expectPasses("""
        type Test @repeatableDirective(id: 1) {
          field: String!
        }
        extend type Test @repeatableDirective(id: 2) {
          anotherField: String!
        }
      """)

    "unknown directives must be ignored" in expectPasses("""
        type Test @unknownDirective @unknownDirective {
          field: String!
        }

        extend type Test @unknownDirective {
          anotherField: String!
        }
      """)

    "duplicate directives in one location" in expectFailsPosList(
      """
        fragment Test on Type {
          field @genericDirectiveA @genericDirectiveA
        }
      """,
      List(
        "The directive 'genericDirectiveA' can only be used once at this location." -> List(
          Pos(3, 17),
          Pos(3, 36))
      )
    )

    "many duplicate directives in one location" in expectFailsPosList(
      """
        fragment Test on Type {
          field @genericDirectiveA @genericDirectiveA @genericDirectiveA
        }
      """,
      List(
        "The directive 'genericDirectiveA' can only be used once at this location." -> List(
          Pos(3, 17),
          Pos(3, 36)),
        "The directive 'genericDirectiveA' can only be used once at this location." -> List(
          Pos(3, 17),
          Pos(3, 55))
      )
    )

    "different duplicate directives in one location" in expectFailsPosList(
      """
        fragment Test on Type {
          field @genericDirectiveA @genericDirectiveB @genericDirectiveA @genericDirectiveB
        }
      """,
      List(
        "The directive 'genericDirectiveA' can only be used once at this location." -> List(
          Pos(3, 17),
          Pos(3, 55)),
        "The directive 'genericDirectiveB' can only be used once at this location." -> List(
          Pos(3, 36),
          Pos(3, 74))
      )
    )

    "duplicate directives in many locations" in expectFailsPosList(
      """
        fragment Test on Type @genericDirectiveA @genericDirectiveA {
          field @genericDirectiveA @genericDirectiveA
        }
      """,
      List(
        "The directive 'genericDirectiveA' can only be used once at this location." -> List(
          Pos(2, 31),
          Pos(2, 50)),
        "The directive 'genericDirectiveA' can only be used once at this location." -> List(
          Pos(3, 17),
          Pos(3, 36))
      )
    )
  }
}
