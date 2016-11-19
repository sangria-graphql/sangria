package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class UniqueDirectivesPerLocationSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new UniqueDirectivesPerLocation)

  "Validate: Directives Are Unique Per Location" should {
    "no directives" in expectPasses(
      """
        fragment Test on Type {
          field
        }
      """)

    "unique directives in different locations" in expectPasses(
      """
        fragment Test on Type @directiveA {
          field @directiveB
        }
      """)

    "unique directives in same locations" in expectPasses(
      """
        fragment Test on Type @directiveA @directiveB {
          field @directiveA @directiveB
        }
      """)

    "same directives in different locations" in expectPasses(
      """
        fragment Test on Type @directiveA {
          field @directiveA
        }
      """)

    "same directives in similar locations" in expectPasses(
      """
        fragment Test on Type {
          field @directive
          field @directive
        }
      """)

    "duplicate directives in one location" in expectFailsPosList(
      """
        fragment Test on Type {
          field @directive @directive
        }
      """,
      List(
        "The directive 'directive' can only be used once at this location." → List(Pos(3, 17), Pos(3, 28))
      ))

    "many duplicate directives in one location" in expectFailsPosList(
      """
        fragment Test on Type {
          field @directive @directive @directive
        }
      """,
      List(
        "The directive 'directive' can only be used once at this location." → List(Pos(3, 17), Pos(3, 28)),
        "The directive 'directive' can only be used once at this location." → List(Pos(3, 17), Pos(3, 39))
      ))

    "different duplicate directives in one location" in expectFailsPosList(
      """
        fragment Test on Type {
          field @directiveA @directiveB @directiveA @directiveB
        }
      """,
      List(
        "The directive 'directiveA' can only be used once at this location." → List(Pos(3, 17), Pos(3, 41)),
        "The directive 'directiveB' can only be used once at this location." → List(Pos(3, 29), Pos(3, 53))
      ))

    "duplicate directives in many locations" in expectFailsPosList(
      """
        fragment Test on Type @directive @directive {
          field @directive @directive
        }
      """,
      List(
        "The directive 'directive' can only be used once at this location." → List(Pos(2, 31), Pos(2, 42)),
        "The directive 'directive' can only be used once at this location." → List(Pos(3, 17), Pos(3, 28))
      ))
  }
}
