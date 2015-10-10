package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class ScalarLeafsSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new ScalarLeafs)

  "Validate: Scalar leafs" should {
    "valid scalar selection" in expectPasses(
      """
        fragment scalarSelection on Dog {
          barks
        }
      """)

    "object type missing selection" in expectFails(
      """
        query directQueryOnObjectWithoutSubFields {
          human
        }
      """,
      List(
        "Field 'human' of type 'Human' must have a sub selection." → Some(Pos(3, 11))
      ))

    "interface type missing selection" in expectFails(
      """
        {
          human { pets }
        }
      """,
      List(
        "Field 'pets' of type 'Pet' must have a sub selection." → Some(Pos(3, 19))
      ))

    "valid scalar selection with args" in expectPasses(
      """
        fragment scalarSelectionWithArgs on Dog {
          doesKnowCommand(dogCommand: SIT)
        }
      """)

    "scalar selection not allowed on Boolean" in expectFails(
      """
        fragment scalarSelectionsNotAllowedOnBoolean on Dog {
          barks { sinceWhen }
        }
      """,
      List(
        "Field 'barks' of type 'Boolean' must not have a sub selection." → Some(Pos(3, 11))
      ))

    "scalar selection not allowed on Enum" in expectFails(
      """
        fragment scalarSelectionsNotAllowedOnEnum on Cat {
          furColor { inHexdec }
        }
      """,
      List(
        "Field 'furColor' of type 'FurColor' must not have a sub selection." → Some(Pos(3, 11))
      ))

    "scalar selection not allowed with args" in expectFails(
      """
        fragment scalarSelectionsNotAllowedWithArgs on Dog {
          doesKnowCommand(dogCommand: SIT) { sinceWhen }
        }
      """,
      List(
        "Field 'doesKnowCommand' of type 'Boolean' must not have a sub selection." → Some(Pos(3, 11))
      ))

    "Scalar selection not allowed with directives" in expectFails(
      """
        fragment scalarSelectionsNotAllowedWithDirectives on Dog {
          name @include(if: true) { isAlsoHumanName }
        }
      """,
      List(
        "Field 'name' of type 'String' must not have a sub selection." → Some(Pos(3, 11))
      ))

    "Scalar selection not allowed with directives and args" in expectFails(
      """
        fragment scalarSelectionsNotAllowedWithDirectivesAndArgs on Dog {
          doesKnowCommand(dogCommand: SIT) @include(if: true) { sinceWhen }
        }
      """,
      List(
        "Field 'doesKnowCommand' of type 'Boolean' must not have a sub selection." → Some(Pos(3, 11))
      ))
  }
}
