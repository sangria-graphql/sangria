package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class VariablesInAllowedPositionSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new VariablesInAllowedPosition)

  "Validate: Variables are in allowed positions" should {
    "Boolean ⇒ Boolean" in expectPasses(
      """
        query Query($booleanArg: Boolean)
        {
          complicatedArgs {
            booleanArgField(booleanArg: $booleanArg)
          }
        }
      """)

    "Boolean ⇒ Boolean within fragment" in expectPasses(
      """
        fragment booleanArgFrag on ComplicatedArgs {
          booleanArgField(booleanArg: $booleanArg)
        }
        query Query($booleanArg: Boolean)
        {
          complicatedArgs {
            ...booleanArgFrag
          }
        }
      """)

    "Boolean ⇒ Boolean within fragment (bonus)" in expectPasses(
      """
        query Query($booleanArg: Boolean)
        {
          complicatedArgs {
            ...booleanArgFrag
          }
        }
        fragment booleanArgFrag on ComplicatedArgs {
          booleanArgField(booleanArg: $booleanArg)
        }
      """)

    "Boolean! ⇒ Boolean" in expectPasses(
      """
        query Query($nonNullBooleanArg: Boolean!)
        {
          complicatedArgs {
            booleanArgField(booleanArg: $nonNullBooleanArg)
          }
        }
      """)

    "Boolean! ⇒ Boolean within fragment" in expectPasses(
      """
        fragment booleanArgFrag on ComplicatedArgs {
          booleanArgField(booleanArg: $nonNullBooleanArg)
        }

        query Query($nonNullBooleanArg: Boolean!)
        {
          complicatedArgs {
            ...booleanArgFrag
          }
        }
      """)

    "Int ⇒ Int! with default" in expectPasses(
      """
        query Query($intArg: Int = 1)
        {
          complicatedArgs {
            nonNullIntArgField(nonNullIntArg: $intArg)
          }
        }
      """)

    "[String] ⇒ [String]" in expectPasses(
      """
        query Query($stringListVar: [String])
        {
          complicatedArgs {
            stringListArgField(stringListArg: $stringListVar)
          }
        }
      """)

    "[String!] ⇒ [String]" in expectPasses(
      """
        query Query($stringListVar: [String!])
        {
          complicatedArgs {
            stringListArgField(stringListArg: $stringListVar)
          }
        }
      """)

    "String ⇒ [String] in item position" in expectPasses(
      """
        query Query($stringVar: String)
        {
          complicatedArgs {
            stringListArgField(stringListArg: [$stringVar])
          }
        }
      """)

    "String! ⇒ [String] in item position" in expectPasses(
      """
        query Query($stringVar: String!)
        {
          complicatedArgs {
            stringListArgField(stringListArg: [$stringVar])
          }
        }
      """)

    "ComplexInput ⇒ ComplexInput" in expectPasses(
      """
        query Query($complexVar: ComplexInput)
        {
          complicatedArgs {
            complexArgField(complexArg: $complexVar)
          }
        }
      """)

    "ComplexInput ⇒ ComplexInput in field position" in expectPasses(
      """
        query Query($boolVar: Boolean = false)
        {
          complicatedArgs {
            complexArgField(complexArg: {requiredArg: $boolVar})
          }
        }
      """)

    "Boolean! ⇒ Boolean! in directive" in expectPasses(
      """
        query Query($boolVar: Boolean!)
        {
          dog @include(if: $boolVar)
        }
      """)

    "Boolean ⇒ Boolean! in directive with default" in expectPasses(
      """
        query Query($boolVar: Boolean = false)
        {
          dog @include(if: $boolVar)
        }
      """)

    "Int ⇒ Int!" in expectFailsPosList(
      """
        query Query($intArg: Int)
        {
          complicatedArgs {
            nonNullIntArgField(nonNullIntArg: $intArg)
          }
        }
      """,
      List(
        "Variable '$intArg' of type 'Int' used in position expecting type 'Int!'." → List(Pos(2, 21), Pos(5, 47))
      ))

    "Int ⇒ Int! within fragment" in expectFailsPosList(
      """
        fragment nonNullIntArgFieldFrag on ComplicatedArgs {
          nonNullIntArgField(nonNullIntArg: $intArg)
        }

        query Query($intArg: Int)
        {
          complicatedArgs {
            ...nonNullIntArgFieldFrag
          }
        }
      """,
      List(
        "Variable '$intArg' of type 'Int' used in position expecting type 'Int!'." → List(Pos(6, 21), Pos(3, 45))
      ))

    "Int ⇒ Int! within nested fragment" in expectFailsPosList(
      """
        fragment outerFrag on ComplicatedArgs {
          ...nonNullIntArgFieldFrag
        }

        fragment nonNullIntArgFieldFrag on ComplicatedArgs {
          nonNullIntArgField(nonNullIntArg: $intArg)
        }

        query Query($intArg: Int)
        {
          complicatedArgs {
            ...outerFrag
          }
        }
      """,
      List(
        "Variable '$intArg' of type 'Int' used in position expecting type 'Int!'." → List(Pos(10, 21), Pos(7, 45))
      ))

    "String over Boolean" in expectFailsPosList(
      """
        query Query($stringVar: String)
        {
          complicatedArgs {
            booleanArgField(booleanArg: $stringVar)
          }
        }
      """,
      List(
        "Variable '$stringVar' of type 'String' used in position expecting type 'Boolean'." → List(Pos(2, 21), Pos(5, 41))
      ))

    "String ⇒ [String]" in expectFailsPosList(
      """
        query Query($stringVar: String)
        {
          complicatedArgs {
            stringListArgField(stringListArg: $stringVar)
          }
        }
      """,
      List(
        "Variable '$stringVar' of type 'String' used in position expecting type '[String]'." → List(Pos(2, 21), Pos(5, 47))
      ))

    "Boolean ⇒ Boolean! in directive" in expectFailsPosList(
      """
        query Query($boolVar: Boolean)
        {
          dog @include(if: $boolVar)
        }
      """,
      List(
        "Variable '$boolVar' of type 'Boolean' used in position expecting type 'Boolean!'." → List(Pos(2, 21), Pos(4, 28))
      ))

    "String ⇒ Boolean! in directive" in expectFailsPosList(
      """
        query Query($stringVar: String)
        {
          dog @include(if: $stringVar)
        }
      """,
      List(
        "Variable '$stringVar' of type 'String' used in position expecting type 'Boolean!'." → List(Pos(2, 21), Pos(4, 28))
      ))
  }
}
