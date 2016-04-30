package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}
import sangria.validation.UndefinedFieldViolation

class FieldsOnCorrectTypeSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new FieldsOnCorrectType)

  "Validate: Fields on correct type" should {
    "Object field selection" in expectPasses(
      """
        fragment objectFieldSelection on Dog {
          __typename
          name
        }
      """)

    "Aliased object field selection" in expectPasses(
      """
        fragment aliasedObjectFieldSelection on Dog {
          tn : __typename
          otherName : name
        }
      """)

    "Interface field selection" in expectPasses(
      """
        fragment interfaceFieldSelection on Pet {
          __typename
          name
        }
      """)

    "Aliased interface field selection" in expectPasses(
      """
        fragment interfaceFieldSelection on Pet {
          otherName : name
        }
      """)

    "Lying alias selection" in expectPasses(
      """
        fragment lyingAliasSelection on Dog {
          name : nickname
        }
      """)

    "Ignores fields on unknown type" in expectPasses(
      """
        fragment unknownSelection on UnknownType {
          unknownField
        }
      """)

    "Field not defined on fragment" in expectFails(
      """
        fragment fieldNotDefined on Dog {
          meowVolume
        }
      """,
      List(
        "Cannot query field 'meowVolume' on type 'Dog'. Did you mean 'barkVolume'?" → Some(Pos(3, 11))
      ))

    "Field not defined deeply, only reports first" in expectFails(
      """
        fragment deepFieldNotDefined on Dog {
          unknown_field {
            deeper_unknown_field
          }
        }
      """,
      List(
        "Cannot query field 'unknown_field' on type 'Dog'." → Some(Pos(3, 11))
      ))

    "Sub-field not defined" in expectFails(
      """
        fragment subFieldNotDefined on Human {
          pets {
            unknown_field
          }
        }
      """,
      List(
        "Cannot query field 'unknown_field' on type 'Pet'." → Some(Pos(4, 13))
      ))

    "Field not defined on inline fragment" in expectFails(
      """
        fragment fieldNotDefined on Pet {
          ... on Dog {
            meowVolume
          }
        }
      """,
      List(
        "Cannot query field 'meowVolume' on type 'Dog'. Did you mean 'barkVolume'?" → Some(Pos(4, 13))
      ))

    "Aliased field target not defined" in expectFails(
      """
        fragment aliasedFieldTargetNotDefined on Dog {
          volume : mooVolume
        }
      """,
      List(
        "Cannot query field 'mooVolume' on type 'Dog'. Did you mean 'barkVolume'?" → Some(Pos(3, 11))
      ))

    "Aliased lying field target not defined" in expectFails(
      """
        fragment aliasedLyingFieldTargetNotDefined on Dog {
          barkVolume : kawVolume
        }
      """,
      List(
        "Cannot query field 'kawVolume' on type 'Dog'. Did you mean 'barkVolume'?" → Some(Pos(3, 11))
      ))

    "Not defined on interface" in expectFails(
      """
        fragment notDefinedOnInterface on Pet {
          tailLength
        }
      """,
      List(
        "Cannot query field 'tailLength' on type 'Pet'." → Some(Pos(3, 11))
      ))

    "Defined on implementors but not on interface" in expectFails(
      """
        fragment definedOnImplementorsButNotInterface on Pet {
          nickname
        }
      """,
      List(
        "Cannot query field 'nickname' on type 'Pet'. Did you mean to use an inline fragment on 'Cat' or 'Dog'?" → Some(Pos(3, 11))
      ))

    "Meta field selection on union" in expectPasses(
      """
        fragment directFieldSelectionOnUnion on CatOrDog {
          __typename
        }
      """)

    "Direct field selection on union" in expectFails(
      """
        fragment directFieldSelectionOnUnion on CatOrDog {
          directField
        }
      """,
      List(
        "Cannot query field 'directField' on type 'CatOrDog'." → Some(Pos(3, 11))
      ))

    "Defined on implementors queried on union" in expectFails(
      """
        fragment definedOnImplementorsQueriedOnUnion on CatOrDog {
          name
        }
      """,
      List(
        "Cannot query field 'name' on type 'CatOrDog'. Did you mean to use an inline fragment on 'Being', 'Pet', 'Canine', 'Cat' or 'Dog'?" → Some(Pos(3, 11))
      ))

    "valid field in inline fragment" in expectPasses(
      """
        fragment objectFieldSelection on Pet {
          ... on Dog {
            name
          }
        }
      """)

    "valid field in inline fragment without type condition" in expectFails(
      """
        query {
          dog {
            ... {
              name
              numberOfTails
            }
          }
        }
      """,
      List(
        "Cannot query field 'numberOfTails' on type 'Dog'" → Some(Pos(6, 15))
      ))
  }

  "Fields on correct type error message" should {
    "Works with no suggestions" in {
      UndefinedFieldViolation("f", "T", Nil, Nil, None, Nil).simpleErrorMessage should be (
        "Cannot query field 'f' on type 'T'.")
    }

    "Works with no small numbers of type suggestions" in {
      UndefinedFieldViolation("f", "T", "A" :: "B" :: Nil, Nil, None, Nil).simpleErrorMessage should be (
        "Cannot query field 'f' on type 'T'. Did you mean to use an inline fragment on 'A' or 'B'?")
    }

    "Works with no small numbers of field suggestions" in {
      UndefinedFieldViolation("f", "T", Nil, "z" :: "y" :: Nil, None, Nil).simpleErrorMessage should be (
        "Cannot query field 'f' on type 'T'. Did you mean 'z' or 'y'?")
    }

    "Only shows one set of suggestions at a time, preferring types" in {
      UndefinedFieldViolation("f", "T", "A" :: "B" :: Nil, "z" :: "y" :: Nil, None, Nil).simpleErrorMessage should be (
        "Cannot query field 'f' on type 'T'. Did you mean to use an inline fragment on 'A' or 'B'?")
    }

    "Limits lots of type suggestions" in {
      UndefinedFieldViolation("f", "T", "A" :: "B" :: "C" :: "D" :: "E" :: "F" :: Nil, Nil, None, Nil).simpleErrorMessage should be (
        "Cannot query field 'f' on type 'T'. Did you mean to use an inline fragment on 'A', 'B', 'C', 'D' or 'E'?")
    }

    "Limits lots of field suggestions" in {
      UndefinedFieldViolation("f", "T", Nil, "z" :: "y" :: "x" :: "w" :: "v" :: "u" :: Nil, None, Nil).simpleErrorMessage should be (
        "Cannot query field 'f' on type 'T'. Did you mean 'z', 'y', 'x', 'w' or 'v'?")
    }
  }
}
