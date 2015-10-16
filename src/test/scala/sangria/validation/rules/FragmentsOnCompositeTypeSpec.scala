package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class FragmentsOnCompositeTypeSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new FragmentsOnCompositeType)

  "Validate: Fragments on composite types" should {
    "object is valid fragment type" in expectPasses(
      """
        fragment validFragment on Dog {
          barks
        }
      """)

    "interface is valid fragment type" in expectPasses(
      """
        fragment validFragment on Pet {
          name
        }
      """)

    "object is valid inline fragment type" in expectPasses(
      """
        fragment validFragment on Pet {
          ... on Dog {
            barks
          }
        }
      """)

    "union is valid fragment type" in expectPasses(
      """
        fragment validFragment on CatOrDog {
          __typename
        }
      """)

    "scalar is invalid fragment type" in expectFails(
      """
        fragment scalarFragment on Boolean {
          bad
        }
      """,
      List(
        "Fragment 'scalarFragment' cannot condition on non composite type 'Boolean'." → Some(Pos(2, 36))
      ))

    "enum is invalid fragment type" in expectFails(
      """
        fragment scalarFragment on FurColor {
          bad
        }
      """,
      List(
        "Fragment 'scalarFragment' cannot condition on non composite type 'FurColor'." → Some(Pos(2, 36))
      ))

    "input object is invalid fragment type" in expectFails(
      """
        fragment inputFragment on ComplexInput {
          stringField
        }
      """,
      List(
        "Fragment 'inputFragment' cannot condition on non composite type 'ComplexInput'." → Some(Pos(2, 35))
      ))

    "scalar is invalid inline fragment type" in expectFails(
      """
        fragment invalidFragment on Pet {
          ... on String {
            barks
          }
        }
      """,
      List(
        "Fragment cannot condition on non composite type 'String'." → Some(Pos(3, 18))
      ))

    "inline fragment without type is valid" in expectPasses(
      """
        fragment validFragment on Pet {
          ... {
            name
          }
        }
      """)
  }
}
