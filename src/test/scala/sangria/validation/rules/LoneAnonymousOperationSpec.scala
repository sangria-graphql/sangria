package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class LoneAnonymousOperationSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new LoneAnonymousOperation)

  "Validate: Anonymous operation must be alone" should {
    "no operations" in expectPasses(
      """
        fragment fragA on Type {
          field
        }
      """)

    "one anon operation" in expectPasses(
      """
        {
          field
        }
      """)

    "multiple named operations" in expectPasses(
      """
        query Foo {
          field
        }

        query Bar {
          field
        }
      """)

    "anon operation with fragment" in expectPasses(
      """
        {
          ...Foo
        }
        fragment Foo on Type {
          field
        }
      """)

    "multiple anon operations" in expectFails(
      """
        {
          fieldA
        }
        {
          fieldB
        }
      """,
      List(
        "This anonymous operation must be the only defined operation." → Some(Pos(2, 9)),
        "This anonymous operation must be the only defined operation." → Some(Pos(5, 9))
      ))

    "anon operation with another operation" in expectFails(
      """
        {
          fieldA
        }
        mutation Foo {
          fieldB
        }
      """,
      List(
        "This anonymous operation must be the only defined operation." → Some(Pos(2, 9))
      ))

    "anon operation with another operation with subscription" in expectFails(
      """
        {
          fieldA
        }
        subscription Foo {
          fieldB
        }
      """,
      List(
        "This anonymous operation must be the only defined operation." → Some(Pos(2, 9))
      ))
  }
}
