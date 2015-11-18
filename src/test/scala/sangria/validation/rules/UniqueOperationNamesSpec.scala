package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class UniqueOperationNamesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new UniqueOperationNames)

  "Validate: Unique operation names" should {
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

    "one named operation" in expectPasses(
      """
        query Foo {
          field
        }
      """)

    "multiple operations" in expectPasses(
      """
        query Foo {
          field
        }

        query Bar {
          field
        }
      """)

    "multiple operations of different types" in expectPasses(
      """
        query Foo {
          field
        }

        mutation Bar {
          field
        }

        subscription Baz {
          field
        }
      """)

    "fragment and operation named the same" in expectPasses(
      """
        query Foo {
          ...Foo
        }
        fragment Foo on Type {
          field
        }
      """)

    "multiple operations of same name" in expectFails(
      """
        query Foo {
          fieldA
        }
        query Foo {
          fieldB
        }
      """,
      List(
        "There can only be one operation named 'Foo'." → Some(Pos(5, 9))
      ))

    "multiple operations of same name of different types (mutation)" in expectFails(
      """
        query Foo {
          fieldA
        }
        mutation Foo {
          fieldB
        }
      """,
      List(
        "There can only be one operation named 'Foo'." → Some(Pos(5, 9))
      ))

    "multiple operations of same name of different types (subscription)" in expectFails(
      """
        query Foo {
          fieldA
        }
        subscription Foo {
          fieldB
        }
      """,
      List(
        "There can only be one operation named 'Foo'." → Some(Pos(5, 9))
      ))
  }
}
