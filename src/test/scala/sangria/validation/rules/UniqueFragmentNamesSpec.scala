package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class UniqueFragmentNamesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new UniqueFragmentNames)

  "Validate: Unique fragment names" should {
    "no fragments" in expectPasses(
      """
        {
          field
        }
      """)

    "one fragment" in expectPasses(
      """
        {
          ...fragA
        }

        fragment fragA on Type {
          field
        }
      """)

    "many fragments" in expectPasses(
      """
        {
          ...fragA
          ...fragB
          ...fragC
        }
        fragment fragA on Type {
          fieldA
        }
        fragment fragB on Type {
          fieldB
        }
        fragment fragC on Type {
          fieldC
        }
      """)

    "inline fragments are always unique" in expectPasses(
      """
        {
          ...on Type {
            fieldA
          }
          ...on Type {
            fieldB
          }
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

    "fragments named the same" in expectFails(
      """
        {
          ...fragA
        }
        fragment fragA on Type {
          fieldA
        }
        fragment fragA on Type {
          fieldB
        }
      """,
      List(
        "There can only be one fragment named 'fragA'." → Some(Pos(8, 9))
      ))

    "fragments named the same without being referenced" in expectFails(
      """
        fragment fragA on Type {
          fieldA
        }
        fragment fragA on Type {
          fieldB
        }
      """,
      List(
        "There can only be one fragment named 'fragA'." → Some(Pos(5, 9))
      ))
  }
}
