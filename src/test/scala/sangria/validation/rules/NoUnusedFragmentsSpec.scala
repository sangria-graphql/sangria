package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class NoUnusedFragmentsSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new NoUnusedFragments)

  "Validate: No unused fragments" should {
    "all fragment names are used" in expectPasses(
      """
        {
          human(id: 4) {
            ...HumanFields1
            ... on Human {
              ...HumanFields2
            }
          }
        }
        fragment HumanFields1 on Human {
          name
          ...HumanFields3
        }
        fragment HumanFields2 on Human {
          name
        }
        fragment HumanFields3 on Human {
          name
        }
      """)

    "all fragment names are used by multiple operations" in expectPasses(
      """
        query Foo {
          human(id: 4) {
            ...HumanFields1
          }
        }
        query Bar {
          human(id: 4) {
            ...HumanFields2
          }
        }
        fragment HumanFields1 on Human {
          name
          ...HumanFields3
        }
        fragment HumanFields2 on Human {
          name
        }
        fragment HumanFields3 on Human {
          name
        }
      """)

    "contains unknown fragments" in expectFails(
      """
        query Foo {
          human(id: 4) {
            ...HumanFields1
          }
        }
        query Bar {
          human(id: 4) {
            ...HumanFields2
          }
        }
        fragment HumanFields1 on Human {
          name
          ...HumanFields3
        }
        fragment HumanFields2 on Human {
          name
        }
        fragment HumanFields3 on Human {
          name
        }
        fragment Unused1 on Human {
          name
        }
        fragment Unused2 on Human {
          name
        }
      """,
      List(
        "Fragment 'Unused1' is not used." → Some(Pos(22, 9)),
        "Fragment 'Unused2' is not used." → Some(Pos(25, 9))
      ))

    "contains unknown fragments with ref cycle" in expectFails(
      """
        query Foo {
          human(id: 4) {
            ...HumanFields1
          }
        }
        query Bar {
          human(id: 4) {
            ...HumanFields2
          }
        }
        fragment HumanFields1 on Human {
          name
          ...HumanFields3
        }
        fragment HumanFields2 on Human {
          name
        }
        fragment HumanFields3 on Human {
          name
        }
        fragment Unused1 on Human {
          name
          ...Unused2
        }
        fragment Unused2 on Human {
          name
          ...Unused1
        }
      """,
      List(
        "Fragment 'Unused1' is not used." → Some(Pos(22, 9)),
        "Fragment 'Unused2' is not used." → Some(Pos(26, 9))
      ))

    "contains unknown and undef fragments" in expectFails(
      """
        query Foo {
          human(id: 4) {
            ...bar
          }
        }
        fragment foo on Human {
          name
        }
      """,
      List(
        "Fragment 'foo' is not used." → Some(Pos(7, 9))
      ))
  }
}
