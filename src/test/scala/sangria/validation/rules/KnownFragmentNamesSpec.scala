package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class KnownFragmentNamesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new KnownFragmentNames)

  "Validate: Known fragment names" should {
    "known fragment names are valid" in expectPasses(
      """
        {
          human(id: 4) {
            ...HumanFields1
            ... on Human {
              ...HumanFields2
            }
            ... {
              name
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

    "unknown fragment names are invalid" in expectFails(
      """
        {
          human(id: 4) {
            ...UnknownFragment1
            ... on Human {
              ...UnknownFragment2
            }
          }
        }
        fragment HumanFields on Human {
          name
          ...UnknownFragment3
        }
      """,
      List(
        "Unknown fragment 'UnknownFragment1'." → Some(Pos(4, 13)),
        "Unknown fragment 'UnknownFragment2'." → Some(Pos(6, 15)),
        "Unknown fragment 'UnknownFragment3'." → Some(Pos(12, 11))
      ))
  }
}
