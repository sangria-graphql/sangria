package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class KnownArgumentNamesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new KnownArgumentNames)

  "Validate: Known argument names" should {
    "single arg is known" in expectPasses(
      """
        fragment argOnRequiredArg on Dog {
          doesKnowCommand(dogCommand: SIT)
        }
      """)

    "multiple args are known" in expectPasses(
      """
        fragment multipleArgs on ComplicatedArgs {
          multipleReqs(req1: 1, req2: 2)
        }
      """)

    "ignores args of unknown fields" in expectPasses(
      """
        fragment argOnUnknownField on Dog {
          unknownField(unknownArg: SIT)
        }
      """)

    "multiple args in reverse order are known" in expectPasses(
      """
        fragment multipleArgsReverseOrder on ComplicatedArgs {
          multipleReqs(req2: 2, req1: 1)
        }
      """)

    "no args on optional arg" in expectPasses(
      """
        fragment noArgOnOptionalArg on Dog {
          isHousetrained
        }
      """)

    "args are known deeply" in expectPasses(
      """
        {
          dog {
            doesKnowCommand(dogCommand: SIT)
          }
          human {
            pet {
              ... on Dog {
                doesKnowCommand(dogCommand: SIT)
              }
            }
          }
        }
      """)

    "directive args are known" in expectPasses(
      """
        {
          dog @skip(if: true)
        }
      """)

    "undirective args are invalid" in expectFails(
      """
        {
          dog @skip(unless: true)
        }
      """,
      List(
        "Unknown argument 'unless' on directive 'skip'." → Some(Pos(3, 21))
      ))

    "invalid arg name" in expectFails(
      """
        fragment invalidArgName on Dog {
          doesKnowCommand(unknown: true)
        }
      """,
      List(
        "Unknown argument 'unknown' on field 'doesKnowCommand' of type 'Dog'." → Some(Pos(3, 27))
      ))

    "unknown args amongst known args" in expectFails(
      """
        fragment oneGoodArgOneInvalidArg on Dog {
          doesKnowCommand(whoknows: 1, dogCommand: SIT, unknown: true)
        }
      """,
      List(
        "Unknown argument 'whoknows' on field 'doesKnowCommand' of type 'Dog'." → Some(Pos(3, 27)),
        "Unknown argument 'unknown' on field 'doesKnowCommand' of type 'Dog'." → Some(Pos(3, 57))
      ))

    "unknown args deeply" in expectFails(
      """
        {
          dog {
            doesKnowCommand(unknown: true)
          }
          human {
            pet {
              ... on Dog {
                doesKnowCommand(unknown: true)
              }
            }
          }
        }
      """,
      List(
        "Unknown argument 'unknown' on field 'doesKnowCommand' of type 'Dog'." → Some(Pos(4, 29)),
        "Unknown argument 'unknown' on field 'doesKnowCommand' of type 'Dog'." → Some(Pos(9, 33))
      ))
  }
}
