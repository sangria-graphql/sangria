package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class ProvidedNonNullArgumentsSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new ProvidedNonNullArguments)

  "Validate: Provided required arguments" should {
    "ignores unknown arguments" in expectPasses(
      """
        {
          dog {
            isHousetrained(unknownArgument: true)
          }
        }
      """)

    "Valid non-nullable value" should {
      "Arg on optional arg" in expectPasses(
        """
          {
            dog {
              isHousetrained(atOtherHomes: true)
            }
          }
        """)

      "No Arg on optional arg" in expectPasses(
        """
          {
            dog {
              isHousetrained
            }
          }
        """)

      "Multiple args" in expectPasses(
        """
          {
            complicatedArgs {
              multipleReqs(req1: 1, req2: 2)
            }
          }
        """)

      "Multiple args reverse order" in expectPasses(
        """
          {
            complicatedArgs {
              multipleReqs(req2: 2, req1: 1)
            }
          }
        """)

      "No args on multiple optional" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOpts(opt1: 1)
            }
          }
        """)

      "Second arg on multiple optional" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOpts(opt2: 1)
            }
          }
        """)

      "Multiple reqs on mixedList" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOptAndReq(req1: 3, req2: 4)
            }
          }
        """)

      "Multiple reqs and one opt on mixedList" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOptAndReq(req1: 3, req2: 4, opt1: 5)
            }
          }
        """)

      "All reqs and opts on mixedList" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOptAndReq(req1: 3, req2: 4, opt1: 5, opt2: 6)
            }
          }
        """)
    }

    "Invalid non-nullable value" should {
      "Missing one non-nullable argument" in expectFails(
        """
          {
            complicatedArgs {
              multipleReqs(req2: 2)
            }
          }
        """,
        List(
          "Field 'multipleReqs' argument 'req1' of type 'Int!' is required but not provided." → Some(Pos(4, 15))
        ))

      "Missing multiple non-nullable arguments" in expectFails(
        """
          {
            complicatedArgs {
              multipleReqs
            }
          }
        """,
        List(
          "Field 'multipleReqs' argument 'req1' of type 'Int!' is required but not provided." → Some(Pos(4, 15)),
          "Field 'multipleReqs' argument 'req2' of type 'Int!' is required but not provided." → Some(Pos(4, 15))
        ))

      "Incorrect value and missing argument" in expectFails(
        """
          {
            complicatedArgs {
              multipleReqs(req1: "one")
            }
          }
        """,
        List(
          "Field 'multipleReqs' argument 'req2' of type 'Int!' is required but not provided." → Some(Pos(4, 15))
        ))
    }

    "Directive arguments" should {
      "ignores unknown directives" in expectPasses(
        """
          {
            dog @unknown
          }
        """)

      "with directives of valid types" in expectPasses(
        """
          {
            dog @include(if: true) {
              name
            }
            human @skip(if: false) {
              name
            }
          }
        """)

      "with directive with missing types" in expectFails(
        """
          {
            dog @include {
              name @skip
            }
          }
        """,
        List(
          "Field 'include' argument 'if' of type 'Boolean!' is required but not provided." → Some(Pos(3, 17)),
          "Field 'skip' argument 'if' of type 'Boolean!' is required but not provided." → Some(Pos(4, 20))
        ))
    }
  }
}
