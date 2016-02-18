package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class UniqueVariableNamesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new UniqueVariableNames)

  "Validate: Unique variable names" should {
    "unique variable names" in expectPasses(
      """
        query A($x: Int, $y: String) { __typename }
        query B($x: String, $y: Int) { __typename }
      """)

    "duplicate variable names" in expectFailsPosList(
      """
        query A($x: Int, $x: Int, $x: String) { __typename }
        query B($x: String, $x: Int) { __typename }
        query C($x: Int, $x: Int) { __typename }
      """,
      List(
        "There can be only one variable named 'x'." → List(Pos(2, 17), Pos(2, 26)),
        "There can be only one variable named 'x'." → List(Pos(2, 17), Pos(2, 35)),
        "There can be only one variable named 'x'." → List(Pos(3, 17), Pos(3, 29)),
        "There can be only one variable named 'x'." → List(Pos(4, 17), Pos(4, 26))
      ))
  }
}
