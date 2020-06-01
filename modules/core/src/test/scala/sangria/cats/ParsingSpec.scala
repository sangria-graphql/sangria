package sangria.cats

import sangria.util.CatsSupport
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ParsingSpec extends AnyWordSpec with Matchers with CatsSupport {

  generateTests("parsing")

}
