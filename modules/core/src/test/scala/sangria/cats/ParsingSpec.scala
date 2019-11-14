package sangria.cats

import org.scalatest.{Matchers, WordSpec}
import sangria.util.CatsSupport

class ParsingSpec extends WordSpec with Matchers with CatsSupport {

  generateTests("parsing")

}
