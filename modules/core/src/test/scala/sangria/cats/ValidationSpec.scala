package sangria.cats

import org.scalatest.{Matchers, WordSpec}
import sangria.util.CatsSupport

class ValidationSpec extends WordSpec with Matchers with CatsSupport {

  generateTests("validation")

}
