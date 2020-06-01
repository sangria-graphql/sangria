package sangria.cats

import sangria.util.CatsSupport
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ExecutionSpec extends AnyWordSpec with Matchers with CatsSupport {

  generateTests("execution")

}
