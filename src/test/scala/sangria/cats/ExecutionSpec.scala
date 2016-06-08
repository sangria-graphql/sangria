package sangria.cats

import org.scalatest.{Matchers, WordSpec}
import sangria.util.CatsSupport

class ExecutionSpec extends WordSpec with Matchers with CatsSupport {

  generateTests("execution")

}
