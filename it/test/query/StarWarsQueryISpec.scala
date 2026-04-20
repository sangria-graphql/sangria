package query

import controllers.TestExecutorController
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.freespec.AnyFreeSpec
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import play.api.mvc.{Result, Results}
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout}
import play.api.test.{FakeRequest, Helpers}
import utils.FutureResultSupport

import scala.concurrent.Future

/*
  Similar to the tests we have within => StarWarsQuerySpec but via controller
 */
trait BaseQueryISpec
    extends AnyWordSpec
    with ScalaFutures
    with Matchers
    with IntegrationPatience
    with FutureResultSupport

class StarWarsQuerySpecISpec extends BaseQueryISpec {

  "Evaluate Executor within controller" when {
    "expect correct response" should {
      "as json" in {
        val controller = new TestExecutorController(Helpers.stubControllerComponents())
        val result: Future[Result] = controller.index().apply(FakeRequest())
        val resAsString = contentAsString(result)
        assert(resAsString == "{\"data\":{\"hero\":{\"name\":\"R2-D2\"}}}")
      }

    }
  }

}
