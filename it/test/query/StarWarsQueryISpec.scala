package query

import controllers.TestExecutorController
import org.scalatestplus.play.PlaySpec
import play.api.mvc.{Result, Results}
import play.api.test.Helpers.{contentAsString, defaultAwaitTimeout}
import play.api.test.{FakeRequest, Helpers}

import scala.concurrent.Future

/*
  Similar to the tests we have within => StarWarsQuerySpec but via controller
 */
class StarWarsQuerySpecISpec extends PlaySpec with Results {
//  extends AnyFreeSpec
//    with Matchers
//    with ScalaFutures
//    with IntegrationPatience
//    with GuiceOneAppPerSuite
//    with FutureResultSupport{

  // TODO: implement test controller which will call executor within

  "Evaluate Executor within controller" should {
    "expect correct response" in {
      val controller             = new TestExecutorController(Helpers.stubControllerComponents())
      val result: Future[Result] = controller.index().apply(FakeRequest())
      val bodyText: String       = contentAsString(result)
      bodyText mustBe "{\"data\":{\"hero\":{\"name\":\"R2-D2\"}}}"
    }
  }

}