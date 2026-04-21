package query

import controllers.TestEnvironment
import org.scalatest.concurrent.ScalaFutures
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import spray.json._
import DefaultJsonProtocol._

import scala.concurrent.Future

class StarWarsQuerySpecISpec extends AnyWordSpec
  with ScalaFutures
  with Matchers {

  type ExecutorResult = Map[String, Map[String, Map[String, String]]]

  "Evaluate Executor within controller" when {
    "expect correct response" should {
      "as json" in {
        val env = new TestEnvironment()
        val result: Future[String] = env.execute()
        val jsonAst = result.futureValue.parseJson
        val obj = jsonAst.convertTo[ExecutorResult]
        assert(obj == Map("data" -> Map("hero" -> Map("name" -> "R2-D2"))))
      }

    }
  }

}
