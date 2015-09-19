package sangria.integration

import org.scalatest.{Matchers, WordSpec}
import play.api.libs.json._
import sangria.execution.Executor
import sangria.integration.PlayJsonSupport.PlayJsonResultMarshaller
import sangria.parser.QueryParser
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class PlayJsonSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "Json4sSupport" should {
    "Marshal and Unmarshal" in {
      import sangria.integration.PlayJsonSupport._

      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = JsObject(Seq("someId" -> JsString("1000")))

      val result = Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, variables = args).await

      Json.prettyPrint(result).replace("\r", "") should be (
        """{
          |  "data" : {
          |    "human" : {
          |      "name" : "Luke Skywalker"
          |    }
          |  }
          |}""".stripMargin)
    }
    
    "correctly serialize" in {
      PlayJsonResultMarshaller.addArrayNodeElem(JsArray(List(JsString("aa"))), JsString("bb")) should be (
        JsArray(List(JsString("aa"), JsString("bb"))))

      PlayJsonResultMarshaller.emptyArrayNode should be (JsArray(Nil))
      PlayJsonResultMarshaller.isEmptyArrayNode(JsArray(Nil)) should be (true)
      PlayJsonResultMarshaller.isEmptyArrayNode(JsArray(JsString("aa") :: Nil)) should be (false)
      PlayJsonResultMarshaller.arrayNode(JsString("aa") :: JsNumber(11) :: Nil) should be (JsArray(JsString("aa") :: JsNumber(11) :: Nil))

      PlayJsonResultMarshaller.booleanNode(true) should be (JsBoolean(true))
      PlayJsonResultMarshaller.intNode(111) should be (JsNumber(111))
      PlayJsonResultMarshaller.floatNode(111.22D) should be (JsNumber(111.22D))
      PlayJsonResultMarshaller.stringNode("qq") should be (JsString("qq"))
      PlayJsonResultMarshaller.bigIntNode(BigInt("12323432432432")) should be (JsNumber(BigDecimal("12323432432432")))
      PlayJsonResultMarshaller.bigDecimalNode(BigDecimal("12323432432432.2435454354543")) should be (JsNumber(BigDecimal("12323432432432.2435454354543")))

      PlayJsonResultMarshaller.emptyMapNode should be (JsObject(Nil))
      PlayJsonResultMarshaller.addMapNodeElem(JsObject(Seq("aa" -> JsString("bb"))), "cc", JsNumber(321)) should be (
        JsObject(Seq("aa" -> JsString("bb"), "cc" -> JsNumber(321))))
      PlayJsonResultMarshaller.mapNode("aa" -> JsString("bb") :: "cc" -> JsNumber(321) :: Nil) should be (
        JsObject(Seq("aa" -> JsString("bb"), "cc" -> JsNumber(321))))

      PlayJsonResultMarshaller.nullNode should be (JsNull)
    }
  }
}
