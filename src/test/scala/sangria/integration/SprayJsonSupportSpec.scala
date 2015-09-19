package sangria.integration

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.integration.SprayJsonSupport.SprayJsonResultMarshaller
import sangria.parser.QueryParser
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import spray.json._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class SprayJsonSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "Json4sSupport" should {
    "Marshal and Unmarshal" in {
      import sangria.integration.SprayJsonSupport._

      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = JsObject("someId" -> JsString("1000"))

      val result = Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, variables = args).await

      result.prettyPrint should be (
        """{
          |  "data": {
          |    "human": {
          |      "name": "Luke Skywalker"
          |    }
          |  }
          |}""".stripMargin)
    }
    
    "correctly serialize" in {
      SprayJsonResultMarshaller.addArrayNodeElem(JsArray(Vector(JsString("aa"))), JsString("bb")) should be (
        JsArray(Vector(JsString("aa"), JsString("bb"))))

      SprayJsonResultMarshaller.emptyArrayNode should be (JsArray(Vector.empty))
      SprayJsonResultMarshaller.isEmptyArrayNode(JsArray(Vector.empty)) should be (true)
      SprayJsonResultMarshaller.isEmptyArrayNode(JsArray(Vector(JsString("aa")))) should be (false)
      SprayJsonResultMarshaller.arrayNode(JsString("aa") :: JsNumber(11) :: Nil) should be (JsArray(Vector(JsString("aa"), JsNumber(11))))

      SprayJsonResultMarshaller.booleanNode(true) should be (JsBoolean(true))
      SprayJsonResultMarshaller.intNode(111) should be (JsNumber(111))
      SprayJsonResultMarshaller.floatNode(111.22D) should be (JsNumber(111.22D))
      SprayJsonResultMarshaller.stringNode("qq") should be (JsString("qq"))
      SprayJsonResultMarshaller.bigIntNode(BigInt("12323432432432")) should be (JsNumber(BigDecimal("12323432432432")))
      SprayJsonResultMarshaller.bigDecimalNode(BigDecimal("12323432432432.2435454354543")) should be (JsNumber(BigDecimal("12323432432432.2435454354543")))

      SprayJsonResultMarshaller.emptyMapNode should be (JsObject.empty)
      SprayJsonResultMarshaller.addMapNodeElem(JsObject(Map("aa" -> JsString("bb"))), "cc", JsNumber(321)) should be (
        JsObject(Map("aa" -> JsString("bb"), "cc" -> JsNumber(321))))
      SprayJsonResultMarshaller.mapNode("aa" -> JsString("bb") :: "cc" -> JsNumber(321) :: Nil) should be (
        JsObject(Map("aa" -> JsString("bb"), "cc" -> JsNumber(321))))

      SprayJsonResultMarshaller.nullNode should be (JsNull)
    }
  }
}
