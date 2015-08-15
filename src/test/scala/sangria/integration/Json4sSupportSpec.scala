package sangria.integration

import org.json4s.JsonAST._
import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.integration.Json4sSupport.Json4sResultMarshaller
import sangria.parser.QueryParser
import sangria.starWars.TestData.{FriendsResolver, CharacterRepo}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class Json4sSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "Json4sSupport" should {
    "Marshal and Unmarshal" in {
      import Json4sSupport._
      import org.json4s.native.JsonMethods._

      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = JObject("someId" -> JString("1000"))

      val result = Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, arguments = Some(args)).await

      pretty(render(result)) should be (
        """{
          |  "data":{
          |    "human":{
          |      "name":"Luke Skywalker"
          |    }
          |  }
          |}""".stripMargin)
    }

    "correctly serialize" in {
      Json4sResultMarshaller.addArrayNodeElem(JArray(List(JString("aa"))), JString("bb")) should be (
        JArray(List(JString("aa"), JString("bb"))))

      Json4sResultMarshaller.emptyArrayNode should be (JArray(Nil))
      Json4sResultMarshaller.isEmptyArrayNode(JArray(Nil)) should be (true)
      Json4sResultMarshaller.isEmptyArrayNode(JArray(JString("aa") :: Nil)) should be (false)
      Json4sResultMarshaller.arrayNode(JString("aa") :: JInt(11) :: Nil) should be (JArray(JString("aa") :: JInt(11) :: Nil))

      Json4sResultMarshaller.booleanNode(true) should be (JBool(true))
      Json4sResultMarshaller.intNode(111) should be (JInt(111))
      Json4sResultMarshaller.floatNode(111.22D) should be (JDouble(111.22D))
      Json4sResultMarshaller.stringNode("qq") should be (JString("qq"))
      Json4sResultMarshaller.bigIntNode(BigInt("12323432432432")) should be (JInt(BigInt("12323432432432")))
      Json4sResultMarshaller.bigDecimalNode(BigDecimal("12323432432432.2435454354543")) should be (JDecimal(BigDecimal("12323432432432.2435454354543")))

      Json4sResultMarshaller.emptyMapNode should be (JObject(Nil))
      Json4sResultMarshaller.addMapNodeElem(JObject("aa" -> JString("bb")), "cc", JInt(321)) should be (
        JObject("aa" -> JString("bb"), "cc" -> JInt(321)))
      Json4sResultMarshaller.mapNode("aa" -> JString("bb") :: "cc" -> JInt(321) :: Nil) should be (
        JObject("aa" -> JString("bb"), "cc" -> JInt(321)))

      Json4sResultMarshaller.nullNode should be (JNull)
    }
  }
}
