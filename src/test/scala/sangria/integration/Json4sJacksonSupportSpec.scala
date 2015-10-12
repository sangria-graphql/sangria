package sangria.integration

import org.json4s.JsonAST._
import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.starWars.TestData.{CharacterRepo, FriendsResolver}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class Json4sJacksonSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "Json4sJacksonSupport" should {
    "Marshal and Unmarshal" in {
      import org.json4s.jackson.JsonMethods._
      import sangria.integration.json4s.jackson._

      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = JObject("someId" → JString("1000"))

      val result = Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, variables = args).await

      pretty(render(result)).replaceAll("\r\n", "\n") should be (
        """{
          |  "data" : {
          |    "human" : {
          |      "name" : "Luke Skywalker"
          |    }
          |  }
          |}""".stripMargin)
    }

    "correctly serialize" in {
      import sangria.integration.json4s.jackson.Json4sJacksonResultMarshaller

      Json4sJacksonResultMarshaller.addArrayNodeElem(JArray(List(JString("aa"))), JString("bb")) should be (
        JArray(List(JString("aa"), JString("bb"))))

      Json4sJacksonResultMarshaller.emptyArrayNode should be (JArray(Nil))
      Json4sJacksonResultMarshaller.isEmptyArrayNode(JArray(Nil)) should be (true)
      Json4sJacksonResultMarshaller.isEmptyArrayNode(JArray(JString("aa") :: Nil)) should be (false)
      Json4sJacksonResultMarshaller.arrayNode(JString("aa") :: JInt(11) :: Nil) should be (JArray(JString("aa") :: JInt(11) :: Nil))

      Json4sJacksonResultMarshaller.booleanNode(true) should be (JBool(true))
      Json4sJacksonResultMarshaller.intNode(111) should be (JInt(111))
      Json4sJacksonResultMarshaller.floatNode(111.22D) should be (JDouble(111.22D))
      Json4sJacksonResultMarshaller.stringNode("qq") should be (JString("qq"))
      Json4sJacksonResultMarshaller.bigIntNode(BigInt("12323432432432")) should be (JInt(BigInt("12323432432432")))
      Json4sJacksonResultMarshaller.bigDecimalNode(BigDecimal("12323432432432.2435454354543")) should be (JDecimal(BigDecimal("12323432432432.2435454354543")))

      Json4sJacksonResultMarshaller.emptyMapNode should be (JObject(Nil))
      Json4sJacksonResultMarshaller.addMapNodeElem(JObject("aa" → JString("bb")), "cc", JInt(321)) should be (
        JObject("aa" → JString("bb"), "cc" → JInt(321)))
      Json4sJacksonResultMarshaller.mapNode("aa" → JString("bb") :: "cc" → JInt(321) :: Nil) should be (
        JObject("aa" → JString("bb"), "cc" → JInt(321)))

      Json4sJacksonResultMarshaller.nullNode should be (JNull)
    }
  }
}
