package sangria.integration

import org.json4s.JsonAST._
import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.starWars.TestData.{FriendsResolver, CharacterRepo}
import sangria.starWars.TestSchema._
import sangria.util.AwaitSupport

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class Json4sNativeSupportSpec extends WordSpec with Matchers with AwaitSupport {
  "Json4sNativeSupport" should {
    "Marshal and Unmarshal" in {
      import json4s.native._
      import org.json4s.native.JsonMethods._

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
      import sangria.integration.json4s.native.Json4sNativeResultMarshaller

      Json4sNativeResultMarshaller.arrayNode(Vector(JString("aa"), JInt(11))) should be (JArray(JString("aa") :: JInt(11) :: Nil))

      Json4sNativeResultMarshaller.booleanNode(true) should be (JBool(true))
      Json4sNativeResultMarshaller.intNode(111) should be (JInt(111))
      Json4sNativeResultMarshaller.floatNode(111.22D) should be (JDouble(111.22D))
      Json4sNativeResultMarshaller.stringNode("qq") should be (JString("qq"))
      Json4sNativeResultMarshaller.bigIntNode(BigInt("12323432432432")) should be (JInt(BigInt("12323432432432")))
      Json4sNativeResultMarshaller.bigDecimalNode(BigDecimal("12323432432432.2435454354543")) should be (JDecimal(BigDecimal("12323432432432.2435454354543")))

      Json4sNativeResultMarshaller.emptyMapNode should be (JObject(Nil))
      Json4sNativeResultMarshaller.addMapNodeElem(JObject("aa" → JString("bb")), "cc", JInt(321), false) should be (
        JObject("aa" → JString("bb"), "cc" → JInt(321)))
      Json4sNativeResultMarshaller.mapNode("aa" → JString("bb") :: "cc" → JInt(321) :: Nil) should be (
        JObject("aa" → JString("bb"), "cc" → JInt(321)))

      Json4sNativeResultMarshaller.nullNode should be (JNull)
    }
  }
}
