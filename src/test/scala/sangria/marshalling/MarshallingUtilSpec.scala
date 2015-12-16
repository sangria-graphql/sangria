package sangria.marshalling

import io.circe.Json
import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import org.json4s.JsonAST._
import sangria.marshalling.queryAst._
import sangria.integration.json4s.native._
import sangria.integration.sprayJson._
import sangria.integration.circe._
import sangria.marshalling.scalaMarshalling._
import sangria.marshalling.MarshallingUtil._
import sangria.macros._
import sangria.util.tag._
import spray.json._

class MarshallingUtilSpec extends WordSpec with Matchers {

  "MarshallingUtil" should {
    "convert query AST to JValue" in {
      val in = graphqlInput"""
        {
          id: 1
          name: "door"
          items: [
            # pretty broken door
            {state: Open, durability: 0.1465645654675762354763254763343243242}
            null
            {state: Open, durability: 0.5, foo: null}
          ]
        }
      """

      val out = in.convertMarshaled[JValue]

      out should be (
        JObject(List(
          "id" → JInt(1),
          "name" → JString("door"),
          "items" → JArray(List(
            JObject(List(
              "state" → JString("Open"),
              "durability" → JDecimal(BigDecimal("0.1465645654675762354763254763343243242")))),
            JNull,
            JObject(List(
              "state" → JString("Open"),
              "durability" → JDecimal(BigDecimal("0.5")),
              "foo" → JNull)))))))
    }

    "convert sprayJson to circe" in {
      val in: JsValue = """
        {
          "id": 1,
          "name": "door",
          "items": [
            {"state": "Open", "durability": 0.1465645654675762354763254763343243242},
            null,
            {"state": "Open", "durability": 0.5, "foo": null}
          ]
        }
      """.parseJson

      val out = in.convertMarshaled[Json]

      out should be (
        Json.obj(
          "id" → Json.int(1),
          "name" → Json.string("door"),
          "items" → Json.array(
            Json.obj(
              "state" → Json.string("Open"),
              "durability" → Json.bigDecimal(BigDecimal("0.1465645654675762354763254763343243242"))),
            Json.empty,
            Json.obj(
              "state" → Json.string("Open"),
              "durability" → Json.bigDecimal(BigDecimal("0.5")),
              "foo" → Json.empty))))
    }

    "convert query AST to scala map" in {
      val in = graphqlInput"""
        {
          id: 1
          name: "door"
          items: [
            # pretty broken door
            {state: Open, durability: 0.1465645654675762354763254763343243242}
            null
            {state: Open, durability: 0.5, foo: null}
          ]
        }
      """

      val out: Any = in.convertMarshaled[Any @@ ScalaInput]

      out should be (
        Map(
          "id" → 1, 
          "name" → "door", 
          "items" → Vector(
            Map(
              "state" → "Open", 
              "durability" → BigDecimal("0.1465645654675762354763254763343243242")),
            null,
            Map(
              "state" → "Open", 
              "durability" → BigDecimal("0.5"),
              "foo" → null))))
    }
  }

}
