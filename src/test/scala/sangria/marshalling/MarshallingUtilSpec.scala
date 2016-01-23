package sangria.marshalling

import org.scalatest.{Matchers, WordSpec}
import sangria.marshalling.queryAst._
import sangria.marshalling.sprayJson._
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

      val out = in.convertMarshaled[JsValue]

      out should be (
        JsObject(
          "id" → JsNumber(1),
          "name" → JsString("door"),
          "items" → JsArray(Vector(
            JsObject(
              "state" → JsString("Open"),
              "durability" → JsNumber(BigDecimal("0.1465645654675762354763254763343243242"))),
            JsNull,
            JsObject(
              "state" → JsString("Open"),
              "durability" → JsNumber(BigDecimal("0.5")),
              "foo" → JsNull)))))
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
