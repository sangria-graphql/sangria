package sangria.integration.json4s

import org.json4s.JsonAST.{JObject, JValue}

import org.json4s.jackson.JsonMethods.{render ⇒ jsonRender, pretty, compact}
import sangria.marshalling.{FromInput, InputUnmarshaller, ToInput}

object jackson extends Json4sJacksonSupportLowPrioImplicits {
  implicit object Json4sJacksonResultMarshaller extends Json4sResultMarshaller {
    def renderCompact(node: JValue) =  compact(jsonRender(node))
    def renderPretty(node: JValue) = pretty(jsonRender(node))
  }

  implicit object Json4sJacksonInputUnmarshaller extends Json4sInputUnmarshaller {
    def render(node: JValue) = compact(jsonRender(node))
  }

  private object Json4sJacksonToInput extends ToInput[JValue, JValue] {
    def toInput(value: JValue) = (value, Json4sJacksonInputUnmarshaller)
  }

  implicit def json4sJacksonToInput[T <: JValue]: ToInput[T, JValue] =
    Json4sJacksonToInput.asInstanceOf[ToInput[T, JValue]]

  private object Json4sJacksonFromInput extends FromInput[JValue] {
    val marshaller = Json4sJacksonResultMarshaller
    def fromResult(node: marshaller.Node) = node
  }

  implicit def json4sJacksonFromInput[T <: JValue]: FromInput[T] =
    Json4sJacksonFromInput.asInstanceOf[FromInput[T]]
}

trait Json4sJacksonSupportLowPrioImplicits {
  implicit val Json4sJacksonInputUnmarshallerJObject =
    jackson.Json4sJacksonInputUnmarshaller.asInstanceOf[InputUnmarshaller[JObject]]
}
