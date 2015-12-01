package sangria.integration.json4s

import org.json4s.JsonAST.{JObject, JValue}
import org.json4s.native.JsonMethods.{render ⇒ jsonRender, pretty, compact}
import sangria.marshalling.{InputUnmarshaller, ToInput}

object native extends Json4sNativeSupportLowPrioImplicits {
  implicit object Json4sNativeResultMarshaller extends Json4sResultMarshaller {
    def renderCompact(node: JValue) =  compact(jsonRender(node))
    def renderPretty(node: JValue) = pretty(jsonRender(node))
  }

  implicit object Json4sNativeInputUnmarshaller extends Json4sInputUnmarshaller {
    def render(node: JValue) = compact(jsonRender(node))
  }

  private object Json4sNativeToInput extends ToInput[JValue, JValue] {
    def toInput(value: JValue) = (value, Json4sNativeInputUnmarshaller)
  }

  implicit def json4sNativeToInput[T <: JValue]: ToInput[T, JValue] =
    Json4sNativeToInput.asInstanceOf[ToInput[T, JValue]]
}

trait Json4sNativeSupportLowPrioImplicits {
  implicit val Json4sNativeInputUnmarshallerJObject =
    native.Json4sNativeInputUnmarshaller.asInstanceOf[InputUnmarshaller[JObject]]
}
