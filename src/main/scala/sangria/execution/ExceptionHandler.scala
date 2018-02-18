package sangria.execution

import sangria.ast.AstLocation
import sangria.marshalling.ResultMarshaller
import sangria.validation.Violation

case class ExceptionHandler(
  onException: PartialFunction[(ResultMarshaller, Throwable), HandledException] = PartialFunction.empty,
  onViolation: PartialFunction[(ResultMarshaller, Violation), HandledException] = PartialFunction.empty,
  onUserFacingError: PartialFunction[(ResultMarshaller, UserFacingError), HandledException] = PartialFunction.empty)

object ExceptionHandler {
  val empty = ExceptionHandler()
}

sealed trait HandledException

case class SingleHandledException(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty, positions: List[AstLocation] = Nil) extends HandledException
case class MultipleHandledExceptions(messages: Vector[(String, Map[String, ResultMarshaller#Node], List[AstLocation])]) extends HandledException

object HandledException {
  def apply(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty, positions: List[AstLocation] = Nil) =
    SingleHandledException(message, additionalFields, positions)

  def apply(messages: Vector[(String, Map[String, ResultMarshaller#Node], List[AstLocation])]) =
    MultipleHandledExceptions(messages)
}