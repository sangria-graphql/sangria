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

sealed trait HandledException {
  def addFieldsInExtensions: Boolean
  def addFieldsInError: Boolean
}

case class SingleHandledException(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty, locations: List[AstLocation] = Nil, addFieldsInExtensions: Boolean = true, addFieldsInError: Boolean = false) extends HandledException
case class MultipleHandledExceptions(messages: Vector[(String, Map[String, ResultMarshaller#Node], List[AstLocation])], addFieldsInExtensions: Boolean = true, addFieldsInError: Boolean = false) extends HandledException

object HandledException {
  def apply(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty, positions: List[AstLocation] = Nil, addFieldsInExtensions: Boolean = true, addFieldsInError: Boolean = false) =
    single(message, additionalFields, positions, addFieldsInExtensions, addFieldsInError)

  def single(message: String, additionalFields: Map[String, ResultMarshaller#Node] = Map.empty, positions: List[AstLocation] = Nil, addFieldsInExtensions: Boolean = true, addFieldsInError: Boolean = false) =
    SingleHandledException(message, additionalFields, positions, addFieldsInExtensions, addFieldsInError)

  def multiple(messages: Vector[(String, Map[String, ResultMarshaller#Node], List[AstLocation])], addFieldsInExtensions: Boolean = true, addFieldsInError: Boolean = false) =
    MultipleHandledExceptions(messages, addFieldsInExtensions, addFieldsInError)
}