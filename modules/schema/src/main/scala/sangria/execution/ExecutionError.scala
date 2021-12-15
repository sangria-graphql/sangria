package sangria.execution

import sangria.ast.{AstLocation, SourceMapper}
import sangria.marshalling.ResultMarshaller
import sangria.validation.{AstNodeLocation, Violation}

trait UserFacingError {
  def getMessage(): String
}

trait WithViolations extends UserFacingError {
  def violations: Vector[Violation]
}

trait ErrorWithResolver {
  this: Throwable =>

  def exceptionHandler: ExceptionHandler

  def resolveError(implicit marshaller: ResultMarshaller): marshaller.Node =
    new ResultResolver(marshaller, exceptionHandler, false)
      .resolveError(this)
      .asInstanceOf[marshaller.Node]
}

class ExecutionError(
  message: String,
  val exceptionHandler: ExceptionHandler,
  val sourceMapper: Option[SourceMapper] = None,
  val locations: List[AstLocation] = Nil)
  extends Exception(message)
    with AstNodeLocation
    with UserFacingError
    with ErrorWithResolver {
  override def simpleErrorMessage = super.getMessage
  override def getMessage() = super.getMessage + astLocation
}

trait QueryAnalysisError extends ErrorWithResolver {
  this: Throwable =>
}

case class VariableCoercionError(violations: Vector[Violation], eh: ExceptionHandler)
  extends ExecutionError(
    s"Error during variable coercion. Violations:\n\n${violations.map(_.errorMessage).mkString("\n\n")}",
    eh)
    with WithViolations
    with QueryAnalysisError

case class AttributeCoercionError(violations: Vector[Violation], eh: ExceptionHandler)
  extends ExecutionError(
    s"Error during attribute coercion. Violations:\n\n${violations.map(_.errorMessage).mkString("\n\n")}",
    eh)
    with WithViolations
    with QueryAnalysisError

case class QueryReducingError(cause: Throwable, exceptionHandler: ExceptionHandler)
  extends Exception(s"Query reducing error: ${cause.getMessage}", cause)
    with QueryAnalysisError
