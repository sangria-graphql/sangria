package sangria.execution

import org.parboiled2.Position
import sangria.marshalling.ResultMarshaller
import sangria.parser.SourceMapper
import sangria.validation.{AstNodeLocation, Violation}

trait UserFacingError {
  def getMessage(): String
}

trait WithViolations extends UserFacingError{
  def violations: Vector[Violation]
}

trait ErrorWithResolver {
  this: Throwable ⇒

  def exceptionHandler: Executor.ExceptionHandler

  def resolveError(implicit marshaller: ResultMarshaller): marshaller.Node =
    new ResultResolver(marshaller, exceptionHandler, false).resolveError(this).asInstanceOf[marshaller.Node]
}

class ExecutionError(message: String, val exceptionHandler: Executor.ExceptionHandler, val sourceMapper: Option[SourceMapper] = None, val positions: List[Position] = Nil) extends Exception(message) with AstNodeLocation with UserFacingError with ErrorWithResolver {
  override def simpleErrorMessage = super.getMessage
  override def getMessage() = super.getMessage + astLocation
}

case class MaxQueryDepthReachedError(maxDepth: Int) extends Exception(s"Max query depth $maxDepth is reached.") with UserFacingError

trait QueryAnalysisError extends ErrorWithResolver {
  this: Throwable ⇒
}

case class VariableCoercionError(violations: Vector[Violation], eh: Executor.ExceptionHandler) extends ExecutionError(
  s"Error during variable coercion. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError

case class AttributeCoercionError(violations: Vector[Violation], eh: Executor.ExceptionHandler) extends ExecutionError(
  s"Error during attribute coercion. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError

case class ValidationError(violations: Vector[Violation], eh: Executor.ExceptionHandler) extends ExecutionError(
  s"Query does not pass validation. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError

case class QueryReducingError(cause: Throwable, exceptionHandler: Executor.ExceptionHandler) extends Exception(s"Query reducing error: ${cause.getMessage}", cause) with QueryAnalysisError

case class OperationSelectionError(message: String, eh: Executor.ExceptionHandler, sm: Option[SourceMapper] = None, pos: List[Position] = Nil)
  extends ExecutionError(message, eh, sm, pos) with QueryAnalysisError
