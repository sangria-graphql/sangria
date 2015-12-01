package sangria.execution

import org.parboiled2.Position
import sangria.parser.SourceMapper
import sangria.validation.{AstNodeLocation, Violation}

trait UserFacingError {
  def getMessage: String
}

trait WithViolations extends UserFacingError{
  def violations: List[Violation]
}

class ExecutionError(message: String, val sourceMapper: Option[SourceMapper] = None, val positions: List[Position] = Nil) extends Exception(message) with AstNodeLocation with UserFacingError {
  override def simpleErrorMessage = super.getMessage
  override def getMessage = super.getMessage + astLocation
}

case class VariableCoercionError(violations: List[Violation]) extends ExecutionError(
  s"Error during variable coercion. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}") with WithViolations

case class ValidationError(violations: List[Violation]) extends ExecutionError(
  s"Query does not pass validation. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}") with WithViolations

case class AttributeCoercionError(violations: List[Violation]) extends ExecutionError(
  s"Error during attribute coercion. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}") with WithViolations
