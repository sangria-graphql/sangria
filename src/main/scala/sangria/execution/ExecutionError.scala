package sangria.execution

import org.parboiled2.Position
import sangria.parser.SourceMapper
import sangria.validation.{AstNodeLocation, Violation}

trait WithViolations {
  def violations: List[Violation]
}

trait UserFacingError {
  def getMessage: String
}

class ExecutionError(message: String, val sourceMapper: Option[SourceMapper] = None, val position: Option[Position] = None) extends Exception(message) with AstNodeLocation with UserFacingError {
  override def getMessage = super.getMessage + astLocation
}

case class VariableCoercionError(violations: List[Violation]) extends ExecutionError(
  s"Error during variable coercion. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}") with WithViolations

case class AttributeCoercionError(violations: List[Violation]) extends ExecutionError(
  s"Error during attribute coercion. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}") with WithViolations
