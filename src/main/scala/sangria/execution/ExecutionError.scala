package sangria.execution

import sangria.validation.Violation

trait WithViolations {
  def violations: List[Violation]
}

class ExecutionError(message: String) extends Exception(message)

case class VariableCoercionError(violations: List[Violation]) extends ExecutionError(
  s"Error during variable coercion. Violations: ${violations map (_.errorMessage) mkString " "}") with WithViolations
