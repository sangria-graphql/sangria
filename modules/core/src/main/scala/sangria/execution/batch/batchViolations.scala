package sangria.execution.batch

import sangria.ast.AstLocation
import sangria.execution.{ExecutionError, Executor, QueryAnalysisError, WithViolations}
import sangria.parser.SourceMapper
import sangria.validation.{AstNodeViolation, Violation}

case class BatchExecutionError(message: String, eh: Executor.ExceptionHandler)
    extends ExecutionError(message, eh)
    with QueryAnalysisError

case class VariableDefinitionInferenceViolation(
    operationName: String,
    variableName: String,
    type1: String,
    type2: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Inferred variable '$$$variableName' in operation '$operationName' is used with two conflicting types: '$type1' and '$type2'."
}

case class UndefinedVariableDefinitionViolation(
    operationName: String,
    variableName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Variable '$$$variableName' is not defined in the operation '$operationName'."
}

case class CircularOperationDependencyViolation(
    operationName: String,
    path: Vector[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Operation '$operationName' has a circular dependency at path '${path.mkString(" -> ")} -> $operationName'."
}

case class BatchExecutionViolationError(
    violations: Vector[Violation],
    eh: Executor.ExceptionHandler)
    extends ExecutionError(
      s"Invalid batch query. Violations:\n\n${violations.map(_.errorMessage).mkString("\n\n")}",
      eh)
    with QueryAnalysisError
    with WithViolations
