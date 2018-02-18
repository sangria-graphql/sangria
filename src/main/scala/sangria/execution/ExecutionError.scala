package sangria.execution

import sangria.ast.AstLocation
import sangria.marshalling.ResultMarshaller
import sangria.parser.SourceMapper
import sangria.schema.{AbstractType, InterfaceType, ObjectType, UnionType}
import sangria.validation.{AstNodeLocation, Violation}

trait UserFacingError {
  def getMessage(): String
}

trait WithViolations extends UserFacingError{
  def violations: Vector[Violation]
}

trait ErrorWithResolver {
  this: Throwable ⇒

  def exceptionHandler: ExceptionHandler

  def resolveError(implicit marshaller: ResultMarshaller): marshaller.Node =
    new ResultResolver(marshaller, exceptionHandler, false).resolveError(this).asInstanceOf[marshaller.Node]
}

class ExecutionError(message: String, val exceptionHandler: ExceptionHandler, val sourceMapper: Option[SourceMapper] = None, val locations: List[AstLocation] = Nil) extends Exception(message) with AstNodeLocation with UserFacingError with ErrorWithResolver {
  override def simpleErrorMessage = super.getMessage
  override def getMessage() = super.getMessage + astLocation
}

abstract class InternalExecutionError(message: String) extends Exception(message) with AstNodeLocation with ErrorWithResolver {
  override def simpleErrorMessage = super.getMessage
  override def getMessage() = super.getMessage + astLocation
}

case class UndefinedConcreteTypeError(path: ExecutionPath, abstractType: AbstractType, possibleTypes: Vector[ObjectType[_, _]], value: Any, exceptionHandler: ExceptionHandler, sourceMapper: Option[SourceMapper] = None, locations: List[AstLocation] = Nil)
  extends InternalExecutionError(s"Can't find appropriate subtype of ${UndefinedConcreteTypeError.renderAbstractType(abstractType)} type '${abstractType.name}' for value of class '${UndefinedConcreteTypeError.renderValueClass(value)}' at path '$path'. Possible types: ${UndefinedConcreteTypeError.renderPossibleTypes(possibleTypes)}. Got value: $value.")

object UndefinedConcreteTypeError {
  private def renderAbstractType(abstractType: AbstractType) = abstractType match {
    case _: UnionType[_] ⇒ "a union"
    case _: InterfaceType[_, _] ⇒ "an interface"
  }

  private def renderPossibleTypes(possibleTypes: Vector[ObjectType[_, _]]) =
    if (possibleTypes.isEmpty) "none"
    else possibleTypes.map(pt ⇒ s"${pt.name} (defined for '${pt.valClass.getName}')") mkString ", "

  private def renderValueClass(value: Any) = value.getClass.getName
}

case class MaxQueryDepthReachedError(maxDepth: Int) extends Exception(s"Max query depth $maxDepth is reached.") with UserFacingError

case object IntrospectionNotAllowedError extends Exception(s"Introspection is not allowed.") with UserFacingError

trait QueryAnalysisError extends ErrorWithResolver {
  this: Throwable ⇒
}

case class VariableCoercionError(violations: Vector[Violation], eh: ExceptionHandler) extends ExecutionError(
  s"Error during variable coercion. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError

case class AttributeCoercionError(violations: Vector[Violation], eh: ExceptionHandler) extends ExecutionError(
  s"Error during attribute coercion. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError

case class ValidationError(violations: Vector[Violation], eh: ExceptionHandler) extends ExecutionError(
  s"Query does not pass validation. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError

case class InputDocumentMaterializationError(violations: Vector[Violation], eh: ExceptionHandler) extends ExecutionError(
  s"Input document does not pass validation. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError

case class MaterializedSchemaValidationError(violations: Vector[Violation], eh: ExceptionHandler = ExceptionHandler.empty) extends ExecutionError(
  s"Materialized schema does not pass validation. Violations:\n\n${violations map (_.errorMessage) mkString "\n\n"}", eh) with WithViolations with QueryAnalysisError

case class QueryReducingError(cause: Throwable, exceptionHandler: ExceptionHandler) extends Exception(s"Query reducing error: ${cause.getMessage}", cause) with QueryAnalysisError

case class OperationSelectionError(message: String, eh: ExceptionHandler, sm: Option[SourceMapper] = None, pos: List[AstLocation] = Nil)
  extends ExecutionError(message, eh, sm, pos) with QueryAnalysisError
