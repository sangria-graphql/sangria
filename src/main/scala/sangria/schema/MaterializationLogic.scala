package sangria.schema

import sangria.ast
import sangria.execution.{UserFacingError, FieldTag}
import sangria.introspection.TypeKind
import sangria.marshalling.{ToInput, InputParser, InputUnmarshaller, MarshallerCapability}
import sangria.validation.Violation

import scala.util.Try

trait MaterializationLogic[Ctx] {
  def resolveField(ctx: Context[Ctx, _]): Action[Ctx, _]

  def coerceScalarUserInput(scalarName: String, value: Any): Either[Violation, Any]
  def coerceScalarInput(scalarName: String, value: ast.Value): Either[Violation, Any]
  def coerceScalarOutput(scalarName: String, coerced: Any, capabilities: Set[MarshallerCapability]): Any

  def rewriteTypeName(kind: TypeKind.Value, name: String): String

  def fieldTags(fieldName: String): List[FieldTag]

  def defaultValueParser: Option[String ⇒ Try[(Any, InputUnmarshaller[Any])]]
}

object MaterializationLogic {
  def default[Ctx] = new DefaultMaterializationLogic[Ctx]

  def withDefaultValues[Ctx, T : InputUnmarshaller : InputParser] = new DefaultMaterializationLogic[Ctx] {
    override val defaultValueParser =
      Some((raw: String) ⇒ implicitly[InputParser[T]].parse(raw) map (_ → implicitly[InputUnmarshaller[T]].asInstanceOf[InputUnmarshaller[Any]]))
  }
}

class DefaultMaterializationLogic[Ctx] extends MaterializationLogic[Ctx] {
  import DefaultMaterializationLogic._

  def resolveField(ctx: Context[Ctx, _]): Action[Ctx, _] = throw MaterializedSchemaException

  def coerceScalarUserInput(scalarName: String, value: Any): Either[Violation, Any] = Left(MaterializedSchemaViolation)
  def coerceScalarOutput(scalarName: String, coerced: Any, caps: Set[MarshallerCapability]): ast.Value = throw MaterializedSchemaException
  def coerceScalarInput(scalarName: String, value: ast.Value): Either[Violation, Any] = Left(MaterializedSchemaViolation)

  def rewriteTypeName(kind: TypeKind.Value, name: String) = name

  def fieldTags(fieldName: String) = Nil

  /**
    * By default all default values are ignored because there is no knowledge how to parse them
    */
  def defaultValueParser: Option[String ⇒ Try[(Any, InputUnmarshaller[Any])]] = None
}

object DefaultMaterializationLogic {
  case object MaterializedSchemaException extends Exception("Schema was materialized and cannot be used for any queries except introspection queries.") with UserFacingError
  case object MaterializedSchemaViolation extends Violation {
    val errorMessage = "Schema was materialized and cannot be used for any queries except introspection queries."
  }

  case class ConstantToInput[T](iu: InputUnmarshaller[T]) extends ToInput[T, T] {
    def toInput(value: T) = value → iu
  }
}
