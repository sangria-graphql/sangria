package sangria.validation

import org.parboiled2.Position
import sangria.parser.SourceMapper
import sangria.validation.rules.ConflictReason

trait Violation {
  def errorMessage: String
}

abstract class BaseViolation(val errorMessage: String) extends Violation

trait AstNodeLocation {
  def sourceMapper: Option[SourceMapper]
  def positions: List[Position]

  lazy val astLocation = (for {
    sm <- sourceMapper
  } yield positions map(p => s" ${sm.renderLocation(p)}:\n${sm.renderLinePosition(p)}") mkString "\n") getOrElse ""
}

trait AstNodeViolation extends Violation with AstNodeLocation

abstract class ValueCoercionViolation(errorMessage: String) extends BaseViolation(errorMessage)

case object IntCoercionViolation extends ValueCoercionViolation("Int value expected")
case object BigIntCoercionViolation extends ValueCoercionViolation("Value is too big to fit in Int")
case object FloatCoercionViolation extends ValueCoercionViolation("Float or Int value expected")
case object BigDecimalCoercionViolation extends ValueCoercionViolation("Float or Int value is too bit to fit in double")
case object BooleanCoercionViolation extends ValueCoercionViolation("Boolean value expected")
case object StringCoercionViolation extends ValueCoercionViolation("String value expected")
case object IDCoercionViolation extends ValueCoercionViolation("String or Int value expected")

case class EnumValueCoercionViolation(name: String) extends ValueCoercionViolation(s"Enum value '$name' is undefined")
case object EnumCoercionViolation extends ValueCoercionViolation(s"Enum value expected")

case class FieldCoercionViolation(fieldPath: List[String], valueViolation: Violation, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Field '${fieldPath mkString "."}' has wrong value: '${valueViolation.errorMessage}'.$astLocation"
}

case class VarTypeMismatchViolation(definitionName: String, expectedType: String, input: Option[String], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$$$definitionName' expected value of type '$expectedType' but ${input map ("got: " + _) getOrElse "value is undefined"}.$astLocation"
}

case class UnknownVariableTypeViolation(definitionName: String, varType: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$varType' expected value of type '$$$definitionName' which cannot be used as an input type.$astLocation"
}

case class NullValueForNotNullTypeViolation(fieldPath: List[String], typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Null value was provided for the NotNull Type '$typeName' at path '${fieldPath mkString "."}'.$astLocation"
}

case class InputObjectTypeMismatchViolation(fieldPath: List[String], typeName: String, value: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Value '$value' of wrong type was provided to the field of type '$typeName' at path '${fieldPath mkString "."}'.$astLocation"
}

// validation

case class BadValueViolation(argName: String, typeName: String, value: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Argument '$argName' expected type '$typeName' but got: $value.$astLocation"
}

case class BadValueForDefaultArgViolation(varName: String, typeName: String, value: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$$$varName' of type '$typeName' has invalid default value: $value.$astLocation"
}

case class DefaultForNonNullArgViolation(varName: String, typeName: String, guessTypeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$$$varName' of type '$typeName' is required and will never use the default value. Perhaps you meant to use type '$guessTypeName'.$astLocation"
}

case class UndefinedFieldViolation(fieldName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Cannot query field '$fieldName' on '$typeName'.$astLocation"
}

case class InlineFragmentOnNonCompositeErrorViolation(typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Fragment cannot condition on non composite type '$typeName'.$astLocation"
}

case class FragmentOnNonCompositeErrorViolation(fragName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Fragment '$fragName' cannot condition on non composite type '$typeName'.$astLocation"
}

case class UnknownArgViolation(argName: String, fieldName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Unknown argument '$argName' on field '$fieldName' of type '$typeName'.$astLocation"
}

case class UnknownDirectiveArgViolation(argName: String, dirName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Unknown argument '$argName' on directive '$dirName'.$astLocation"
}

case class UnknownDirectiveViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Unknown directive '$name'.$astLocation"
}

case class MisplacedDirectiveViolation(name: String, placement: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Directive '$name' may not be used on $placement.$astLocation"
}

case class UnknownFragmentViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Unknown fragment '$name'.$astLocation"
}

case class UnknownTypeViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Unknown type '$name'.$astLocation"
}

case class CycleErrorViolation(fragmentName: String, spreadNames: List[String], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Cannot spread fragment '$fragmentName' within itself${if (spreadNames.nonEmpty) s" via ${spreadNames map ("'" + _ + "'") mkString ", " }" else ""}.$astLocation"
}

case class UndefinedVarByOpViolation(varName: String, operationName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$$$varName' is not defined by operation '$operationName'.$astLocation"
}

case class UndefinedVarViolation(varName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$$$varName' is not defined.$astLocation"
}

case class UnusedFragmentViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Fragment '$name' is not used.$astLocation"
}

case class UnusedVariableViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$$$name' is not used.$astLocation"
}

case class NoSubselectionAllowedViolation(fieldName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Field '$fieldName' of type '$typeName' must not have a sub selection.$astLocation"
}

case class RequiredSubselectionViolation(fieldName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Field '$fieldName' of type '$typeName' must have a sub selection.$astLocation"
}

case class TypeIncompatibleAnonSpreadViolation(parentTypeName: String, fragTypeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Fragment cannot be spread here as objects of type '$parentTypeName' can never be of type '$fragTypeName'.$astLocation"
}

case class TypeIncompatibleSpreadViolation(fragName: String, parentTypeName: String, fragTypeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Fragment '$fragName' cannot be spread here as objects of type '$parentTypeName' can never be of type '$fragTypeName'.$astLocation"
}

case class NonInputTypeOnVarViolation(varName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$$$varName' cannot be non input type '$typeName'.$astLocation"
}

case class BadVarPositionViolation(varName: String, varType: String, expectedType: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$$$varName' of type '$varType' used in position expecting type '$expectedType'.$astLocation"
}

case class MissingFieldArgViolation(fieldName: String, argName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Field '$fieldName' argument '$argName' of type '$typeName' is required but not provided.$astLocation"
}

case class FieldsConflictViolation(outputName: String, reason: Either[String, Vector[ConflictReason]], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Field '$outputName' conflict because ${reasonMessage(reason)}.$astLocation"

  private def reasonMessage(reason: Either[String, Vector[ConflictReason]]): String = reason match {
    case Left(message) => message
    case Right(subReasons) => subReasons map (sr => s"subfields '${sr.fieldName}' conflict because ${reasonMessage(sr.reason)}") mkString " and "
  }
}

case class AnonOperationNotAloneViolation(sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"This anonymous operation must be the only defined operation.$astLocation"
}
