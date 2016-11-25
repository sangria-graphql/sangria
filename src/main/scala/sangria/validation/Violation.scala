package sangria.validation

import org.parboiled2.Position
import sangria.parser.SourceMapper
import sangria.util.StringUtil
import sangria.validation.rules.ConflictReason

trait Violation {
  def errorMessage: String
}

object Violation {
  def didYouMean(suggestions: Seq[String], text: String = "Did you mean"): String =
    if (suggestions.nonEmpty)
      s" $text ${StringUtil.quotedOrList(suggestions)}?"
    else
      ""
}

abstract class BaseViolation(val errorMessage: String) extends Violation

trait AstNodeLocation {
  def sourceMapper: Option[SourceMapper]
  def positions: List[Position]
  def simpleErrorMessage: String

  lazy val astLocation = (for {
    sm ← sourceMapper
  } yield positions map(p ⇒ s" ${sm.renderLocation(p)}:\n${sm.renderLinePosition(p)}") mkString "\n") getOrElse ""

  final def errorMessage = simpleErrorMessage + astLocation
}

trait AstNodeViolation extends Violation with AstNodeLocation

abstract class ValueCoercionViolation(errorMessage: String) extends BaseViolation(errorMessage)

case object IntCoercionViolation extends ValueCoercionViolation("Int value expected")
case object BigIntCoercionViolation extends ValueCoercionViolation("Value is too big to fit in Int")

case object LongCoercionViolation extends ValueCoercionViolation("Long value expected")
case object BigLongCoercionViolation extends ValueCoercionViolation("Value is too big to fit in Long")

case object FloatCoercionViolation extends ValueCoercionViolation("Float or Int value expected")
case object BigDecimalCoercionViolation extends ValueCoercionViolation("Float or Int value is too bit to fit in double")

case object BooleanCoercionViolation extends ValueCoercionViolation("Boolean value expected")
case object StringCoercionViolation extends ValueCoercionViolation("String value expected")
case object IDCoercionViolation extends ValueCoercionViolation("String or Int value expected")

case class EnumValueCoercionViolation(name: String) extends ValueCoercionViolation(s"Enum value '$name' is undefined")
case object EnumCoercionViolation extends ValueCoercionViolation(s"Enum value expected")

case class FieldCoercionViolation(fieldPath: List[String], valueViolation: Violation, ownSourceMapper: Option[SourceMapper], ownPositions: List[Position], errorPrefix: String) extends AstNodeViolation {
  lazy val sourceMapper = valueViolation match {
    case astv: AstNodeViolation ⇒ astv.sourceMapper
    case _ ⇒ ownSourceMapper
  }

  lazy val positions = valueViolation match {
    case astv: AstNodeViolation ⇒ (ownPositions ++ astv.positions).distinct
    case _ ⇒ ownPositions
  }

  lazy val violationMessage = valueViolation match {
    case astv: AstNodeViolation ⇒ astv.simpleErrorMessage
    case v ⇒ v.errorMessage
  }

  lazy val simpleErrorMessage = s"${errorPrefix}Field '${fieldPath mkString "."}' has wrong value: $violationMessage."
}

case class VarTypeMismatchViolation(definitionName: String, expectedType: String, input: Option[String], violation: Violation, ownSourceMapper: Option[SourceMapper], ownPositions: List[Position]) extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation ⇒ astv.sourceMapper
    case _ ⇒ ownSourceMapper
  }

  lazy val positions = violation match {
    case astv: AstNodeViolation ⇒ (ownPositions ++ astv.positions).distinct
    case _ ⇒ ownPositions
  }

  lazy val violationMessage = violation match {
    case astv: AstNodeViolation ⇒ astv.simpleErrorMessage
    case v ⇒ v.errorMessage
  }

  lazy val simpleErrorMessage = s"Variable '$$$definitionName' expected value of type '$expectedType' but ${input map ("got: " + _) getOrElse "value is undefined"}. Reason: $violationMessage"
}

case class UnknownVariableTypeViolation(definitionName: String, varType: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$varType' expected value of type '$$$definitionName' which cannot be used as an input type."
}

case class NullValueForNotNullTypeViolation(fieldPath: List[String], typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Null value was provided for the NotNull Type '$typeName' at path '${fieldPath mkString "."}'."
}

case class InputObjectTypeMismatchViolation(fieldPath: List[String], typeName: String, value: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Value '$value' of wrong type was provided to the field of type '$typeName' at path '${fieldPath mkString "."}'."
}

// validation

case class BadValueViolation(argName: String, typeName: String, value: String, violation: Violation, ownSourceMapper: Option[SourceMapper], ownPositions: List[Position]) extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation ⇒ astv.sourceMapper
    case _ ⇒ ownSourceMapper
  }

  lazy val positions = violation match {
    case astv: AstNodeViolation ⇒ (ownPositions ++ astv.positions).distinct
    case _ ⇒ ownPositions
  }

  lazy val violationMessage = violation match {
    case astv: AstNodeViolation ⇒ astv.simpleErrorMessage
    case v ⇒ v.errorMessage
  }

  lazy val simpleErrorMessage = s"Argument '$argName' expected type '$typeName' but got: $value. Reason: $violationMessage"
}

case class BadValueForDefaultArgViolation(varName: String, typeName: String, value: String, violation: Violation, ownSourceMapper: Option[SourceMapper], ownPositions: List[Position]) extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation ⇒ astv.sourceMapper
    case _ ⇒ ownSourceMapper
  }

  lazy val positions = violation match {
    case astv: AstNodeViolation ⇒ (ownPositions ++ astv.positions).distinct
    case _ ⇒ ownPositions
  }

  lazy val violationMessage = violation match {
    case astv: AstNodeViolation ⇒ astv.simpleErrorMessage
    case v ⇒ v.errorMessage
  }
  
  lazy val simpleErrorMessage = s"Variable '$$$varName' of type '$typeName' has invalid default value: $value. Reason: $violationMessage"
}

case class DefaultForNonNullArgViolation(varName: String, typeName: String, guessTypeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$$$varName' of type '$typeName' is required and will never use the default value. Perhaps you meant to use type '$guessTypeName'."
}

case class UndefinedFieldViolation(
  fieldName: String,
  typeName: String,
  suggestedTypeNames: Seq[String],
  suggestedFieldNames: Seq[String],
  sourceMapper: Option[SourceMapper],
  positions: List[Position]
) extends AstNodeViolation {
  lazy val simpleErrorMessage = {
    val message = s"Cannot query field '$fieldName' on type '$typeName'."
    val didYouMean =
      if (suggestedTypeNames.nonEmpty) Violation.didYouMean(suggestedTypeNames, "Did you mean to use an inline fragment on")
      else Violation.didYouMean(suggestedFieldNames)

    message + didYouMean
  }
}

case class InlineFragmentOnNonCompositeErrorViolation(typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Fragment cannot condition on non composite type '$typeName'."
}

case class FragmentOnNonCompositeErrorViolation(fragName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Fragment '$fragName' cannot condition on non composite type '$typeName'."
}

case class UnknownArgViolation(argName: String, fieldName: String, typeName: String, suggestedArgs: Seq[String], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Unknown argument '$argName' on field '$fieldName' of type '$typeName'.${Violation.didYouMean(suggestedArgs)}"
}

case class UnknownDirectiveArgViolation(argName: String, dirName: String, suggestedArgs: Seq[String], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Unknown argument '$argName' on directive '$dirName'.${Violation.didYouMean(suggestedArgs)}"
}

case class UnknownDirectiveViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Unknown directive '$name'."
}

case class MisplacedDirectiveViolation(name: String, placement: Option[String], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Directive '$name' may not be used ${placement.fold("here")("on " + _)}."
}

case class UnknownFragmentViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Unknown fragment '$name'."
}

case class UnknownTypeViolation(name: String, suggestedTypes: Seq[String], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Unknown type '$name'.${Violation.didYouMean(suggestedTypes)}"
}

case class CycleErrorViolation(fragmentName: String, spreadNames: List[String], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Cannot spread fragment '$fragmentName' within itself${if (spreadNames.nonEmpty) s" via ${spreadNames map ("'" + _ + "'") mkString ", " }" else ""}."
}

case class UndefinedVarByOpViolation(varName: String, operationName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$$$varName' is not defined by operation '$operationName'."
}

case class UndefinedVarViolation(varName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$$$varName' is not defined."
}

case class UnusedFragmentViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Fragment '$name' is not used."
}

case class UnusedVariableViolation(name: String, operationName: Option[String], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$$$name' is not used${operationName.fold("")(" in operation " + _)}."
}

case class NoSubselectionAllowedViolation(fieldName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Field '$fieldName' of type '$typeName' must not have a sub selection."
}

case class RequiredSubselectionViolation(fieldName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Field '$fieldName' of type '$typeName' must have a sub selection."
}

case class DuplicateDirectiveViolation(directiveName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"The directive '$directiveName' can only be used once at this location."
}

case class TypeIncompatibleAnonSpreadViolation(parentTypeName: String, fragTypeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Fragment cannot be spread here as objects of type '$parentTypeName' can never be of type '$fragTypeName'."
}

case class TypeIncompatibleSpreadViolation(fragName: String, parentTypeName: String, fragTypeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Fragment '$fragName' cannot be spread here as objects of type '$parentTypeName' can never be of type '$fragTypeName'."
}

case class NonInputTypeOnVarViolation(varName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$$$varName' cannot be non input type '$typeName'."
}

case class BadVarPositionViolation(varName: String, varType: String, expectedType: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$$$varName' of type '$varType' used in position expecting type '$expectedType'."
}

case class MissingFieldArgViolation(fieldName: String, argName: String, typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Field '$fieldName' argument '$argName' of type '$typeName' is required but not provided."
}

case class FieldsConflictViolation(outputName: String, reason: Either[String, Vector[ConflictReason]], sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Field '$outputName' conflict because ${reasonMessage(reason)}. Use different aliases on the fields to fetch both if this was intentional."

  private def reasonMessage(reason: Either[String, Vector[ConflictReason]]): String = reason match {
    case Left(message) ⇒ message
    case Right(subReasons) ⇒ subReasons map (sr ⇒ s"subfields '${sr.fieldName}' conflict because ${reasonMessage(sr.reason)}") mkString " and "
  }
}

case class AnonOperationNotAloneViolation(sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"This anonymous operation must be the only defined operation."
}

case class DuplicateFragmentNameViolation(fragName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can only be one fragment named '$fragName'."
}

case class DuplicateOperationNameViolation(opName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can only be one operation named '$opName'."
}

case class DuplicateArgNameViolation(argName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can be only one argument named '$argName'."
}

case class DuplicateInputFieldViolation(name: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can be only one input field named '$name'."
}

case class InvalidImplementationFieldTypeViolation(interfaceName: String, objectName: String, fieldName: String, interfaceFieldType: String, objectFieldType: String) extends Violation {
  lazy val errorMessage = s"$interfaceName.$fieldName expects type '$interfaceFieldType', but $objectName.$fieldName provides type '$objectFieldType'."
}

case class MissingImplementationFieldArgumentViolation(interfaceName: String, objectName: String, fieldName: String, argumentName: String) extends Violation {
  lazy val errorMessage = s"$interfaceName.$fieldName expects argument '$argumentName', but $objectName.$fieldName does not provide it."
}

case class InvalidImplementationFieldArgumentTypeViolation(interfaceName: String, objectName: String, fieldName: String, argumentName: String, interfaceFieldType: String, objectFieldType: String) extends Violation {
  lazy val errorMessage = s"$interfaceName.$fieldName($argumentName) expects type '$interfaceFieldType', but $objectName.$fieldName($argumentName) provides type '$objectFieldType'."
}

case class ImplementationExtraFieldArgumentNotOptionalViolation(interfaceName: String, objectName: String, fieldName: String, argumentName: String, objectFieldType: String) extends Violation {
  lazy val errorMessage = s"$objectName.$fieldName($argumentName) is of required type '$objectFieldType', but is not also provided by the interface $interfaceName.$fieldName."
}

case class InvalidSubscriptionFieldViolation(typeName: String, fieldName: String) extends Violation {
  lazy val errorMessage = s"Field '$typeName.$fieldName' is defined as a subscription field, but type '$typeName' is not used as a subscription type."
}

case class NotAllSubscriptionFieldsViolation(typeName: String, fieldNames: Vector[String]) extends Violation {
  lazy val errorMessage = s"Subscription type '$typeName' may either contain only non-subscription fields or only subscription fields (defined with `Field.subs`). Following fields are non-subscription fields among other subscription fields: ${fieldNames map ("'" + _ + "'") mkString ", "}."
}

case class NotAllSubscriptionFieldsHaveSameStreamViolation(typeName: String, fieldNames: Vector[String]) extends Violation {
  lazy val errorMessage = s"Some fields of subscription type '$typeName' have incompatible stream implementations: ${fieldNames map ("'" + _ + "'") mkString ", "}."
}

case class ListValueViolation(index: Int, violation: Violation, listSourceMapper: Option[SourceMapper], listPosition: List[Position]) extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation ⇒ astv.sourceMapper
    case _ ⇒ listSourceMapper
  }

  lazy val positions = violation match {
    case astv: AstNodeViolation ⇒ astv.positions
    case _ ⇒ listPosition
  }

  lazy val simpleErrorMessage = s"[at index #$index] ${violation.errorMessage}"
}

case class MapValueViolation(fieldName: String, violation: Violation, mapSourceMapper: Option[SourceMapper], mapPosition: List[Position]) extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation ⇒ astv.sourceMapper
    case _ ⇒ mapSourceMapper
  }

  lazy val positions = violation match {
    case astv: AstNodeViolation ⇒ astv.positions
    case _ ⇒ mapPosition
  }

  lazy val simpleErrorMessage = s"[in field '$fieldName'] ${violation.errorMessage}"
}

case class NotNullInputObjectFieldMissingViolation(typeName: String, fieldName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"The NotNull field '$fieldName' defined in the input type '$typeName' is missing."
}

case class NotNullValueIsNullViolation(sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Expected non-null value, found null."
}

case class UnknownInputObjectFieldViolation(typeName: String, fieldName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Unknown field '$fieldName' is not defined in the input type '$typeName'."
}

case class InputObjectIsOfWrongTypeMissingViolation(typeName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Expected '$typeName', found not an object."
}

case class GenericInvalidValueViolation(sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Invalid value."
}

case class VariableNotAllowedViolation(varName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$varName' is used in a place where it is not allowed to be used."
}

case class InvalidInputValueViolation(argumentName: String, errorText: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Argument '$argumentName' has invalid value: $errorText"
}

case class DuplicateVariableViolation(variableName: String, sourceMapper: Option[SourceMapper], positions: List[Position]) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can be only one variable named '$variableName'."
}

case class ConflictingTypeDefinitionViolation(typeName: String, conflictingTypes: List[String], parentInfo: String) extends Violation {
  lazy val errorMessage = s"Type name '$typeName' is used for several conflicting GraphQL type kinds: ${conflictingTypes mkString ", "}. Conflict found in $parentInfo."
}

case class ReservedTypeNameViolation(typeName: String) extends Violation {
  lazy val errorMessage = s"Type name '$typeName' must not begin with '__', which is reserved by GraphQL introspection."
}

case class ReservedNameViolation(typeName: String, name: String) extends Violation {
  lazy val errorMessage = s"Name '$typeName.$name' must not begin with '__', which is reserved by GraphQL introspection."
}