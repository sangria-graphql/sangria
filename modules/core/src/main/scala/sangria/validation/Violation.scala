package sangria.validation

import sangria.ast.{AstLocation, Definition}
import sangria.ast.SourceMapper
import sangria.schema.DirectiveLocation
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

trait SpecViolation {
  def code: String
  def args: Map[String, String]
}

abstract class BaseViolation(val errorMessage: String) extends Violation

trait AstNodeLocation {
  def sourceMapper: Option[SourceMapper]
  def locations: List[AstLocation]
  def simpleErrorMessage: String

  lazy val astLocation = (for {
    sm <- sourceMapper
  } yield locations
    .map(p => s" ${sm.renderLocation(p)}:\n${sm.renderLinePosition(p)}")
    .mkString("\n")).getOrElse("")

  final def errorMessage = simpleErrorMessage + astLocation
}

trait AstNodeViolation extends Violation with AstNodeLocation

abstract class ValueCoercionViolation(errorMessage: String) extends BaseViolation(errorMessage)

case object IntCoercionViolation extends ValueCoercionViolation("Int value expected")
case object BigIntCoercionViolation extends ValueCoercionViolation("Value is too big to fit in Int")

case object LongCoercionViolation extends ValueCoercionViolation("Long value expected")
case object BigLongCoercionViolation
    extends ValueCoercionViolation("Value is too big to fit in Long")

case object FloatCoercionViolation extends ValueCoercionViolation("Float or Int value expected")
case object BigDecimalCoercionViolation
    extends ValueCoercionViolation("Float or Int value is too big to fit in double")

case object BooleanCoercionViolation extends ValueCoercionViolation("Boolean value expected")
case object StringCoercionViolation extends ValueCoercionViolation("String value expected")
case object IDCoercionViolation extends ValueCoercionViolation("String or Int value expected")

case class EnumValueCoercionViolation(name: String, typeName: String, knownValues: Seq[String])
    extends ValueCoercionViolation(
      s"Enum value '$name' is undefined in enum type '$typeName'. Known values are: ${knownValues
          .mkString(", ")}.")
case object EnumCoercionViolation extends ValueCoercionViolation(s"Enum value expected")

case class FieldCoercionViolation(
    fieldPath: List[String],
    valueViolation: Violation,
    ownSourceMapper: Option[SourceMapper],
    ownLocations: List[AstLocation],
    errorPrefix: String,
    isArgument: Boolean)
    extends AstNodeViolation {
  lazy val sourceMapper = valueViolation match {
    case astv: AstNodeViolation => astv.sourceMapper
    case _ => ownSourceMapper
  }

  lazy val locations = valueViolation match {
    case astv: AstNodeViolation => (ownLocations ++ astv.locations).distinct
    case _ => ownLocations
  }

  lazy val violationMessage = valueViolation match {
    case astv: AstNodeViolation => astv.simpleErrorMessage
    case v => v.errorMessage
  }

  lazy val simpleErrorMessage =
    s"$errorPrefix${if (isArgument) "Argument" else "Field"} '${fieldPath.mkString(".")}' has wrong value: $violationMessage."
}

case class VarTypeMismatchViolation(
    definitionName: String,
    expectedType: String,
    input: Option[String],
    violation: Violation,
    ownSourceMapper: Option[SourceMapper],
    ownLocations: List[AstLocation])
    extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation => astv.sourceMapper
    case _ => ownSourceMapper
  }

  lazy val locations = violation match {
    case astv: AstNodeViolation => (ownLocations ++ astv.locations).distinct
    case _ => ownLocations
  }

  lazy val violationMessage = violation match {
    case astv: AstNodeViolation => astv.simpleErrorMessage
    case v => v.errorMessage
  }

  lazy val simpleErrorMessage =
    s"Variable '$$$definitionName' expected value of type '$expectedType' but ${input.map("got: " + _).getOrElse("value is undefined")}. Reason: $violationMessage"
}

case class UnknownVariableTypeViolation(
    definitionName: String,
    varType: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Variable '$varType' expected value of type '$$$definitionName' which cannot be used as an input type."
}

case class NullValueForNotNullTypeViolation(
    fieldPath: List[String],
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Null value was provided for the NotNull Type '$typeName' at path '${fieldPath.mkString(".")}'."
}

case class InputObjectTypeMismatchViolation(
    fieldPath: List[String],
    typeName: String,
    value: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Value '$value' of wrong type was provided to the field of type '$typeName' at path '${fieldPath
        .mkString(".")}'."
}

// validation

case class BadValueViolation(
    typeName: String,
    value: String,
    violation: Option[Violation],
    ownSourceMapper: Option[SourceMapper],
    ownLocations: List[AstLocation])
    extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case Some(astv: AstNodeViolation) => astv.sourceMapper
    case _ => ownSourceMapper
  }

  lazy val locations = violation match {
    case Some(astv: AstNodeViolation) => (ownLocations ++ astv.locations).distinct
    case _ => ownLocations
  }

  lazy val violationMessage = violation.map {
    case astv: AstNodeViolation => astv.simpleErrorMessage
    case v => v.errorMessage
  }

  lazy val simpleErrorMessage =
    s"Expected type '$typeName', found '$value'.${violationMessage.fold("")(" " + _)}"
}

case class InvalidInputDocumentViolation(
    typeName: String,
    value: String,
    violation: Violation,
    ownSourceMapper: Option[SourceMapper],
    ownLocations: List[AstLocation])
    extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation => astv.sourceMapper
    case _ => ownSourceMapper
  }

  lazy val locations = violation match {
    case astv: AstNodeViolation => (ownLocations ++ astv.locations).distinct
    case _ => ownLocations
  }

  lazy val violationMessage = violation match {
    case astv: AstNodeViolation => astv.simpleErrorMessage
    case v => v.errorMessage
  }

  lazy val simpleErrorMessage = s"At path $violationMessage"
}

case class BadValueForDefaultArgViolation(
    varName: String,
    typeName: String,
    value: String,
    violation: Violation,
    ownSourceMapper: Option[SourceMapper],
    ownLocations: List[AstLocation])
    extends AstNodeViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation => astv.sourceMapper
    case _ => ownSourceMapper
  }

  lazy val locations = violation match {
    case astv: AstNodeViolation => (ownLocations ++ astv.locations).distinct
    case _ => ownLocations
  }

  lazy val violationMessage = violation match {
    case astv: AstNodeViolation => astv.simpleErrorMessage
    case v => v.errorMessage
  }

  lazy val simpleErrorMessage =
    s"Variable '$$$varName' of type '$typeName' has invalid default value: $value. Reason: $violationMessage"
}

case class DefaultForNonNullArgViolation(
    varName: String,
    typeName: String,
    guessTypeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Variable '$$$varName' of type '$typeName' is required and will never use the default value. Perhaps you meant to use type '$guessTypeName'."
}

case class UndefinedFieldViolation(
    fieldName: String,
    typeName: String,
    suggestedTypeNames: Seq[String],
    suggestedFieldNames: Seq[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation]
) extends AstNodeViolation
    with SpecViolation {
  val code = "undefinedField"
  val args = Map("fieldName" -> fieldName, "type" -> typeName)

  lazy val simpleErrorMessage = {
    val message = s"Cannot query field '$fieldName' on type '$typeName'."
    val didYouMean =
      if (suggestedTypeNames.nonEmpty)
        Violation.didYouMean(suggestedTypeNames, "Did you mean to use an inline fragment on")
      else Violation.didYouMean(suggestedFieldNames)

    message + didYouMean
  }
}

case class InlineFragmentOnNonCompositeErrorViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "inlineFragmentOnNonCompositeType"
  val args = Map("type" -> typeName)

  lazy val simpleErrorMessage = s"Fragment cannot condition on non composite type '$typeName'."
}

case class FragmentOnNonCompositeErrorViolation(
    fragName: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "fragmentOnNonCompositeType"
  val args = Map("fragmentName" -> fragName, "type" -> typeName)

  lazy val simpleErrorMessage =
    s"Fragment '$fragName' cannot condition on non composite type '$typeName'."
}

case class UnknownArgViolation(
    argName: String,
    fieldName: String,
    typeName: String,
    suggestedArgs: Seq[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "unknownArgument"
  val args = Map("argumentName" -> argName, "fieldName" -> fieldName, "typeName" -> typeName)

  lazy val simpleErrorMessage =
    s"Unknown argument '$argName' on field '$fieldName' of type '$typeName'.${Violation.didYouMean(suggestedArgs)}"
}

case class UnknownDirectiveArgViolation(
    argName: String,
    dirName: String,
    suggestedArgs: Seq[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "unknownDirectiveArgument"
  val args = Map("argumentName" -> argName, "directiveName" -> dirName)

  lazy val simpleErrorMessage =
    s"Unknown argument '$argName' on directive '$dirName'.${Violation.didYouMean(suggestedArgs)}"
}

case class UnknownDirectiveViolation(
    name: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "unknownDirective"
  val args = Map("directiveName" -> name)

  lazy val simpleErrorMessage = s"Unknown directive '$name'."
}

case class MisplacedDirectiveViolation(
    name: String,
    correctPlacement: Option[(DirectiveLocation.Value, String)],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "misplacedDirective"
  val args = Map(
    "directiveName" -> name,
    "location" -> correctPlacement
      .map(loc => DirectiveLocation.toSpecString(loc._1))
      .getOrElse("here"))

  lazy val simpleErrorMessage =
    s"Directive '$name' may not be used ${correctPlacement.fold("here")("on " + _._2)}."
}

case class UnknownFragmentViolation(
    name: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Unknown fragment '$name'."
}

case class UnknownTypeViolation(
    name: String,
    suggestedTypes: Seq[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Unknown type '$name'.${Violation.didYouMean(suggestedTypes)}"
}

case class CycleErrorViolation(
    fragmentName: String,
    spreadNames: List[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Cannot spread fragment '$fragmentName' within itself${if (spreadNames.nonEmpty)
        s" via ${spreadNames.map("'" + _ + "'").mkString(", ")}"
      else ""}."
}

case class UndefinedVarByOpViolation(
    varName: String,
    operationName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Variable '$$$varName' is not defined by operation '$operationName'."
}

case class UndefinedVarViolation(
    varName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$$$varName' is not defined."
}

case class UnusedFragmentViolation(
    name: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Fragment '$name' is not used."
}

case class UnusedVariableViolation(
    name: String,
    operationName: Option[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Variable '$$$name' is not used${operationName.fold("")(" in operation " + _)}."
}

case class NoSubselectionAllowedViolation(
    fieldName: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "noSubselectionAllowed"
  val args = Map("fieldName" -> fieldName, "type" -> typeName)

  lazy val simpleErrorMessage =
    s"Field '$fieldName' of type '$typeName' must not have a sub selection."
}

case class SubscriptionSingleFieldOnlyViolation(
    opName: Option[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"${opName.fold("Anonymous Subscription")(n => s"Subscription '$n'")} must select only one top level field."
}

case class RequiredSubselectionViolation(
    fieldName: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "requiredSubselection"
  val args = Map("fieldName" -> fieldName, "type" -> typeName)

  lazy val simpleErrorMessage = s"Field '$fieldName' of type '$typeName' must have a sub selection."
}

case class NonExecutableDefinitionViolation(
    definitionName: String,
    definition: Definition,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with SpecViolation {
  val code = "nonExecutableDefinition"
  val args = Map("defName" -> definitionName)

  lazy val simpleErrorMessage = s"The '$definitionName' definition is not executable."
}

case class DuplicateDirectiveViolation(
    directiveName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"The directive '$directiveName' can only be used once at this location."
}

case class TypeIncompatibleAnonSpreadViolation(
    parentTypeName: String,
    fragTypeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Fragment cannot be spread here as objects of type '$parentTypeName' can never be of type '$fragTypeName'."
}

case class TypeIncompatibleSpreadViolation(
    fragName: String,
    parentTypeName: String,
    fragTypeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Fragment '$fragName' cannot be spread here as objects of type '$parentTypeName' can never be of type '$fragTypeName'."
}

case class NonInputTypeOnVarViolation(
    varName: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Variable '$$$varName' cannot be non input type '$typeName'."
}

case class BadVarPositionViolation(
    varName: String,
    varType: String,
    expectedType: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Variable '$$$varName' of type '$varType' used in position expecting type '$expectedType'."
}

case class MissingFieldArgViolation(
    fieldName: String,
    argName: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Field '$fieldName' argument '$argName' of type '$typeName' is required but not provided."
}

case class FieldsConflictViolation(
    outputName: String,
    reason: Either[String, Vector[ConflictReason]],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Field '$outputName' conflict because ${reasonMessage(reason)}. Use different aliases on the fields to fetch both if this was intentional."

  private def reasonMessage(reason: Either[String, Vector[ConflictReason]]): String = reason match {
    case Left(message) => message
    case Right(subReasons) =>
      subReasons
        .map(sr => s"subfields '${sr.fieldName}' conflict because ${reasonMessage(sr.reason)}")
        .mkString(" and ")
  }
}

case class AnonOperationNotAloneViolation(
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"This anonymous operation must be the only defined operation."
}

case class DuplicateFragmentNameViolation(
    fragName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can only be one fragment named '$fragName'."
}

case class DuplicateOperationNameViolation(
    opName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can only be one operation named '$opName'."
}

case class DuplicateArgNameViolation(
    argName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can be only one argument named '$argName'."
}

case class DuplicateInputFieldViolation(
    name: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can be only one input field named '$name'."
}

case class InvalidImplementationFieldTypeViolation(
    interfaceName: String,
    objectName: String,
    fieldName: String,
    interfaceFieldType: String,
    objectFieldType: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"$interfaceName.$fieldName expects type '$interfaceFieldType', but $objectName.$fieldName provides type '$objectFieldType'."
}

case class MissingImplementationFieldArgumentViolation(
    interfaceName: String,
    objectName: String,
    fieldName: String,
    argumentName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"$interfaceName.$fieldName expects argument '$argumentName', but $objectName.$fieldName does not provide it."
}

case class InvalidImplementationFieldArgumentTypeViolation(
    interfaceName: String,
    objectName: String,
    fieldName: String,
    argumentName: String,
    interfaceFieldType: String,
    objectFieldType: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"$interfaceName.$fieldName($argumentName) expects type '$interfaceFieldType', but $objectName.$fieldName($argumentName) provides type '$objectFieldType'."
}

case class ImplementationExtraFieldArgumentNotOptionalViolation(
    interfaceName: String,
    objectName: String,
    fieldName: String,
    argumentName: String,
    objectFieldType: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"$objectName.$fieldName($argumentName) is of required type '$objectFieldType', but is not also provided by the interface $interfaceName.$fieldName."
}

case class NoInterfaceImplementationViolation(
    interfaceName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Interface '$interfaceName' must be implemented by at least one object type."
}

case class InvalidSubscriptionFieldViolation(typeName: String, fieldName: String)
    extends Violation {
  lazy val errorMessage =
    s"Field '$typeName.$fieldName' is defined as a subscription field, but type '$typeName' is not used as a subscription type."
}

case class NotAllSubscriptionFieldsViolation(typeName: String, fieldNames: Vector[String])
    extends Violation {
  lazy val errorMessage =
    s"Subscription type '$typeName' may either contain only non-subscription fields or only subscription fields (defined with `Field.subs`). Following fields are non-subscription fields among other subscription fields: ${fieldNames
        .map("'" + _ + "'")
        .mkString(", ")}."
}

case class NotAllSubscriptionFieldsHaveSameStreamViolation(
    typeName: String,
    fieldNames: Vector[String])
    extends Violation {
  lazy val errorMessage =
    s"Some fields of subscription type '$typeName' have incompatible stream implementations: ${fieldNames
        .map("'" + _ + "'")
        .mkString(", ")}."
}

trait PathBasedViolation {
  def pathString: String
  def errorMessageWithoutPath: String
}

case class ListValueViolation(
    index: Int,
    violation: Violation,
    listSourceMapper: Option[SourceMapper],
    listPosition: List[AstLocation])
    extends AstNodeViolation
    with PathBasedViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation => astv.sourceMapper
    case _ => listSourceMapper
  }

  lazy val locations = violation match {
    case astv: AstNodeViolation => listPosition ++ astv.locations
    case _ => listPosition
  }

  lazy val pathString = violation match {
    case pbv: PathBasedViolation => s"[$index]" + pbv.pathString
    case _ => s"[$index]"
  }

  lazy val errorMessageWithoutPath = violation match {
    case pbv: PathBasedViolation => pbv.errorMessageWithoutPath
    case v: AstNodeLocation => v.simpleErrorMessage
    case v => v.errorMessage
  }

  lazy val simpleErrorMessage = s"'$pathString' $errorMessageWithoutPath"
}

case class MapValueViolation(
    fieldName: String,
    violation: Violation,
    mapSourceMapper: Option[SourceMapper],
    mapPosition: List[AstLocation])
    extends AstNodeViolation
    with PathBasedViolation {
  lazy val sourceMapper = violation match {
    case astv: AstNodeViolation => astv.sourceMapper
    case _ => mapSourceMapper
  }

  lazy val locations = violation match {
    case astv: AstNodeViolation => mapPosition ++ astv.locations
    case _ => mapPosition
  }

  lazy val pathString = violation match {
    case pbv: PathBasedViolation => "." + fieldName + pbv.pathString
    case _ => "." + fieldName
  }

  lazy val errorMessageWithoutPath = violation match {
    case pbv: PathBasedViolation => pbv.errorMessageWithoutPath
    case v: AstNodeLocation => v.simpleErrorMessage
    case v => v.errorMessage
  }

  lazy val simpleErrorMessage = s"'${pathString.substring(1)}' $errorMessageWithoutPath"
}

case class UnknownInputObjectFieldViolation(
    typeName: String,
    fieldName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with PathBasedViolation {
  lazy val pathString = "." + fieldName

  lazy val errorMessageWithoutPath =
    s"Field '$fieldName' is not defined in the input type '$typeName'."

  lazy val simpleErrorMessage = s"'${pathString.substring(1)}' $errorMessageWithoutPath"
}

case class NotNullInputObjectFieldMissingViolation(
    typeName: String,
    fieldName: String,
    fieldType: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation
    with PathBasedViolation {
  lazy val pathString = "." + fieldName

  lazy val errorMessageWithoutPath =
    s"Not-null field '$fieldName' of type '$fieldType' defined in the '$typeName' input type is missing."

  lazy val simpleErrorMessage = s"'${pathString.substring(1)}' $errorMessageWithoutPath"
}

case class RequiredFieldViolation(
    typeName: String,
    fieldName: String,
    fieldType: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Field '$typeName.$fieldName' of required type '$fieldType' was not provided."
}

case class UnknownFieldViolation(
    typeName: String,
    fieldName: String,
    message: Option[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Field '$fieldName' is not defined by type '$typeName'${message.fold(".")("; " + _)}"
}

case class NotNullValueIsNullViolation(
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Expected non-null value, found null."
}

case class InputObjectIsOfWrongTypeMissingViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Expected '$typeName', found not an object."
}

case class GenericInvalidValueViolation(
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Invalid value"
}

case class VariableNotAllowedViolation(
    varName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Variable '$varName' is used in a place where it is not allowed to be used."
}

case class InvalidInputValueViolation(
    argumentName: String,
    errorText: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Argument '$argumentName' has invalid value: $errorText"
}

case class DuplicateVariableViolation(
    variableName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can be only one variable named '$variableName'."
}

case class ConflictingTypeDefinitionViolation(
    typeName: String,
    conflictingTypes: List[String],
    parentInfo: String)
    extends Violation {
  lazy val errorMessage =
    s"Type name '$typeName' is used for several conflicting GraphQL type kinds: ${conflictingTypes
        .mkString(", ")}. Conflict found in $parentInfo."
}

case class ConflictingObjectTypeCaseClassViolation(typeName: String, parentInfo: String)
    extends Violation {
  // Ideally this error message should include the conflicting classes canonical names but due to https://issues.scala-lang.org/browse/SI-2034 that's not possible
  lazy val errorMessage =
    s"""Type name '$typeName' is used for several conflicting GraphQL ObjectTypes based on different classes. Conflict found in $parentInfo. One possible fix is to use ObjectTypeName like this: deriveObjectType[Foo, Bar](ObjectTypeName("OtherBar")) to avoid that two ObjectTypes have the same name."""
}

case class ConflictingInputObjectTypeCaseClassViolation(typeName: String, parentInfo: String)
    extends Violation {
  // Ideally this error message should include the conflicting classes canonical names but due to https://issues.scala-lang.org/browse/SI-2034 that's not possible
  lazy val errorMessage =
    s"""Type name '$typeName' is used for several conflicting GraphQL InputObjectTypes based on different classes. Conflict found in $parentInfo. One possible fix is to use InputObjectTypeName like this: deriveInputObjectType[Foo, Bar](InputObjectTypeName("OtherBar")) to avoid that two InputObjectTypes have the same name."""
}

case class ReservedTypeNameViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Type name '$typeName' must not begin with '__', which is reserved by GraphQL introspection."
}

case class ReservedNameViolation(
    typeName: String,
    name: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Name '$typeName.$name' must not begin with '__', which is reserved by GraphQL introspection."
}

case class InvalidTypeNameViolation(
    kind: String,
    typeName: String,
    explanation: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"$kind type name '$typeName' is invalid. $explanation"
}

case class InvalidDirectiveNameViolation(
    dirName: String,
    explanation: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Directive name '$dirName' is invalid. $explanation"
}

case class InvalidEnumValueNameViolation(
    typeName: String,
    valueName: String,
    explanation: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Enum value name '$valueName' defined in enum type '$typeName' is invalid. $explanation"
}

case class InvalidFieldNameViolation(
    typeName: String,
    fieldName: String,
    explanation: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Field name '$fieldName' defined in type '$typeName' is invalid. $explanation"
}

case class InvalidInputFieldNameViolation(
    typeName: String,
    fieldName: String,
    explanation: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Field name '$fieldName' defined in input type '$typeName' is invalid. $explanation"
}

case class InvalidFieldArgumentNameViolation(
    typeName: String,
    fieldName: String,
    argName: String,
    explanation: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Argument name '$argName' defined in '$typeName.$fieldName' is invalid. $explanation"
}

case class InvalidDirectiveArgumentNameViolation(
    dirName: String,
    argName: String,
    explanation: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Argument name '$argName' defined in directive '$dirName' is invalid. $explanation"
}

case class EmptyUnionMembersViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Union type '$typeName' must define one or more member types."
}

case class NonUniqueUnionMembersViolation(
    typeName: String,
    memberName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Union type '$typeName' can only include type '$memberName' once."
}

case class EmptyEnumValuesMembersViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Enum type '$typeName' must define one or more values."
}

case class NonUniqueEnumValuesViolation(
    typeName: String,
    valueName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Enum type '$typeName' can include value '$valueName' only once."
}

case class EmptyInputFieldsViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Input type '$typeName' must define one or more fields."
}

case class NonUniqueInputFieldsViolation(
    typeName: String,
    fieldName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Input type '$typeName' can include field '$fieldName' only once."
}

case class EmptyFieldsViolation(
    kind: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"$kind type '$typeName' must define one or more fields."
}

case class NonUniqueFieldsViolation(
    kind: String,
    typeName: String,
    fieldName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"$kind type '$typeName' can include field '$fieldName' only once."
}

case class NonUniqueInterfacesViolation(
    typeName: String,
    interfaceName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Object type '$typeName' can implement interface '$interfaceName' only once."
}

case class CircularImplementViolation(
    typeName: String,
    interfaceName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Interface '$typeName' cannot implement '$interfaceName' because it would create a circular reference."
}

case class NonUniqueFieldArgumentsViolation(
    typeName: String,
    fieldName: String,
    argName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Field '$typeName.$fieldName' can include argument '$argName' only once."
}

case class NonUniqueDirectiveArgumentsViolation(
    dirName: String,
    argName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Directive '$dirName' can include argument '$argName' only once."
}

case class ReservedEnumValueNameViolation(
    typeName: String,
    valueName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Name '$typeName.$valueName' can not be used as an Enum value."
}

case class VariableInferenceViolation(
    variableName: String,
    type1: String,
    type2: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Inferred variable '$$$variableName' is used with two conflicting types: '$type1' and '$type2'."
}

case class NonUniqueRootTypeViolation(
    operationType: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Must provide only one $operationType type in schema."
}

case class NonUniqueSchemaDefinitionViolation(
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = "Must provide only one schema definition."
}

case class NoQueryTypeViolation(sourceMapper: Option[SourceMapper], locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    "Must provide schema definition with query type or a type named Query."
}

case class NotExactlyOneOfField(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation]
) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Exactly one key must be specified for oneOf type '${typeName}'."
}

case class OneOfMandatoryField(
    fieldName: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation]
) extends AstNodeViolation {
  lazy val simpleErrorMessage = s"oneOf input field '${typeName}.${fieldName}' must be nullable."
}

case class OneOfDefaultValueField(
    fieldName: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation]
) extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"oneOf input field '${typeName}.${fieldName}' cannot have a default value."
}

case class NonUniqueTypeDefinitionViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Type '$typeName' is defined more than once."
}

case class NonUniqueDirectiveDefinitionViolation(
    name: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Directive '$name' is defined more than once."
}

case class TypeExtensionOnWrongKindViolation(
    typeKind: String,
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Cannot extend non-$typeKind type '$typeName'."
}

case class TypeExtensionOnNonExistingTypeViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Cannot extend type '$typeName' because it does not exist."
}

case class ExistingTypeViolation(
    typeName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Type '$typeName' already exists in the schema. It cannot also be defined in this type definition."
}

case class InvalidTypeUsageViolation(
    expectedTypeKind: String,
    tpe: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"Type '$tpe' is not an $expectedTypeKind type."
}

case class InputObjectTypeRecursion(
    name: String,
    fieldName: String,
    path: List[String],
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage: String =
    s"Cannot reference InputObjectType '$name' within itself through a series of non-null fields: '$fieldName${if (path.isEmpty) ""
      else "."}${path.mkString(".")}'."
}
