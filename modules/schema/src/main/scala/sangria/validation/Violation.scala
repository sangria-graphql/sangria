package sangria.validation

import sangria.ast.{AstLocation, SourceMapper}
import sangria.util.StringUtil

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

abstract class BaseViolation(val errorMessage: String) extends Violation
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
