package sangria.validation

import sangria.ast.{AstLocation, Definition, SourceMapper}
import sangria.schema.DirectiveLocation
import sangria.validation.rules.ConflictReason

trait SpecViolation {
  def code: String
  def args: Map[String, String]
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

case class NoInterfaceImplementationViolation(
    interfaceName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage =
    s"Interface '$interfaceName' must be implemented by at least one object type."
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

case class DuplicateVariableViolation(
    variableName: String,
    sourceMapper: Option[SourceMapper],
    locations: List[AstLocation])
    extends AstNodeViolation {
  lazy val simpleErrorMessage = s"There can be only one variable named '$variableName'."
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
