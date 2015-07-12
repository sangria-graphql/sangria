package sangria.validation

trait Violation {
  def errorMessage: String
}

abstract class BaseViolation(val errorMessage: String) extends Violation

abstract class ValueCoercionViolation(errorMessage: String) extends BaseViolation(errorMessage)

case object IntCoercionViolation extends ValueCoercionViolation("Int value expected")
case object FloatCoercionViolation extends ValueCoercionViolation("Float or Int value expected")
case object BooleanCoercionViolation extends ValueCoercionViolation("Boolean value expected")
case object StringCoercionViolation extends ValueCoercionViolation("String value expected")
case object IDCoercionViolation extends ValueCoercionViolation("String or Int value expected")

case class EnumValueCoercionViolation(name: String) extends ValueCoercionViolation(s"Enum value '$name' is undefined")
case object EnumCoercionViolation extends ValueCoercionViolation(s"Enum value expected")

case class FieldCoercionViolation(fieldPath: List[String], valueViolation: Violation) extends BaseViolation(
  s"Field '${fieldPath mkString "."}' has wrong value: ${valueViolation.errorMessage}.")

case class VarTypeMismatchViolation(definitionName: String, expectedType: String, input: Option[String]) extends BaseViolation(
  s"Variable $$$definitionName expected value of type $expectedType but got: ${input getOrElse "<UNDEFINED>"}.")

case class UnknownVariableTypeViolation(definitionName: String, varType: String) extends BaseViolation(
  s"Type '$varType' of variable $$$definitionName not found.")