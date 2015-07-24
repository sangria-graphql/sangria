package sangria.validation

import org.parboiled2.Position
import sangria.parser.SourceMapper

trait Violation {
  def errorMessage: String
}

abstract class BaseViolation(val errorMessage: String) extends Violation

trait AstNodeLocation {
  def sourceMapper: Option[SourceMapper]
  def position: Option[Position]

  lazy val astLocation = sourceMapper flatMap (sm => position map (p => s" (line ${p.line}, column ${p.column}}):\n" + sm.renderPosition(p))) getOrElse ""
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

case class FieldCoercionViolation(fieldPath: List[String], valueViolation: Violation, sourceMapper: Option[SourceMapper], position: Option[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Field '${fieldPath mkString "."}' has wrong value: ${valueViolation.errorMessage}.${astLocation}"
}

case class VarTypeMismatchViolation(definitionName: String, expectedType: String, input: Option[String], sourceMapper: Option[SourceMapper], position: Option[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable $$$definitionName expected value of type $expectedType but ${input map ("got: " + _) getOrElse "value is undefined"}.${astLocation}"
}

case class UnknownVariableTypeViolation(definitionName: String, varType: String, sourceMapper: Option[SourceMapper], position: Option[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Variable '$varType' expected value of type '$$$definitionName' which cannot be used as an input type.${astLocation}"
}

case class NullValueForNotNullTypeViolation(fieldPath: List[String], typeName: String, sourceMapper: Option[SourceMapper], position: Option[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Null value was provided for the NotNull Type '$typeName' at path '${fieldPath mkString "."}'.${astLocation}"
}

case class InputObjectTypeMismatchViolation(fieldPath: List[String], typeName: String, value: String, sourceMapper: Option[SourceMapper], position: Option[Position]) extends AstNodeViolation {
  lazy val errorMessage = s"Value '${value}' of wrong type was provided to the field of type '${typeName}' at path '${fieldPath mkString "."}'.${astLocation}"
}