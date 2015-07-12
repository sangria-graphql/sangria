package sangria.validation

trait Violation {
  def errorMessage: String
}

abstract class SimpleViolation(val errorMessage: String) extends Violation

case object IntCoercionViolation extends SimpleViolation("Int value expected")
case object FloatCoercionViolation extends SimpleViolation("Float or Int value expected")
case object BooleanCoercionViolation extends SimpleViolation("Boolean value expected")
case object StringCoercionViolation extends SimpleViolation("String value expected")
case object IDCoercionViolation extends SimpleViolation("String or Int value expected")

case class EnumValueCoercionViolation(name: String) extends SimpleViolation(s"Enum value '$name' is undefined")
case object EnumCoercionViolation extends SimpleViolation(s"Enum value expected")