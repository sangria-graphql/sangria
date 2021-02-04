package sangria.federation

case class _Any(__typename: String, fields: NodeObject)

object _Any {

  import sangria.validation.ValueCoercionViolation

  case object AnyCoercionViolation extends ValueCoercionViolation("_Any value expected")

  import sangria.schema.ScalarType

  val Type: ScalarType[_Any] = ScalarType[_Any](
    name = "_Any",
    coerceOutput = { case _ => "output" },
    coerceUserInput = {
      case n: NodeObject => Right(_Any(n.__typename, n))
      case _ => Left(AnyCoercionViolation)
    },
    coerceInput = { _ => Left(AnyCoercionViolation) }
  )
}
