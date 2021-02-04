package sangria.federation

case class _Any[Node](__typename: String, fields: NodeObject[Node])

object _Any {

  import sangria.validation.ValueCoercionViolation

  case object AnyCoercionViolation extends ValueCoercionViolation("_Any value expected")

  import sangria.schema.ScalarType

  def __type[Node] = ScalarType[_Any[Node]](
    name = "_Any",
    coerceOutput = { case _ => "output" },
    coerceUserInput = {
      case n: NodeObject[Node] => Right(_Any(n.__typename, n))
      case _ => Left(AnyCoercionViolation)
    },
    coerceInput = { _ => Left(AnyCoercionViolation) }
  )
}
