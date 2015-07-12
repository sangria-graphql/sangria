package sangria

import sangria.validation.{IDCoercionViolation, StringCoercionViolation, FloatCoercionViolation, IntCoercionViolation}

package object schema {
  val IntType = ScalarType[Int]("Int",
    coerceOutput = ast.IntValue(_),
    coerceInput = {
      case ast.IntValue(i, _) => Right(i)
      case _ => Left(IntCoercionViolation)
    })

  val FloatType = ScalarType[Double]("Float",
    coerceOutput = ast.FloatValue(_),
    coerceInput = {
      case ast.FloatValue(d, _) => Right(d)
      case ast.IntValue(i, _) => Right(i)
      case _ => Left(FloatCoercionViolation)
    })

  val BooleanType = ScalarType[Boolean]("Boolean",
    coerceOutput = (b) => ast.BooleanValue(b),
    coerceInput = {
      case ast.BooleanValue(b, _) => Right(b)
      case _ => Left(IntCoercionViolation)
    })

  val StringType = ScalarType[String]("String",
    coerceOutput = (s) => ast.StringValue(s),
    coerceInput = {
      case ast.StringValue(s, _) => Right(s)
      case _ => Left(StringCoercionViolation)
    })

  val IDType = ScalarType[String]("ID",
    coerceOutput = (s) => ast.StringValue(s),
    coerceInput = {
      case ast.StringValue(id, _) => Right(id)
      case ast.IntValue(id, _) => Right(id.toString)
      case _ => Left(IDCoercionViolation)
    })

  val IncludeDirective = Directive("include",
    description = Some("Directs the executor to include this field or fragment only when the `if` argument is true."),
    arguments = Argument("if", NonNullInputType(BooleanType), Some("Included when true.")) :: Nil,
    onOperation = false,
    onFragment = true,
    onField = true)

  val SkipDirective = Directive("skip",
    description = Some("Directs the executor to skip this field or fragment when the `if` argument is true."),
    arguments = Argument("if", NonNullInputType(BooleanType), Some("Skipped when true.")) :: Nil,
    onOperation = false,
    onFragment = true,
    onField = true)
}
