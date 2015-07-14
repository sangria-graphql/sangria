package sangria

import sangria.validation._

package object schema {
  val IntType = ScalarType[Int]("Int",
    coerceOutput = ast.IntValue(_),
    coerceUserInput = {
      case i: Int => Right(i)
      case _ => Left(IntCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _) => Right(i)
      case _ => Left(IntCoercionViolation)
    })

  val FloatType = ScalarType[Double]("Float",
    coerceOutput = ast.FloatValue(_),
    coerceUserInput = {
      case i: Int => Right(i.toDouble)
      case f: Float => Right(f.toDouble)
      case d: Double => Right(d)
      case _ => Left(FloatCoercionViolation)
    },
    coerceInput = {
      case ast.FloatValue(d, _) => Right(d)
      case ast.IntValue(i, _) => Right(i)
      case _ => Left(FloatCoercionViolation)
    })

  val BooleanType = ScalarType[Boolean]("Boolean",
    coerceOutput = b => ast.BooleanValue(b),
    coerceUserInput = {
      case b: Boolean => Right(b)
      case _ => Left(BooleanCoercionViolation)
    },
    coerceInput = {
      case ast.BooleanValue(b, _) => Right(b)
      case _ => Left(BooleanCoercionViolation)
    })

  val StringType = ScalarType[String]("String",
    coerceOutput = s => ast.StringValue(s),
    coerceUserInput = {
      case s: String => Right(s)
      case _ => Left(StringCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(s, _) => Right(s)
      case _ => Left(StringCoercionViolation)
    })

  val IDType = ScalarType[String]("ID",
    coerceOutput = s => ast.StringValue(s),
    coerceUserInput = {
      case s: String => Right(s)
      case i: Int => Right(i.toString)
      case _ => Left(IDCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(id, _) => Right(id)
      case ast.IntValue(id, _) => Right(id.toString)
      case _ => Left(IDCoercionViolation)
    })

  val BuiltinScalars = IntType :: FloatType :: BooleanType :: StringType :: IDType :: Nil

  val IfArg = Argument("if", BooleanType, Some("Included when true."))

  val IncludeDirective = Directive("include",
    description = Some("Directs the executor to include this field or fragment only when the `if` argument is true."),
    arguments = IfArg :: Nil,
    onOperation = false,
    onFragment = true,
    onField = true,
    shouldInclude = ctx => ctx.arg[Boolean](IfArg))

  val SkipDirective = Directive("skip",
    description = Some("Directs the executor to skip this field or fragment when the `if` argument is true."),
    arguments = IfArg :: Nil,
    onOperation = false,
    onFragment = true,
    onField = true,
    shouldInclude = ctx => !ctx.arg[Boolean](IfArg))

  val BuiltinDirectives = IncludeDirective :: SkipDirective :: Nil
}
