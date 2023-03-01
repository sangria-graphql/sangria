package sangria

import sangria.marshalling.MarshallerCapability
import sangria.validation._

/** Types that describe a GraphQL schema.
  *
  * In order to serve GraphQL, one needs to define a [[Schema GraphQL schema]] built upon these
  * types.
  */
package object schema {
  def valueOutput[T](value: T, capabilities: Set[MarshallerCapability]): T = value

  implicit val IntType: ScalarType[Int] = ScalarType[Int](
    "Int",
    description = Some(
      "The `Int` scalar type represents non-fractional signed whole numeric values. " +
        "Int can represent values between -(2^31) and 2^31 - 1."),
    coerceOutput = valueOutput,
    coerceUserInput = {
      case i: Int => Right(i)
      case i: Long if i.isValidInt => Right(i.toInt)
      case i: BigInt if !i.isValidInt => Left(BigIntCoercionViolation)
      case i: BigInt => Right(i.intValue)
      case d: Double if d.isValidInt => Right(d.intValue)
      case d: BigDecimal if d.isValidInt => Right(d.intValue)
      case _ => Left(IntCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _, _) => Right(i)
      case ast.BigIntValue(i, _, _) if !i.isValidInt => Left(BigIntCoercionViolation)
      case ast.BigIntValue(i, _, _) => Right(i.intValue)
      case _ => Left(IntCoercionViolation)
    }
  )

  implicit val LongType: ScalarType[Long] = ScalarType[Long](
    "Long",
    description = Some(
      "The `Long` scalar type represents non-fractional signed whole numeric values. " +
        "Long can represent values between -(2^63) and 2^63 - 1."),
    coerceOutput = valueOutput,
    coerceUserInput = {
      case i: Int => Right(i: Long)
      case i: Long => Right(i)
      case i: BigInt if !i.isValidLong => Left(BigLongCoercionViolation)
      case i: BigInt => Right(i.longValue)
      case d: Double if d.isWhole => Right(d.toLong)
      case d: BigDecimal if d.isValidLong => Right(d.longValue)
      case _ => Left(LongCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _, _) => Right(i: Long)
      case ast.BigIntValue(i, _, _) if !i.isValidLong => Left(BigLongCoercionViolation)
      case ast.BigIntValue(i, _, _) => Right(i.longValue)
      case _ => Left(LongCoercionViolation)
    }
  )

  implicit val BigIntType: ScalarType[BigInt] = ScalarType[BigInt](
    "BigInt",
    description = Some(
      "The `BigInt` scalar type represents non-fractional signed whole numeric values. " +
        "BigInt can represent arbitrary big values."),
    coerceOutput = valueOutput,
    coerceUserInput = {
      case i: Int => Right(BigInt(i))
      case i: Long => Right(BigInt(i))
      case i: BigInt => Right(i)
      case d: Double if d.isWhole => Right(BigInt(d.toLong))
      case d: BigDecimal if d.isWhole => Right(d.toBigInt)
      case _ => Left(IntCoercionViolation)
    },
    coerceInput = {
      case ast.IntValue(i, _, _) => Right(i)
      case ast.BigIntValue(i, _, _) => Right(i)
      case _ => Left(IntCoercionViolation)
    }
  )

  implicit val FloatType: ScalarType[Double] = ScalarType[Double](
    "Float",
    description = Some(
      "The `Float` scalar type represents signed double-precision fractional " +
        "values as specified by [IEEE 754](https://en.wikipedia.org/wiki/IEEE_754)."),
    coerceOutput = (v, _) =>
      // .isNaN and .isInfinity box, we explicitly avoid that here
      if (java.lang.Double.isNaN(v) || java.lang.Double.isInfinite(v))
        null
      else
        v,
    coerceUserInput = {
      case i: Int => Right(i.toDouble)
      case i: Long => Right(i.toDouble)
      case i: BigInt if !i.isValidDouble => Left(BigDecimalCoercionViolation)
      case i: BigInt => Right(i.doubleValue)
      case d: Double => Right(d)
      case d: BigDecimal if !d.isDecimalDouble => Left(BigDecimalCoercionViolation)
      case d: BigDecimal => Right(d.doubleValue)
      case _ => Left(FloatCoercionViolation)
    },
    coerceInput = {
      case ast.FloatValue(d, _, _) => Right(d)
      case ast.BigDecimalValue(d, _, _) if !d.isDecimalDouble => Left(BigDecimalCoercionViolation)
      case ast.BigDecimalValue(d, _, _) => Right(d.doubleValue)
      case ast.IntValue(i, _, _) => Right(i)
      case ast.BigIntValue(i, _, _) if !i.isValidDouble => Left(BigDecimalCoercionViolation)
      case ast.BigIntValue(i, _, _) => Right(i.doubleValue)
      case _ => Left(FloatCoercionViolation)
    }
  )

  implicit val BigDecimalType: ScalarType[BigDecimal] = ScalarType[BigDecimal](
    "BigDecimal",
    description = Some(
      "The `BigDecimal` scalar type represents signed fractional values with arbitrary precision."),
    coerceOutput = valueOutput,
    coerceUserInput = {
      case i: Int => Right(BigDecimal(i))
      case i: Long => Right(BigDecimal(i))
      case i: BigInt => Right(BigDecimal(i))
      case d: Double => Right(BigDecimal(d))
      case d: BigDecimal => Right(d)
      case _ => Left(FloatCoercionViolation)
    },
    coerceInput = {
      case ast.BigDecimalValue(d, _, _) => Right(d)
      case ast.FloatValue(d, _, _) => Right(BigDecimal(d))
      case ast.IntValue(i, _, _) => Right(BigDecimal(i))
      case ast.BigIntValue(i, _, _) => Right(BigDecimal(i))
      case _ => Left(FloatCoercionViolation)
    }
  )

  implicit val BooleanType: ScalarType[Boolean] = ScalarType[Boolean](
    "Boolean",
    description = Some("The `Boolean` scalar type represents `true` or `false`."),
    coerceOutput = valueOutput,
    coerceUserInput = {
      case b: Boolean => Right(b)
      case _ => Left(BooleanCoercionViolation)
    },
    coerceInput = {
      case ast.BooleanValue(b, _, _) => Right(b)
      case _ => Left(BooleanCoercionViolation)
    }
  )

  implicit val StringType: ScalarType[String] = ScalarType[String](
    "String",
    description = Some(
      "The `String` scalar type represents textual data, represented as UTF-8 " +
        "character sequences. The String type is most often used by GraphQL to " +
        "represent free-form human-readable text."),
    coerceOutput = valueOutput,
    coerceUserInput = {
      case s: String => Right(s)
      case _ => Left(StringCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(s, _, _, _, _) => Right(s)
      case _ => Left(StringCoercionViolation)
    }
  )

  val IDType: ScalarType[String] = ScalarType[String](
    "ID",
    description = Some(
      "The `ID` scalar type represents a unique identifier, often used to " +
        "refetch an object or as key for a cache. The ID type appears in a JSON " +
        "response as a String; however, it is not intended to be human-readable. " +
        "When expected as an input type, any string (such as `\"4\"`) or integer " +
        "(such as `4`) input value will be accepted as an ID."),
    coerceOutput = valueOutput,
    coerceUserInput = {
      case s: String => Right(s)
      case i: Int => Right(i.toString)
      case i: Long => Right(i.toString)
      case i: BigInt => Right(i.toString)
      case _ => Left(IDCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(id, _, _, _, _) => Right(id)
      case ast.IntValue(id, _, _) => Right(id.toString)
      case ast.BigIntValue(id, _, _) => Right(id.toString)
      case _ => Left(IDCoercionViolation)
    }
  )

  val BuiltinGraphQLScalars: List[ScalarType[_]] =
    IntType :: FloatType :: BooleanType :: StringType :: IDType :: Nil

  val BuiltinSangriaScalars: List[ScalarType[_]] =
    LongType :: BigIntType :: BigDecimalType :: Nil

  val BuiltinScalars: List[ScalarType[_]] =
    BuiltinGraphQLScalars ++ BuiltinSangriaScalars

  val BuiltinScalarsByName: Map[String, ScalarType[_]] =
    BuiltinScalars.groupBy(_.name).map { case (k, v) => (k, v.head) }.toMap // required for 2.12

  val BuiltinGraphQLScalarsByName: Map[String, ScalarType[_]] =
    BuiltinGraphQLScalars
      .groupBy(_.name)
      .map { case (k, v) => (k, v.head) }
      .toMap // required for 2.12

  val BuiltinSangriaScalarsByName: Map[String, ScalarType[_]] =
    BuiltinSangriaScalars
      .groupBy(_.name)
      .map { case (k, v) => (k, v.head) }
      .toMap // required for 2.12

  val IfArg: Argument[Boolean] = Argument("if", BooleanType, "Included when true.")

  val IncludeDirective: Directive = Directive(
    "include",
    description = Some(
      "Directs the executor to include this field or fragment only when the `if` argument is true."),
    arguments = IfArg :: Nil,
    locations = Set(
      DirectiveLocation.Field,
      DirectiveLocation.FragmentSpread,
      DirectiveLocation.InlineFragment),
    // if we don't know if we should include it, then we should include it:
    // ValueCollector will fail before we get here if values must be known, such as when preparing or executing a query,
    // but for e.g. running a QueryReducer without known variables, we must be conservative
    shouldInclude = ctx => ctx.args.argOpt(IfArg).getOrElse(true)
  )

  val SkipDirective: Directive = Directive(
    "skip",
    description =
      Some("Directs the executor to skip this field or fragment when the `if` argument is true."),
    arguments = IfArg :: Nil,
    locations = Set(
      DirectiveLocation.Field,
      DirectiveLocation.FragmentSpread,
      DirectiveLocation.InlineFragment),
    // if we don't know if we should include it, then we should include it:
    // ValueCollector will fail before we get here if values must be known, such as when preparing or executing a query,
    // but for e.g. running a QueryReducer without known variables, we must be conservative
    shouldInclude = ctx => !ctx.args.argOpt(IfArg).getOrElse(false)
  )

  val DefaultDeprecationReason = "No longer supported"

  val ReasonArg: Argument[String] = Argument(
    "reason",
    OptionInputType(StringType),
    description = "Explains why this element was deprecated, usually also including a " +
      "suggestion for how to access supported similar data. Formatted " +
      "in [Markdown](https://daringfireball.net/projects/markdown/).",
    defaultValue = DefaultDeprecationReason
  )

  val DeprecatedDirective: Directive = Directive(
    "deprecated",
    description = Some("Marks an element of a GraphQL schema as no longer supported."),
    arguments = ReasonArg :: Nil,
    locations = Set(DirectiveLocation.FieldDefinition, DirectiveLocation.EnumValue),
    shouldInclude = ctx => !ctx.arg(IfArg)
  )

  val BuiltinDirectives: List[Directive] =
    IncludeDirective :: SkipDirective :: DeprecatedDirective :: Nil

  val BuiltinDirectivesByName: Map[String, Directive] =
    BuiltinDirectives.groupBy(_.name).map { case (k, v) => (k, v.head) }

  def fields[Ctx, Val](fields: Field[Ctx, Val]*): List[Field[Ctx, Val]] = fields.toList

  def interfaces[Ctx, Concrete](
      interfaces: PossibleInterface[Ctx, Concrete]*): List[PossibleInterface[Ctx, Concrete]] =
    interfaces.toList

  def possibleTypes[Ctx, Abstract](
      objectTypes: PossibleObject[Ctx, Abstract]*): List[PossibleObject[Ctx, Abstract]] =
    objectTypes.toList
}
