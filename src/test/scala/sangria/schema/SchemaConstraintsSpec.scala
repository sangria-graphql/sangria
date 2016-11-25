package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.validation.{ReservedNameViolation, ReservedTypeNameViolation, StringCoercionViolation}

class SchemaConstraintsSpec extends WordSpec with Matchers {
  "Schema" should {
    "not allow use same type name for different GraphQL type kinds (input & output type)" in {
      val inputType = InputObjectType("Point", List(
        InputField("x", FloatType),
        InputField("y", FloatType)))

      val ouputType = ObjectType("Point", fields[Unit, Unit](
        Field("x", FloatType, resolve = _ ⇒ 1.234),
        Field("y", FloatType, resolve = _ ⇒ 1.234),
        Field("z", FloatType, resolve = _ ⇒ 1.234)))

      val queryType = ObjectType("Query", fields[Unit, Unit](
        Field("foo", OptionType(ouputType),
          arguments = Argument("points", ListInputType(inputType)) :: Nil,
          resolve = _ ⇒ None)))

      val error = intercept [SchemaValidationException] (Schema(queryType))

      error.getMessage should include (
        "Type name 'Point' is used for several conflicting GraphQL type kinds: ObjectType, InputObjectType. Conflict found in an argument 'points' defined in field 'foo' of 'Query' type.")
    }

    "not allow use same type name for different GraphQL type kinds (input & scalar type)" in {
      val inputType = InputObjectType("Point", List(
        InputField("x", FloatType),
        InputField("y", FloatType)))

      val scalarType = ScalarType[String]("Point",
        coerceOutput = valueOutput,
        coerceUserInput = {
          case s: String ⇒ Right(s)
          case _ ⇒ Left(StringCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _, _) ⇒ Right(s)
          case _ ⇒ Left(StringCoercionViolation)
        })

      val queryType = ObjectType("Query", fields[Unit, Unit](
        Field("foo", OptionType(scalarType),
          arguments = Argument("points", ListInputType(inputType)) :: Nil,
          resolve = _ ⇒ None)))

      val error = intercept [SchemaValidationException] (Schema(queryType))

      error.getMessage should include (
        "Type name 'Point' is used for several conflicting GraphQL type kinds: ScalarType, InputObjectType. Conflict found in an argument 'points' defined in field 'foo' of 'Query' type.")
    }

    "not allow reserved names" in {
      val inputType = InputObjectType("__Input", List(
        InputField("x", FloatType),
        InputField("__y", FloatType)))

      val scalarType = ScalarType[String]("__Point",
        coerceOutput = valueOutput,
        coerceUserInput = {
          case s: String ⇒ Right(s)
          case _ ⇒ Left(StringCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _, _) ⇒ Right(s)
          case _ ⇒ Left(StringCoercionViolation)
        })

      val bazType = InterfaceType("__Baz", fields[Unit, Unit](
        Field("id", IntType, resolve = _ ⇒ 1)))

      val barType = ObjectType("__Bar", interfaces[Unit, Unit](bazType), fields[Unit, Unit](
        Field("foo", OptionType(scalarType),resolve = _ ⇒ None)))

      val colorType = EnumType("__Color", values = List(
        EnumValue("RED", value = 1),
        EnumValue("__GREEN", value = 2),
        EnumValue("__BLUE", value = 3)))

      val queryType = ObjectType("Query", fields[Unit, Unit](
        Field("__foo", OptionType(scalarType),resolve = _ ⇒ None),
        Field("bar", OptionType(barType),resolve = _ ⇒ None),
        Field("color", OptionType(colorType),resolve = _ ⇒ None)))

      val error = intercept [SchemaValidationException] (Schema(queryType, additionalTypes = inputType :: Nil))

      error.violations.toSet should be (Set(
        ReservedTypeNameViolation("__Bar"),
        ReservedTypeNameViolation("__Baz"),
        ReservedTypeNameViolation("__Color"),
        ReservedTypeNameViolation("__Point"),
        ReservedTypeNameViolation("__Input"),
        ReservedNameViolation("Query", "__foo"),
        ReservedNameViolation("__Input", "__y"),
        ReservedNameViolation("__Color", "__GREEN"),
        ReservedNameViolation("__Color", "__BLUE")))
    }
  }
}
