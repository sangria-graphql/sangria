package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.validation._

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
          case ast.StringValue(s, _, _, _, _) ⇒ Right(s)
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
          case ast.StringValue(s, _, _, _, _) ⇒ Right(s)
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

      error.violations.map(_.errorMessage).toSet should be (Set(
        "Input type name '__Input' is invalid. The name is reserved for GraphQL introspection API.",
        "Field name '__y' defined in input type '__Input' is invalid. The name is reserved for GraphQL introspection API.",
        "Field name '__foo' defined in type 'Query' is invalid. The name is reserved for GraphQL introspection API.",
        "Object type name '__Bar' is invalid. The name is reserved for GraphQL introspection API.",
        "Interface type name '__Baz' is invalid. The name is reserved for GraphQL introspection API.",
        "Enum type name '__Color' is invalid. The name is reserved for GraphQL introspection API.",
        "Enum value name '__GREEN' defined in enum type '__Color' is invalid. The name is reserved for GraphQL introspection API.",
        "Enum value name '__BLUE' defined in enum type '__Color' is invalid. The name is reserved for GraphQL introspection API.",
        "Scalar type name '__Point' is invalid. The name is reserved for GraphQL introspection API."))
    }

    "reject an Enum type with incorrectly named values" in {
      val colorType = EnumType("Color", values = List(
        EnumValue("RED", value = 1),
        EnumValue("true", value = 2),
        EnumValue("false", value = 3),
        EnumValue("null", value = 4)))

      val queryType = ObjectType("Query", fields[Unit, Unit](
        Field("color", OptionType(colorType), resolve = _ ⇒ None)))

      val error = intercept [SchemaValidationException] (Schema(queryType))

      error.violations.map(_.errorMessage).toSet should be (Set(
        "Name 'Color.true' can not be used as an Enum value.",
        "Name 'Color.false' can not be used as an Enum value.",
        "Name 'Color.null' can not be used as an Enum value."))
    }

    "not allow empty list of fields" in {
      val int1Type = InterfaceType[Unit, Unit]("Interface1", Nil)
      val int2Type = InterfaceType[Unit, Unit]("Interface2", Nil, interfaces[Unit, Unit](int1Type))
      val outType = ObjectType[Unit, Unit]("Output", interfaces[Unit, Unit](int2Type), Nil)
      val inputType = InputObjectType("Input", Nil)
      val queryType = ObjectType("Query", fields[Unit, Unit](
        Field("foo", OptionType(outType),
          arguments = Argument("bar", inputType) :: Nil,
          resolve = _ ⇒ ())))

      val error = intercept [SchemaValidationException] (Schema(queryType))

      error.violations.map(_.errorMessage).toSet should be (Set(
        "Input type 'Input' must define one or more fields.",
        "Interface type 'Interface1' must define one or more fields.",
        "Interface type 'Interface2' must define one or more fields.",
        "Object type 'Output' must define one or more fields."))
    }
  }
}
