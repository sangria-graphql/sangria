package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.validation.StringCoercionViolation

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
        coerceOutput = s ⇒ ast.StringValue(s),
        coerceUserInput = {
          case s: String ⇒ Right(s)
          case _ ⇒ Left(StringCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _) ⇒ Right(s)
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
  }
}
