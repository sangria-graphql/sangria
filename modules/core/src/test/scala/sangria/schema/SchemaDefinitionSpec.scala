package sangria.schema

import sangria.ast
import sangria.execution.Executor
import sangria.validation.StringCoercionViolation
import sangria.introspection.{IntrospectionParser, introspectionQuery}
import sangria.util.FutureResultSupport
import sangria.marshalling.queryAst._

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SchemaDefinitionSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  "Schema" should {
    "collect all reachable types in `additionalTypes`" in {
      val CustomScalarType = ScalarType[String](
        "CustomScalar",
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

      val NamedType = InterfaceType(
        "Named",
        fields[Unit, Unit](
          Field("name", OptionType(StringType), resolve = _ => None),
          Field("custom", OptionType(CustomScalarType), resolve = _ => None)))

      val DogType = ObjectType(
        "Dog",
        interfaces[Unit, Unit](NamedType),
        fields[Unit, Unit](Field("barks", OptionType(BooleanType), resolve = _ => None)))

      val CatType = ObjectType(
        "Cat",
        interfaces[Unit, Unit](NamedType),
        fields[Unit, Unit](Field("meows", OptionType(BooleanType), resolve = _ => None)))

      val queryType = ObjectType(
        "Query",
        fields[Unit, Unit](Field("foo", OptionType(StringType), resolve = _ => None)))

      val schema = Schema(queryType, additionalTypes = DogType :: CatType :: Nil)

      val introspection =
        IntrospectionParser.parse(Executor.execute(schema, introspectionQuery).await).get

      val fromIntro = introspection.types.map(_.name).toSet

      schema.types.keySet should be(fromIntro)

      List(schema.types.keySet, fromIntro).foreach { typeNames =>
        typeNames should (contain("Named")
          .and(contain("Dog"))
          .and(contain("Cat"))
          .and(contain("CustomScalar")))
      }
    }

    "does not allow defining two object types with the same name and different fields" in {
      case class Foo(name: Option[String], city: Option[String])

      val FooType: ObjectType[Unit, Foo] = ObjectType(
        "Foo",
        interfaces = Nil,
        () =>
          fields(
            Field("name", OptionType(StringType), resolve = _.value.name),
            Field("city", OptionType(StringType), resolve = _.value.name)
          )
      )

      // same name of the existing one, with different fields
      val FooType1: ObjectType[Unit, Foo] = ObjectType(
        "Foo",
        interfaces = Nil,
        () => fields(Field("name", OptionType(StringType), resolve = _.value.name))
      )

      val field1 =
        fields[Unit, Unit](
          Field("foo", OptionType(FooType), resolve = _ => Some(Foo(Some("foo"), None)))
        )

      val field2 =
        fields[Unit, Unit](
          Field("foo1", OptionType(FooType1), resolve = _ => Some(Foo(Some("foo"), None)))
        )

      val queryType = ObjectType(
        name = "Query",
        interfaces = Nil,
        fieldsFn = () => field1 ++ field2
      )

      intercept[SchemaValidationException](Schema(queryType))
    }
  }
}
