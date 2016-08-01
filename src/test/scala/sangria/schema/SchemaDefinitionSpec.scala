package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.execution.Executor
import sangria.validation.StringCoercionViolation
import sangria.introspection.{IntrospectionParser, introspectionQuery}
import sangria.util.FutureResultSupport
import sangria.marshalling.queryAst._
import sangria.parser.DeliveryScheme.Throw

import scala.concurrent.ExecutionContext.Implicits.global

class SchemaDefinitionSpec extends WordSpec with Matchers with FutureResultSupport {
  "Schema" should {
    "collect all reachable types in `additionalTypes`" in {
      val CustomScalarType = ScalarType[String]("CustomScalar",
        coerceOutput = valueOutput,
        coerceUserInput = {
          case s: String ⇒ Right(s)
          case _ ⇒ Left(StringCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _, _) ⇒ Right(s)
          case _ ⇒ Left(StringCoercionViolation)
        })

      val NamedType = InterfaceType("Named", fields[Unit, Unit](
        Field("name", OptionType(StringType), resolve = _ ⇒ None),
        Field("custom", OptionType(CustomScalarType), resolve = _ ⇒ None)))

      val DogType = ObjectType("Dog", interfaces[Unit, Unit](NamedType), fields[Unit, Unit](
        Field("barks", OptionType(BooleanType), resolve = _ ⇒ None)))

      val CatType = ObjectType("Cat", interfaces[Unit, Unit](NamedType), fields[Unit, Unit](
        Field("meows", OptionType(BooleanType), resolve = _ ⇒ None)))

      val queryType = ObjectType("Query", fields[Unit, Unit](
        Field("foo", OptionType(StringType), resolve = _ ⇒ None)))

      val schema = Schema(queryType, additionalTypes = DogType :: CatType :: Nil)

      val introspection = IntrospectionParser.parse(Executor.execute(schema, introspectionQuery).await)

      val fromIntro = introspection.types.map(_.name).toSet

      schema.types.keySet should be (fromIntro)

      List(schema.types.keySet, fromIntro) foreach { typeNames ⇒
        typeNames should (
          contain("Named") and
          contain("Dog") and
          contain("Cat") and
          contain("CustomScalar"))
      }
    }
  }
}
