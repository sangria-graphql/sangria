package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.macros._
import sangria.marshalling.ScalaInput
import sangria.schema._
import sangria.util.{DebugUtil, FutureResultSupport}
import sangria.validation.{ValueCoercionViolation, Violation}

import scala.concurrent.ExecutionContext.Implicits.global

class ScalarMiddlewareSpec extends WordSpec with Matchers with FutureResultSupport {
  case class IDEncodingViolation(error: String) extends ValueCoercionViolation(error)

  class Ctx(prefix: String) {
    def encodeId(id: String): String = prefix + id
    def decodeId(id: String): Either[Violation, String] =
      if (id.startsWith(prefix))
        Right(id.substring(prefix.length))
      else
        Left(IDEncodingViolation("invalid id"))
  }

  val EncodedIdType = ScalarAlias[String, String](
    StringType, identity, id ⇒ Right(id))

  val ComplexInputType = InputObjectType("Complex", List(
    InputField("userId", OptionInputType(EncodedIdType)),
    InputField("name", StringType)))

  val IdArg = Argument("id", EncodedIdType)
  val ComplexArg = Argument("c", ComplexInputType)

  class IdEncodingMiddleware extends Middleware[Ctx] with MiddlewareFromScalar[Ctx] with MiddlewareToScalar[Ctx] {
    type QueryVal = Unit

    def beforeQuery(context: MiddlewareQueryContext[Ctx, _, _]) = ()
    def afterQuery(queryVal: Unit, context: MiddlewareQueryContext[Ctx, _, _]) = ()

    def fromScalar(value: Any, inputType: InputType[_], ctx: Ctx) = {
      inputType match {
        case EncodedIdType ⇒ Some(ctx.decodeId(value.asInstanceOf[String]))
        case _ ⇒ None
      }
    }

    def toScalar(value: Any, inputType: InputType[_], ctx: Ctx) =
      inputType match {
        case EncodedIdType ⇒ Some(ctx.encodeId(value.asInstanceOf[String]))
        case _ ⇒ None
      }
  }

  "Scalar-based middleware traits" should {
    "encode and decode scalar value" in {
      val schema = Schema(ObjectType("Query", fields[Ctx, Unit](
        Field("test", OptionType(EncodedIdType),
          arguments = IdArg :: ComplexArg :: Nil,
          resolve = _.withArgs(IdArg, ComplexArg)(
            (id, complex) ⇒ id + "-" + complex("userId").asInstanceOf[Option[UserId]].get + "-" + complex("name")))
      )))

      val query =
        graphql"""
          query Test($$id: String!, $$c: Complex!) {
            t1: test(id: "test-a", c: {userId: "test-b", name: "foo"})
            t2: test(id: $$id, c: $$c)
            t3: test(id: "invalid", c: {userId: "yay", name: "foo"})
          }
        """

      val ctx = new Ctx("test-")

      val vars = ScalaInput.scalaInput(Map(
        "id" → "test-c",
        "c" → Map(
          "userId" → "test-d",
          "name" → "bar")))

      val middleware = new IdEncodingMiddleware :: Nil

      Executor.execute(schema, query, ctx, variables = vars, middleware = middleware).await should be (
        Map(
          "data" → Map(
            "t1" → "test-a-b-foo",
            "t2" → "test-c-d-bar",
            "t3" → null),
          "errors" → Vector(
            Map(
              "message" → "Field 'id' has wrong value: invalid id. (line 5, column 26):\n            t3: test(id: \"invalid\", c: {userId: \"yay\", name: \"foo\"})\n                         ^",
              "path" → Vector("t3"),
              "locations" → Vector(Map("line" → 5, "column" → 26))),
            Map(
              "message" → "Field 'c.userId' has wrong value: invalid id. (line 5, column 49):\n            t3: test(id: \"invalid\", c: {userId: \"yay\", name: \"foo\"})\n                                                ^",
              "path" → Vector("t3"),
              "locations" → Vector(Map("line" → 5, "column" → 49))))))
    }
  }
}