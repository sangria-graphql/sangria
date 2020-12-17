package sangria.execution

import sangria.macros._
import sangria.marshalling.ScalaInput
import sangria.schema._
import sangria.util.{DebugUtil, FutureResultSupport}
import sangria.validation.{ValueCoercionViolation, Violation}

import scala.concurrent.ExecutionContext.Implicits.global
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class ScalarMiddlewareSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  case class IDEncodingViolation(error: String) extends ValueCoercionViolation(error)

  class Ctx(prefix: String) {
    def encodeId(id: String): String = prefix + id
    def decodeId(id: String): Either[Violation, String] =
      if (id.startsWith(prefix))
        Right(id.substring(prefix.length))
      else
        Left(IDEncodingViolation("invalid id"))
  }

  val EncodedIdType = ScalarAlias[String, String](StringType, identity, id => Right(id))

  val ComplexInputType = InputObjectType(
    "Complex",
    List(InputField("userId", OptionInputType(EncodedIdType)), InputField("name", StringType)))

  val ComplexInputWithDefaultType = InputObjectType(
    "Complex",
    List(
      InputField("userId", OptionInputType(EncodedIdType), defaultValue = "INPUT_ID"),
      InputField("name", StringType)))

  val ComplexInputWithValidDefaultType = InputObjectType(
    "Complex",
    List(
      InputField("userId", OptionInputType(EncodedIdType), defaultValue = "test-INPUT_ID"),
      InputField("name", StringType)))

  val IdArg = Argument("id", EncodedIdType)
  val IdArgWithDefault = Argument("id", OptionInputType(EncodedIdType), defaultValue = "SOME_ID")
  val IdArgWithValidDefault =
    Argument("id", OptionInputType(EncodedIdType), defaultValue = "test-SOME_ID")
  val ComplexArg = Argument("c", ComplexInputType)
  val ComplexArgWithDefault = Argument("c", ComplexInputWithDefaultType)
  val ComplexArgWithValidDefault = Argument("c", ComplexInputWithValidDefaultType)

  class IdEncodingMiddleware
      extends Middleware[Ctx]
      with MiddlewareFromScalar[Ctx]
      with MiddlewareToScalar[Ctx] {
    type QueryVal = Unit

    def beforeQuery(context: MiddlewareQueryContext[Ctx, _, _]) = ()
    def afterQuery(queryVal: Unit, context: MiddlewareQueryContext[Ctx, _, _]) = ()

    def fromScalar(value: Any, inputType: InputType[_], ctx: Ctx) =
      inputType match {
        case EncodedIdType => Some(ctx.decodeId(value.asInstanceOf[String]))
        case _ => None
      }

    def toScalar(value: Any, inputType: InputType[_], ctx: Ctx) =
      inputType match {
        case EncodedIdType => Some(ctx.encodeId(value.asInstanceOf[String]))
        case _ => None
      }
  }

  "Scalar-based middleware traits" should {
    "encode and decode scalar value" in {
      val schema = Schema(
        ObjectType(
          "Query",
          fields[Ctx, Unit](
            Field(
              "test",
              OptionType(EncodedIdType),
              arguments = IdArg :: ComplexArg :: Nil,
              resolve = _.withArgs(IdArg, ComplexArg)((id, complex) =>
                id + "-" + complex("userId").asInstanceOf[Option[UserId]].get + "-" + complex(
                  "name"))
            )
          )
        ))

      val query =
        graphql"""
          query Test($$id: String!, $$c: Complex!) {
            t1: test(id: "test-a", c: {userId: "test-b", name: "foo"})
            t2: test(id: $$id, c: $$c)
            t3: test(id: "invalid", c: {userId: "yay", name: "foo"})
          }
        """

      val ctx = new Ctx("test-")

      val vars = ScalaInput.scalaInput(
        Map("id" -> "test-c", "c" -> Map("userId" -> "test-d", "name" -> "bar")))

      val middleware = new IdEncodingMiddleware :: Nil

      Executor
        .execute(schema, query, ctx, variables = vars, middleware = middleware)
        .await should be(
        Map(
          "data" -> Map("t1" -> "test-a-b-foo", "t2" -> "test-c-d-bar", "t3" -> null),
          "errors" -> Vector(
            Map(
              "message" -> "Argument 'id' has wrong value: invalid id. (line 5, column 13):\n            t3: test(id: \"invalid\", c: {userId: \"yay\", name: \"foo\"})\n            ^\n (line 5, column 26):\n            t3: test(id: \"invalid\", c: {userId: \"yay\", name: \"foo\"})\n                         ^",
              "path" -> Vector("t3"),
              "locations" -> Vector(
                Map("line" -> 5, "column" -> 13),
                Map("line" -> 5, "column" -> 26))
            ),
            Map(
              "message" -> "Field 'c.userId' has wrong value: invalid id. (line 5, column 13):\n            t3: test(id: \"invalid\", c: {userId: \"yay\", name: \"foo\"})\n            ^\n (line 5, column 49):\n            t3: test(id: \"invalid\", c: {userId: \"yay\", name: \"foo\"})\n                                                ^",
              "path" -> Vector("t3"),
              "locations" -> Vector(
                Map("line" -> 5, "column" -> 13),
                Map("line" -> 5, "column" -> 49))
            )
          )
        ))
    }

    "encode and decode scalar value when argument has default value" in {
      val schema = Schema(
        ObjectType(
          "Query",
          fields[Ctx, Unit](
            Field(
              "test",
              OptionType(EncodedIdType),
              arguments = IdArgWithDefault :: ComplexArgWithDefault :: Nil,
              resolve = _.withArgs(IdArgWithDefault, ComplexArgWithDefault)((id, complex) =>
                id + "-" + complex("userId").asInstanceOf[Option[UserId]].get + "-" + complex(
                  "name"))
            )
          )
        ))

      val query =
        graphql"""
          query Test($$id: String!, $$c: Complex!) {
            t1: test(id: "test-a", c: {userId: "test-b", name: "foo"})
            t2: test(id: $$id, c: $$c)
            t3: test(id: "invalid", c: {userId: "test-yay", name: "foo"})
            t4: test(id: "test-valid", c: {userId: "yay", name: "foo"})
          }
        """

      val ctx = new Ctx("test-")

      val vars = ScalaInput.scalaInput(
        Map("id" -> "test-c", "c" -> Map("userId" -> "test-d", "name" -> "bar")))

      val middleware = new IdEncodingMiddleware :: Nil

      Executor
        .execute(schema, query, ctx, variables = vars, middleware = middleware)
        .await should be(
        Map(
          "data" -> Map("t1" -> "test-a-b-foo", "t2" -> "test-c-d-bar", "t3" -> null, "t4" -> null),
          "errors" -> Vector(
            Map(
              "message" -> "Argument 'id' has wrong value: invalid id. (line 5, column 13):\n            t3: test(id: \"invalid\", c: {userId: \"test-yay\", name: \"foo\"})\n            ^\n (line 5, column 26):\n            t3: test(id: \"invalid\", c: {userId: \"test-yay\", name: \"foo\"})\n                         ^",
              "path" -> Vector("t3"),
              "locations" -> Vector(
                Map("line" -> 5, "column" -> 13),
                Map("line" -> 5, "column" -> 26))
            ),
            Map(
              "message" -> "Field 'c.userId' has wrong value: invalid id. (line 6, column 13):\n            t4: test(id: \"test-valid\", c: {userId: \"yay\", name: \"foo\"})\n            ^\n (line 6, column 52):\n            t4: test(id: \"test-valid\", c: {userId: \"yay\", name: \"foo\"})\n                                                   ^",
              "path" -> Vector("t4"),
              "locations" -> Vector(
                Map("line" -> 6, "column" -> 13),
                Map("line" -> 6, "column" -> 52))
            )
          )
        ))
    }

    "applies to valid default values" in {
      val schema = Schema(
        ObjectType(
          "Query",
          fields[Ctx, Unit](
            Field(
              "test",
              OptionType(EncodedIdType),
              arguments = IdArgWithValidDefault :: ComplexArgWithValidDefault :: Nil,
              resolve =
                _.withArgs(IdArgWithValidDefault, ComplexArgWithValidDefault)((id, complex) =>
                  id + "-" + complex("userId").asInstanceOf[Option[UserId]].get + "-" + complex(
                    "name"))
            )
          )
        ))

      val query =
        graphql"""
          query Test($$id: String = "test-ID1", $$c: Complex = {userId: "test-ID2", name: "foo"}) {
            t1: test(c: {name: "bar"})
            t2: test(id: $$id, c: $$c)
          }
        """

      val ctx = new Ctx("test-")

      val middleware = new IdEncodingMiddleware :: Nil

      Executor.execute(schema, query, ctx, middleware = middleware).await should be(
        Map("data" -> Map("t1" -> "test-SOME_ID-INPUT_ID-bar", "t2" -> "test-ID1-ID2-foo")))
    }

    "applies to invalid default values" in {
      val schema = Schema(
        ObjectType(
          "Query",
          fields[Ctx, Unit](
            Field(
              "test",
              OptionType(EncodedIdType),
              arguments = IdArgWithDefault :: ComplexArgWithDefault :: Nil,
              resolve = _.withArgs(IdArgWithDefault, ComplexArgWithDefault)((id, complex) =>
                id + "-" + complex("userId").asInstanceOf[Option[UserId]].get + "-" + complex(
                  "name"))
            )
          )
        ))

      val query =
        graphql"""
          query Test($$id: String = "ID1", $$c: Complex = {userId: "ID2", name: "foo"}) {
            t2: test(id: $$id, c: $$c)
            t3: test(c: {userId: "test-yay", name: "foo"})
            t4: test(id: "test-valid", c: {name: "foo"})
          }
        """

      val ctx = new Ctx("test-")

      val middleware = new IdEncodingMiddleware :: Nil

      Executor.execute(schema, query, ctx, middleware = middleware).await should be(
        Map(
          "data" -> Map("t2" -> null, "t3" -> null, "t4" -> null),
          "errors" -> Vector(
            Map(
              "message" -> "Field '$id' has wrong value: invalid id. (line 2, column 22):\n          query Test($id: String = \"ID1\", $c: Complex = {userId: \"ID2\", name: \"foo\"}) {\n                     ^\n (line 2, column 36):\n          query Test($id: String = \"ID1\", $c: Complex = {userId: \"ID2\", name: \"foo\"}) {\n                                   ^",
              "path" -> Vector("t2"),
              "locations" -> Vector(
                Map("line" -> 2, "column" -> 22),
                Map("line" -> 2, "column" -> 36))
            ),
            Map(
              "message" -> "Field '$c.userId' has wrong value: invalid id. (line 2, column 43):\n          query Test($id: String = \"ID1\", $c: Complex = {userId: \"ID2\", name: \"foo\"}) {\n                                          ^\n (line 2, column 66):\n          query Test($id: String = \"ID1\", $c: Complex = {userId: \"ID2\", name: \"foo\"}) {\n                                                                 ^",
              "path" -> Vector("t2"),
              "locations" -> Vector(
                Map("line" -> 2, "column" -> 43),
                Map("line" -> 2, "column" -> 66))
            ),
            Map(
              "message" -> "Argument 'id' has wrong value: invalid id. (line 4, column 13):\n            t3: test(c: {userId: \"test-yay\", name: \"foo\"})\n            ^",
              "path" -> Vector("t3"),
              "locations" -> Vector(Map("line" -> 4, "column" -> 13))
            ),
            Map(
              "message" -> "Argument 'c.userId' has wrong value: invalid id. (line 5, column 13):\n            t4: test(id: \"test-valid\", c: {name: \"foo\"})\n            ^",
              "path" -> Vector("t4"),
              "locations" -> Vector(Map("line" -> 5, "column" -> 13))
            )
          )
        ))
    }
  }
}
