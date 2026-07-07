package sangria.catseffect

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.catseffect.execution.IOExecutionScheme._
import sangria.catseffect.schema.AsyncValue._
import sangria.execution.Executor
import sangria.macros._
import sangria.macros.derive._
import sangria.marshalling.circe._
import sangria.schema._
import sangria.validation.ValueCoercionViolation

class IOExecutionSchemeDerivationSpec extends AnyWordSpec with Matchers {
  import IOExecutionSchemeDerivationSpec._

  "IOExecutionScheme" must {
    "allow deriving IO-returning fields" in {
      val query =
        gql"""
          query q1 {
            users { id name }
            firstUser { id name }
            currentUser { id name }
            userCount
            noOp
          }
        """

      val res: IO[Json] = Executor.execute(schema, query, root = new Queries)

      val expected: Json = Json.obj(
        "data" -> Json.obj(
          "users" -> Json.arr(
            Json.obj("id" -> Json.fromInt(1), "name" -> Json.fromString("Ada")),
            Json.obj("id" -> Json.fromInt(2), "name" -> Json.fromString("Grace"))
          ),
          "firstUser" -> Json.obj("id" -> Json.fromInt(1), "name" -> Json.fromString("Ada")),
          "currentUser" -> Json.obj("id" -> Json.fromInt(2), "name" -> Json.fromString("Grace")),
          "userCount" -> Json.fromInt(2),
          "noOp" -> Json.fromString("OK")
        )
      )

      res.unsafeRunSync() must be(expected)
    }
  }
}

object IOExecutionSchemeDerivationSpec {
  case object UnitCoercionViolation extends ValueCoercionViolation("Unit value is not input")

  private case class User(id: Int, name: String)

  private implicit val UnitType: ScalarType[Unit] = ScalarType[Unit](
    "Unit",
    coerceOutput = (_, _) => "OK",
    coerceUserInput = _ => Left(UnitCoercionViolation),
    coerceInput = _ => Left(UnitCoercionViolation)
  )

  private implicit val UserType: ObjectType[Unit, User] = deriveObjectType[Unit, User]()

  private class Queries {
    private val values = List(User(1, "Ada"), User(2, "Grace"))

    @GraphQLField
    def users: IO[List[User]] = IO.pure(values)

    @GraphQLField
    def firstUser: IO[Option[User]] = IO.pure(values.headOption)

    @GraphQLField
    def currentUser: IO[User] = IO.pure(values(1))

    @GraphQLField
    def userCount: IO[Int] = IO.pure(values.size)

    @GraphQLField
    def noOp: IO[Unit] = IO.unit
  }

  private val QueryType: ObjectType[Unit, Queries] = deriveObjectType[Unit, Queries]()
  private val schema = Schema(QueryType)
}
