package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.util.{DebugUtil, FutureResultSupport}
import sangria.schema._
import sangria.macros._
import sangria.macros.derive._
import scala.concurrent.ExecutionContext.Implicits.global

case class UserId(id: String) extends AnyVal

class ScalarAliasSpec extends WordSpec with Matchers with FutureResultSupport {
  case class User(id: UserId, name: String)

  "ScalarAlias" should {
    "represent value class as scalar type" in {
      implicit val UserIdType = ScalarAlias[UserId, String](StringType, _.id, id ⇒ Right(UserId(id)))

      val UserType = deriveObjectType[Unit, User]()

      val UserIdArg = Argument("id", UserIdType)

      val schema = Schema(ObjectType("Query", fields[Unit, Unit](
        Field("user", UserType,
          arguments = UserIdArg :: Nil,
          resolve = _.withArgs(UserIdArg)(userId ⇒ User(userId, "generated")))
      )))

      val query =
        graphql"""
          {
            user(id: "1234") {
              id
              name
            }

            __type(name: "User") {
              name
              fields {
                name

                type {
                  kind
                  ofType {
                    kind
                    name
                  }
                }
              }
            }
          }
        """

      Executor.execute(schema, query).await should be (
        Map(
          "data" → Map(
            "user" → Map(
              "id" → "1234",
              "name" → "generated"),
            "__type" → Map(
              "name" → "User",
              "fields" → Vector(
                Map(
                  "name" → "id",
                  "type" → Map(
                    "kind" → "NON_NULL",
                    "ofType" → Map(
                      "kind" → "SCALAR",
                      "name" → "String"))),
                Map(
                  "name" → "name",
                  "type" → Map(
                    "kind" → "NON_NULL",
                    "ofType" → Map(
                      "kind" → "SCALAR",
                      "name" → "String"))))))))
    }
  }
}