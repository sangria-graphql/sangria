package sangria.execution

import java.util.UUID

import eu.timepit.refined._
import eu.timepit.refined.numeric._
import eu.timepit.refined.api.Refined
import org.scalatest.{Matchers, WordSpec}
import sangria.util.{DebugUtil, FutureResultSupport}
import sangria.schema._
import sangria.macros._
import sangria.macros.derive._
import sangria.marshalling.ScalaInput
import sangria.validation.{AstNodeViolation, ValueCoercionViolation}

import scala.concurrent.ExecutionContext.Implicits.global

case class UserId(id: String) extends AnyVal

class ScalarAliasSpec extends WordSpec with Matchers with FutureResultSupport {
  case class User(id: UserId, id2: Option[UserId], name: String, num: Int Refined Positive)

  case class RefineViolation(error: String) extends ValueCoercionViolation(error)

  implicit val UserIdType = ScalarAlias[UserId, String](
    StringType, _.id, id ⇒ Right(UserId(id)))

  implicit val PositiveIntType = ScalarAlias[Int Refined Positive, Int](
    IntType, _.value, i ⇒ refineV[Positive](i).left.map(RefineViolation))

  case object IDViolation extends ValueCoercionViolation("Invalid ID")

  val UUIDType = ScalarAlias[UUID, String](StringType,
    toScalar = _.toString,
    fromScalar = idString ⇒ try Right(UUID.fromString(idString)) catch {
      case _: IllegalArgumentException ⇒ Left(IDViolation)
    })

  val UserType = deriveObjectType[Unit, User]()

  val ComplexInputType = InputObjectType("Complex", List(
    InputField("userId", OptionInputType(UserIdType), defaultValue = "5678"),
    InputField("userNum", OptionInputType(PositiveIntType))))

  val UserIdArg = Argument("id", UserIdType)
  val NumArg = Argument("n", PositiveIntType)
  val ComplexArg = Argument("c", ComplexInputType)
  val UUIDArg = Argument("id", UUIDType)

  "ScalarAlias" should {
    "represent value class as scalar type" in {
      val schema = Schema(ObjectType("Query", fields[Unit, Unit](
        Field("user", UserType,
          arguments = UserIdArg :: NumArg :: ComplexArg :: Nil,
          resolve = _.withArgs(UserIdArg, NumArg, ComplexArg)(
            (userId, num, complex) ⇒ User(userId, complex("userId").asInstanceOf[Option[UserId]], "generated", num))),
        Field("idTest", UUIDType,
          arguments = UUIDArg :: Nil,
          resolve = c ⇒ {
            val uuid: UUID = c.arg(UUIDArg)
            uuid
          })
      )))

      val query =
        graphql"""
          {
            user(id: "1234", n: 42, c: {userNum: 500}) {
              id
              id2
              name
              num
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
              "id2" → "5678",
              "name" → "generated",
              "num" → 42),
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
                  "name" → "id2",
                  "type" → Map(
                    "kind" → "SCALAR",
                    "ofType" → null)),
                Map(
                  "name" → "name",
                  "type" → Map(
                    "kind" → "NON_NULL",
                    "ofType" → Map(
                      "kind" → "SCALAR",
                      "name" → "String"))),
                Map(
                  "name" → "num",
                  "type" → Map(
                    "kind" → "NON_NULL",
                    "ofType" → Map(
                      "kind" → "SCALAR",
                      "name" → "Int"))))))))
    }

    "represent correct transforms UUID values" in {
//      val schema = Schema(ObjectType("Query", fields[Unit, Unit](
//        Field("idTest", UUIDType,
//          arguments = UUIDArg :: Nil,
//          resolve = c ⇒ {
//            val uuid: UUID = c.arg(UUIDArg)
//            uuid
//          })
//      )))
//
//      val schema1 =
//        schema.extend(gql"""
//          extend type Query {
//            foo: Int
//          }
//        """)
//
//      val query =
//        gql"""
//          query Test($$id: String!){
//            idTest(id: $$id)
//          }
//        """
//
//      val vars = ScalaInput.scalaInput(Map("id" → "f28efae0-8808-4514-b356-02808d4e936c"))
//
//      DebugUtil.prettyPrint(Executor.execute(schema1, query, variables = vars).await)
    }

    "coerces input types correctly" in {
      val schema = Schema(ObjectType("Query", fields[Unit, Unit](
        Field("user", UserType,
          arguments = UserIdArg :: NumArg :: ComplexArg :: Nil,
          resolve = _.withArgs(UserIdArg, NumArg, ComplexArg)(
            (userId, num, complex) ⇒ User(userId, complex("userId").asInstanceOf[Option[UserId]], "generated " + complex, num)))
      )))

      val query =
        graphql"""
          {
            user(id: "1234", n: -123, c: {userId: 1, userNum: -5}) {
              id
              name
            }
          }
        """

      val error = intercept [ValidationError] (Executor.execute(schema, query).await)

      val violations = error.violations.map {
        case a: AstNodeViolation ⇒ a.simpleErrorMessage
        case o ⇒ o.errorMessage
      }

      violations should (
        have(size(3)) and
        contain("Argument 'n' expected type 'Int!' but got: -123. Reason: Predicate failed: (-123 > 0).") and
        contain("Argument 'c' expected type 'Complex!' but got: {userId: 1, userNum: -5}. Reason: [in field 'userId'] String value expected") and
        contain("Argument 'c' expected type 'Complex!' but got: {userId: 1, userNum: -5}. Reason: [in field 'userNum'] Predicate failed: (-5 > 0).")
      )
    }
  }
}