package sangria.catseffect

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.execution.Executor
import sangria.catseffect.execution.IOExecutionScheme._
import sangria.catseffect.schema.AsyncValue._
import sangria.macros._
import sangria.marshalling.circe._
import sangria.schema._

import scala.concurrent.ExecutionContext

/** The integration with [[cats.effect.IO]] is far from being complete for now.
  */
class IOExecutionSchemeSpec extends AnyWordSpec with Matchers {
  implicit val ec: ExecutionContext = ExecutionContext.global

  import IOExecutionSchemeSpec._
  "IOExecutionScheme" must {
    "allow using IO effect with pure resolve" in {
      val query = gql"""
        query q1 {
          ids
        }
      """
      val res: IO[Json] = Executor.execute(schema, query)

      val expected: Json = Json.obj(
        "data" -> Json.obj(
          "ids" -> Json.arr(
            Json.fromInt(1),
            Json.fromInt(2)
          )
        )
      )
      res.unsafeRunSync() must be(expected)
    }

    "allow using IO effect with IO resolve" in {
      val query =
        gql"""
          query q1 {
            parent
          }
        """
      val res: IO[Json] = Executor.execute(schema, query)

      val expected: Json = Json.obj(
        "data" -> Json.obj(
          "parent" -> Json.fromString("hello")
        )
      )
      res.unsafeRunSync() must be(expected)
    }

    "allow using IO effect in mutation with IO resolve" in {
      val query = gql""" mutation q1 { parent } """
      val res: IO[Json] = Executor.execute(schema, query)

      val expected: Json = Json.obj("data" -> Json.obj("parent" -> Json.fromString("hello")))
      res.unsafeRunSync() must be(expected)
    }

    "actually run the IO effect during resolution rather than eagerly on schema construction" in {
      val counter = new java.util.concurrent.atomic.AtomicInteger(0)
      val queryType: ObjectType[Unit, Unit] = ObjectType(
        "Query",
        () =>
          fields[Unit, Unit](Field("count", IntType, resolve = _ => IO(counter.incrementAndGet()))))
      val lazySchema = Schema(queryType)
      val query = gql""" query { count } """

      counter.get() must be(0)

      val res: IO[Json] = Executor.execute(lazySchema, query)
      counter.get() must be(0)

      res.unsafeRunSync() must be(Json.obj("data" -> Json.obj("count" -> Json.fromInt(1))))
      counter.get() must be(1)
    }

    "pass field arguments to an IO resolve function" in {
      val query = gql""" query { greet(name: "Ada") } """
      val res: IO[Json] = Executor.execute(schema, query)

      val expected: Json = Json.obj("data" -> Json.obj("greet" -> Json.fromString("Hello, Ada!")))
      res.unsafeRunSync() must be(expected)
    }

    "resolve nested object fields that each use IO" in {
      val query =
        gql"""
          query q1 {
            child {
              value
            }
          }
        """
      val res: IO[Json] = Executor.execute(schema, query)

      val expected: Json = Json.obj(
        "data" -> Json.obj(
          "child" -> Json.obj("value" -> Json.fromString("child-value"))
        )
      )
      res.unsafeRunSync() must be(expected)
    }

    "resolve a list-typed field via a single IO of a list" in {
      val query =
        gql"""
          query q1 {
            items
          }
        """
      val res: IO[Json] = Executor.execute(schema, query)

      val expected: Json = Json.obj(
        "data" -> Json.obj(
          "items" -> Json.arr(Json.fromString("a"), Json.fromString("b"), Json.fromString("c")))
      )
      res.unsafeRunSync() must be(expected)
    }

    "resolve a list-typed field where each item is independently resolved via its own IO" in {
      val query =
        gql"""
          query q1 {
            sequencedItems
          }
        """
      val res: IO[Json] = Executor.execute(schema, query)

      val expected: Json = Json.obj(
        "data" -> Json.obj(
          "sequencedItems" -> Json
            .arr(Json.fromString("a"), Json.fromString("b"), Json.fromString("c")))
      )
      res.unsafeRunSync() must be(expected)
    }

    "surface a user-facing error raised from an IO resolve function" in {
      val query = gql""" query { boom } """
      val res: IO[Json] = Executor.execute(schema, query)

      val json = res.unsafeRunSync()
      (json \\ "data").head must be(Json.obj("boom" -> Json.Null))

      val errors = (json \\ "errors").head.asArray.get
      errors must have size 1
      errors.head.hcursor.get[String]("message").toOption must be(Some("boom happened"))
    }

    "mask a non user-facing error raised from an IO resolve function" in {
      val query = gql""" query { explode } """
      val res: IO[Json] = Executor.execute(schema, query)

      val json = res.unsafeRunSync()
      (json \\ "data").head must be(Json.obj("explode" -> Json.Null))

      val errors = (json \\ "errors").head.asArray.get
      errors must have size 1
      errors.head.hcursor.get[String]("message").toOption must be(Some("Internal server error"))
    }
  }
}

object IOExecutionSchemeSpec {
  private case class BoomException(message: String)
      extends Exception(message)
      with sangria.execution.UserFacingError {
    override def getMessage(): String = message
  }

  private case class ChildType(value: String)

  private val ChildObjectType: ObjectType[Unit, ChildType] = ObjectType(
    "Child",
    () =>
      fields[Unit, ChildType](
        Field("value", StringType, resolve = c => IO(c.value.value))
      ))

  private val NameArg: Argument[String] = Argument("name", StringType)

  private val QueryType: ObjectType[Unit, Unit] = ObjectType(
    "Query",
    () =>
      fields[Unit, Unit](
        Field("ids", ListType(IntType), resolve = _ => List(1, 2)),
        Field("parent", StringType, resolve = _ => IO("hello")),
        Field(
          "greet",
          StringType,
          arguments = NameArg :: Nil,
          resolve = c => IO(s"Hello, ${c.arg(NameArg)}!")),
        Field("child", ChildObjectType, resolve = _ => IO(ChildType("child-value"))),
        Field("items", ListType(StringType), resolve = _ => IO(List("a", "b", "c"))),
        Field(
          "sequencedItems",
          ListType(StringType),
          resolve = _ =>
            Action.sequence(List(IO("a"), IO("b"), IO("c")).map(v => v: LeafAction[Unit, String]))),
        Field(
          "boom",
          OptionType(StringType),
          resolve = _ => IO.raiseError(BoomException("boom happened"))),
        Field(
          "explode",
          OptionType(StringType),
          resolve = _ => IO.raiseError(new RuntimeException("should not leak")))
      )
  )

  private val Mutation: ObjectType[Unit, Unit] = ObjectType(
    "Mutation",
    () => fields[Unit, Unit](Field("parent", StringType, resolve = _ => IO("hello"))))

  private val schema = Schema(QueryType, Some(Mutation))
}
