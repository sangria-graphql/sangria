package sangria.catseffect

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.execution.Executor
import sangria.catseffect.execution.IOExecutionScheme._
import sangria.macros._
import sangria.marshalling.circe._
import sangria.schema._

/** The integration with [[cats.effect.IO]] is far from being complete for now.
  */
class IOExecutionSchemeSpec extends AnyWordSpec with Matchers {

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
  }
}

object IOExecutionSchemeSpec {
  import sangria.catseffect.schema.AsyncValue._
  private val QueryType: ObjectType[Unit, Unit] = ObjectType(
    "Query",
    () =>
      fields[Unit, Unit](
        Field("ids", ListType(IntType), resolve = _ => List(1, 2)),
        Field(
          "parent",
          StringType,
          resolve = { _ =>
            (for {
              value <- IO(Option("hello"))
            } yield value match {
              case Some(value) => IO.pure(value)
              case None => IO.raiseError(new Exception("No value"))
            }).flatten: IO[String]
          }
        )
      )
  )

  private val schema = Schema(QueryType)
}
