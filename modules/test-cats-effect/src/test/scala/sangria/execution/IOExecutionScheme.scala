package sangria.execution

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.macros._
import sangria.marshalling.circe._
import sangria.schema._

import scala.concurrent.{ExecutionContext, Future}

/** The integration with [[cats.effect.IO]] is far from being complete for now.
  */
class IOExecutionScheme extends AnyWordSpec with Matchers {
  private implicit val ec: ExecutionContext = global.compute
  private val ioEffectOps = new EffectOps[IO] {
    override def failed[Ctx, Res](error: Throwable): IO[Res] = IO.raiseError(error)
    override def flatMapFuture[Res, T](future: Future[T])(resultFn: T => IO[Res]): IO[Res] =
      IO.fromFuture(IO(future)).flatMap(resultFn)
    override def map[T, Out](in: Future[T])(f: T => Out): IO[Out] = IO.fromFuture(IO(in)).map(f)
  }
  private implicit val ioExecutionScheme: EffectBasedExecutionScheme[IO] =
    new EffectBasedExecutionScheme[IO](ioEffectOps, FutureResolverBuilder)

  import IOExecutionScheme._
  "IOExecutionScheme" must {
    "allow using IO effect" in {
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
  }
}

object IOExecutionScheme {
  private val QueryType: ObjectType[Unit, Unit] = ObjectType(
    "Query",
    () =>
      fields[Unit, Unit](
        Field("ids", ListType(IntType), resolve = _ => List(1, 2))
      ))

  val schema = Schema(QueryType)
}
