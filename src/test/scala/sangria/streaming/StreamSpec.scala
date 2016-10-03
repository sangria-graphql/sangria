package sangria.streaming

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.{ActorMaterializer, Materializer}
import akka.stream.scaladsl.Source
import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.util.FutureResultSupport
import sangria.schema._
import sangria.macros._

import scala.concurrent.ExecutionContext.Implicits.global

class StreamSpec extends WordSpec with Matchers with FutureResultSupport {
  "Stream based subscriptions" should  {
    "Stream results" in {
      import sangria.marshalling.sprayJson._
      import spray.json._

      import sangria.streaming.akkaStreams._


      implicit val sys = ActorSystem("tests")
      implicit val mat: Materializer = ActorMaterializer()

      val QueryType = ObjectType("QueryType", fields[Unit, Unit](
        Field("hello", StringType, resolve = _ ⇒ "world")
      ))

      val SubscriptionType = ObjectType("Subscription", fields[Unit, Unit](
        Field.subs("letters", StringType, resolve = _ ⇒
          Source.fromIterator(() ⇒ Iterator("a", "b").map(action(_)))),

        Field.subs("numbers", OptionType(IntType), resolve = _ ⇒
          Source.fromIterator(() ⇒ Iterator(1, 2)).map(action(_)))
      ))

      val schema = Schema(QueryType, subscription = Some(SubscriptionType))

      import sangria.execution.ExecutionScheme.Stream

      val stream: Source[JsValue, NotUsed] =
        Executor.execute(schema, graphql"subscription { letters numbers }")

      val result = stream.runFold(List.empty[JsValue]){(acc, r) ⇒ acc :+ r}.await

      result should (
        have(size(4)) and
        contain("""{"data": {"letters": "a"}}""".parseJson) and
        contain("""{"data": {"letters": "b"}}""".parseJson) and
        contain("""{"data": {"numbers": 1}}""".parseJson) and
        contain("""{"data": {"numbers": 2}}""".parseJson))
    }
  }

}
