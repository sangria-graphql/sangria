package sangria.streaming

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.deferred.{DeferredResolver, Fetcher, HasId}
import sangria.execution.{Executor, HandledException}
import sangria.util.{DebugUtil, FutureResultSupport}
import sangria.schema._
import sangria.macros._
import sangria.macros.derive._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class StreamSpec extends WordSpec with Matchers with FutureResultSupport {
  "Stream based subscriptions" should  {
    "Stream results with akka-streams" in {
      import akka.NotUsed
      import akka.actor.ActorSystem
      import akka.stream.{ActorMaterializer, Materializer}
      import akka.stream.scaladsl.Source

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

    "Stream results with rxscala" in {
      import sangria.marshalling.sprayJson._
      import spray.json._
      import rx.lang.scala.Observable

      import sangria.streaming.rxscala._

      import scala.concurrent.ExecutionContext.Implicits.global

      val QueryType = ObjectType("QueryType", fields[Unit, Unit](
        Field("hello", StringType, resolve = _ ⇒ "world")
      ))

      val SubscriptionType = ObjectType("Subscription", fields[Unit, Unit](
        Field.subs("letters", StringType, resolve = _ ⇒
          Observable.from(List("a", "b").map(action(_)))),

        Field.subs("numbers", OptionType(IntType), resolve = _ ⇒
          Observable.from(List(1, 2).map(action(_))))
      ))

      val schema = Schema(QueryType, subscription = Some(SubscriptionType))

      import sangria.execution.ExecutionScheme.Stream

      val stream: Observable[JsValue] =
        Executor.execute(schema, graphql"subscription { letters numbers }")

      val result = stream.toBlocking.toList

      result should (
        have(size(4)) and
        contain("""{"data": {"letters": "a"}}""".parseJson) and
        contain("""{"data": {"letters": "b"}}""".parseJson) and
        contain("""{"data": {"numbers": 1}}""".parseJson) and
        contain("""{"data": {"numbers": 2}}""".parseJson))
    }

    "Stream results with monix" in {
      import _root_.monix.execution.Scheduler.Implicits.global
      import _root_.monix.reactive.Observable

      import sangria.marshalling.sprayJson._
      import spray.json._

      import sangria.streaming.monix._

      val QueryType = ObjectType("QueryType", fields[Unit, Unit](
        Field("hello", StringType, resolve = _ ⇒ "world")
      ))

      val SubscriptionType = ObjectType("Subscription", fields[Unit, Unit](
        Field.subs("letters", StringType, resolve = _ ⇒
          Observable("a", "b").map(action(_))),

        Field.subs("numbers", OptionType(IntType), resolve = _ ⇒
          Observable(1, 2).map(action(_)))
      ))

      val schema = Schema(QueryType, subscription = Some(SubscriptionType))

      import sangria.execution.ExecutionScheme.Stream

      val stream: Observable[JsValue] =
        Executor.execute(schema, graphql"subscription { letters numbers }")

      val result = stream.toListL.runAsync.await

      result should (
        have(size(4)) and
        contain("""{"data": {"letters": "a"}}""".parseJson) and
        contain("""{"data": {"letters": "b"}}""".parseJson) and
        contain("""{"data": {"numbers": 1}}""".parseJson) and
        contain("""{"data": {"numbers": 2}}""".parseJson))
    }

    "recover stream errors" in {
      import _root_.monix.execution.Scheduler.Implicits.global
      import _root_.monix.reactive.Observable

      import sangria.marshalling.sprayJson._
      import spray.json._

      import sangria.streaming.monix._

      val QueryType = ObjectType("QueryType", fields[Unit, Unit](
        Field("hello", StringType, resolve = _ ⇒ "world")
      ))

      val SubscriptionType = ObjectType("Subscription", fields[Unit, Unit](
        Field.subs("letters", OptionType(StringType), resolve = _ ⇒
          Observable("a", "b", "c", "d", "e").map { l ⇒
            if (l == "c") throw new IllegalStateException("foo")
            else l
          }.map(action(_))),

        Field.subs("numbers", OptionType(IntType), resolve = _ ⇒
          Observable(1, 2, 3, 4).map(action(_)))
      ))

      val schema = Schema(QueryType, subscription = Some(SubscriptionType))

      import sangria.execution.ExecutionScheme.Stream

      val exceptionHandler: Executor.ExceptionHandler = {
        case (m, e: IllegalStateException) ⇒ HandledException(e.getMessage)
      }

      val stream: Observable[JsValue] =
        Executor.execute(schema, graphql"subscription { letters numbers }", exceptionHandler = exceptionHandler)

      val result = stream.toListL.runAsync.await

      result should (
        have(size(7)) and
        contain("""{"data": {"letters": "a"}}""".parseJson) and
        contain("""{"data": {"letters": "b"}}""".parseJson) and
        contain("""{"data": {"letters": null}, "errors": [{"message": "foo", "path":["letters"]}]}""".parseJson) and
        contain("""{"data": {"numbers": 1}}""".parseJson) and
        contain("""{"data": {"numbers": 2}}""".parseJson) and
        contain("""{"data": {"numbers": 3}}""".parseJson) and
        contain("""{"data": {"numbers": 4}}""".parseJson))
    }

    "complex stream scenario" in {
      import _root_.monix.execution.Scheduler.Implicits.global
      import _root_.monix.reactive.Observable
      import _root_.monix.reactive.subjects.ReplaySubject

      import sangria.marshalling.sprayJson._
      import spray.json._

      import sangria.streaming.monix._

      case class Fruit(id: Int, name: String, color: String)

      case class FruitEaten(name: String, eater: String)
      case class FruitSmashed(id: Int)

      trait Mutation {
        this: Ctx ⇒

        @GraphQLField
        def eatFruit(name: String, eater: String): String = {
          eventBus.onNext(FruitEaten(name, eater))

          "OmNomNom"
        }

        @GraphQLField
        def smashFruit(id: Int) = {
          eventBus.onNext(FruitSmashed(id))

          "Splash!"
        }

        @GraphQLField
        def stop = {
          eventBus.onComplete()

          "Full!"
        }
      }

      class Ctx extends Mutation {
        val eventBus = ReplaySubject[Any]()
      }

      val cherryPicker = Fetcher.caching[Ctx, Fruit, Int](
        (ctx, ids) ⇒ Future.successful(ids.map(id ⇒ Fruit(id, "cherry", "red"))))(HasId(_.id))

      val FruitType = ObjectType("Fruit", fields[Unit, Fruit](
        Field("name", StringType, resolve = _.value.name),
        Field("color", StringType, resolve = _.value.color)
      ))

      val FruitEatenType = ObjectType("FruitEaten", fields[Unit, FruitEaten](
        Field("name", StringType, resolve = _.value.name),
        Field("eater", StringType, resolve = _.value.eater)
      ))

      val FruitSmashedType = ObjectType("FruitSmashed", fields[Unit, FruitSmashed](
        Field("fruit", FruitType, resolve = c ⇒ cherryPicker.defer(c.value.id))
      ))

      val FruitEventType = UnionType("FruitEvent", types = FruitEatenType :: FruitSmashedType :: Nil)

      val MutationType = deriveContextObjectType[Ctx, Mutation, Unit](identity)

      val QueryType = ObjectType("QueryType", fields[Ctx, Unit](
        Field("hello", StringType, resolve = _ ⇒ "world")
      ))

      val SubscriptionType = ObjectType("Subscription", fields[Ctx, Unit](
        Field.subs("fruitEvents", OptionType(FruitEventType), resolve =
            c ⇒ c.ctx.eventBus.map(action(_)))
      ))

      val schema = Schema(QueryType, Some(MutationType), Some(SubscriptionType))

      import sangria.execution.ExecutionScheme.Stream

      val exceptionHandler: Executor.ExceptionHandler = {
        case (m, e: IllegalStateException) ⇒ HandledException(e.getMessage)
      }

      val subscription =
        graphql"""
          subscription {
            fruitEvents {
              ... on FruitEaten {name, eater}
              ... on FruitSmashed {
                fruit {
                  name
                  color
                }
              }
            }
          }
        """

      val ctx = new Ctx

      val stream: Observable[JsValue] =
        Executor.execute(schema, subscription,
          ctx,
          deferredResolver = DeferredResolver.fetchers(cherryPicker),
          exceptionHandler = exceptionHandler)

      val mutation =
        graphql"""
          mutation {
             e1: eatFruit(name: "banana", eater: "me")
             smashFruit(id: 123)
             e2: eatFruit(name: "orange", eater: "someone else")
             stop
          }
        """

      {
        import sangria.execution.ExecutionScheme.Default

        Executor.execute(schema, mutation,
          ctx,
          deferredResolver = DeferredResolver.fetchers(cherryPicker),
          exceptionHandler = exceptionHandler).await
      }

      val result = stream.toListL.runAsync.await

      result should (
        have(size(3)) and
        contain("""{"data": {"fruitEvents": {"name": "banana", "eater": "me"}}}""".parseJson) and
        contain("""{"data": {"fruitEvents": {"name": "orange", "eater": "someone else"}}}""".parseJson) and
        contain("""{"data": {"fruitEvents": {"fruit": {"name": "cherry", "color": "red"}}}}""".parseJson))
    }
  }
}
