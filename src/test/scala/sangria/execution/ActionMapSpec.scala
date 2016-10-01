package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.FutureResultSupport

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class ActionMapSpec extends WordSpec with Matchers with FutureResultSupport {
  case class Color(name: String)

  case class ColorDefer(num: Int) extends Deferred[String]

  class ColorResolver extends DeferredResolver[Any] {
    override def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) = deferred map {
      case ColorDefer(num) ⇒ Future.successful("[" + (num + 45) + "]")
    }
  }

  val ColorType = ObjectType("Color", fields[Unit, Color](
    Field("name", StringType, resolve = _.value.name)))


  val QueryType = ObjectType("Query", fields[Unit, Unit](
    Field("value", StringType, resolve = _ ⇒
      Value("red").map("light-" + _)),
    Field("doubleMap", StringType, resolve = _ ⇒
      Value("red").map("light-" + _).map(_ + "-color")),
    Field("future", StringType, resolve = _ ⇒
      FutureValue(Future.successful("green")).map("light-" + _)),
    Field("futureDouble", ColorType, resolve = _ ⇒
      FutureValue(Future.successful("green")).map("light-" + _).map(Color(_))),
    Field("futureTriple", StringType, resolve = _ ⇒
      FutureValue(Future.successful("green")).map("light-" + _).map(Color(_)).map("super-" + _.name)),
    Field("deferred", StringType, resolve = _ ⇒
      DeferredValue(ColorDefer(123)).map(x ⇒ x + 345)),
    Field("futureDeferred", StringType, resolve = _ ⇒
      DeferredFutureValue(Future.successful(ColorDefer(34))).map(x ⇒ x + 56)),
    Field("futureDeferredDouble", StringType, resolve = _ ⇒
      DeferredFutureValue(Future.successful(ColorDefer(34))).map(x ⇒ x + 576).map("Yay! " + _ + " +++")),
    Field("futureDeferredTriple", StringType, resolve = _ ⇒
      DeferredFutureValue(Future.successful(ColorDefer(34))).map(x ⇒ x + 576).map(Color(_)).map(c ⇒ "Yay! " + c.name + " +++")),
    Field("ctxUpdate", ColorType, resolve = ctx ⇒
      UpdateCtx(DeferredFutureValue(Future.successful(ColorDefer(11)))){v ⇒ require(v == "[56]"); ctx.ctx}.map("!" + _ + "?").map(x ⇒ x + 576).map(Color(_)).map(c ⇒ "(" + c.name + ")").map(Color(_)))
  ))

  val schema = Schema(QueryType)

  "Actions when mapped" should {
    "transform values correctly" in {
      val Success(doc) = QueryParser.parse("""
        {
          value
          doubleMap
          future
          futureDouble {name}
          futureTriple
          deferred
          futureDeferred
          futureDeferredDouble
          futureDeferredTriple
          ctxUpdate {name}
        }
      """)

      Executor.execute(schema, doc, deferredResolver = new ColorResolver).await should be (Map(
        "data" → Map(
          "value" → "light-red",
          "doubleMap" → "light-red-color",
          "future" → "light-green",
          "futureDouble" → Map("name" → "light-green"),
          "futureTriple" → "super-light-green",
          "deferred" → "[168]345",
          "futureDeferred" → "[79]56",
          "futureDeferredDouble" → "Yay! [79]576 +++",
          "futureDeferredTriple" → "Yay! [79]576 +++",
          "ctxUpdate" → Map("name" → "(![56]?576)")
        )))
    }
  }
}
