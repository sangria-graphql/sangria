package sangria.execution

import language.postfixOps

import org.scalatest.{Matchers, WordSpec}

import sangria.parser.QueryParser
import sangria.schema._

import scala.concurrent.{Future, Await}
import scala.util.Success
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class ListsSpec extends WordSpec with Matchers {
  case class Data(test: Action[Unit, Any])

  def check(testType: OutputType[Any], testData: Action[Unit, Any], expected: Any) = {
    implicit val validAny = new ValidOutType[Any, Any] {}
    val data = Data(testData)

    lazy val Type: ObjectType[Unit, Data] = ObjectType("DataType", () => List[Field[Unit, Data]](
      Field("test", testType, resolve = _.value.test),
      Field("nest", OptionType(Type), resolve = _ => data)
    ))

    val schema = Schema(Type)

    val Success(doc) = QueryParser.parse("{ nest { test } }")

    val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node] = {
      case (m, e: IllegalStateException) => m.mapNode(Seq("message" -> m.stringNode(e.getMessage)))
    }

    Await.result(Executor(schema, data, exceptionHandler = exceptionHandler).execute(doc), 5 seconds) should be (expected)
  }

  def success[T](t: T) = Future.successful(t)

  "Execute: Handles list nullability" when {
    "[T]" when {
      val tpe = OptionType(ListType(OptionType(IntType)))

      "List[T]" should {
        "Contains values" in check(tpe, List(Some(1), Some(2)), Map("data" -> Map("nest" -> Map("test" -> List(1, 2)))))
        "Contains None" in check(tpe, List(Some(1), None, Some(2)), Map("data" -> Map("nest" -> Map("test" -> List(1, null, 2)))))
        "Contains null" in check(tpe, List(1, null, 2), Map("data" -> Map("nest" -> Map("test" -> List(1, null, 2)))))
        "Returns None" in check(tpe, None, Map("data" -> Map("nest" -> Map("test" -> null))))
        "Returns null" in check(tpe, Value(null), Map("data" -> Map("nest" -> Map("test" -> null))))
      }

      "Future[List[T]]" should {
        "Contains values" in check(tpe,
          success(List(Some(1), Some(2))),
          Map("data" -> Map("nest" -> Map("test" -> List(1, 2)))))
        "Contains None" in check(tpe,
          success(List(Some(1), None, Some(2))),
          Map("data" -> Map("nest" -> Map("test" -> List(1, null, 2)))))
        "Contains null" in check(tpe,
          success(List(1, null, 2)),
          Map("data" -> Map("nest" -> Map("test" -> List(1, null, 2)))))
        "Returns None" in check(tpe, success(None), Map("data" -> Map("nest" -> Map("test" -> null))))
        "Returns null" in check(tpe, FutureValue(success(null)), Map("data" -> Map("nest" -> Map("test" -> null))))
        "Rejected" in check(tpe,
          FutureValue(Future.failed(new IllegalStateException("Boom"))),
          Map(
            "data" -> Map("nest" -> Map("test" -> null)),
            "errors" -> List(Map("message" -> "Boom", "field" -> "nest.test", "locations" -> List(Map("line" -> 1, "column" -> 10))))))
      }
    }
  }
}
