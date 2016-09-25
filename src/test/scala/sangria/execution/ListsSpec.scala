package sangria.execution

import sangria.util.FutureResultSupport
import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._

import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class ListsSpec extends WordSpec with Matchers with FutureResultSupport {
  case class Data(test: Action[Unit, Any])

  def check(testType: OutputType[Any], testData: Action[Unit, Any], expected: Any) = {
    implicit val validAny = new ValidOutType[Any, Any] {}
    val data = Data(testData)

    lazy val Type: ObjectType[Unit, Data] = ObjectType("DataType", () ⇒ fields[Unit, Data](
      Field("test", testType, resolve = _.value.test),
      Field("nest", OptionType(Type), resolve = _ ⇒ data)
    ))

    val schema = Schema(Type)

    val Success(doc) = QueryParser.parse("{ nest { test } }")

    val exceptionHandler: Executor.ExceptionHandler = {
      case (m, e: IllegalStateException) ⇒ HandledException(e.getMessage)
    }

    Executor.execute(schema, doc.copy(sourceMapper = None), root = data, exceptionHandler = exceptionHandler).await should be (expected)
  }

  def success[T](t: T) = Future.successful(t)

  "Execute: Handles list nullability" when {
    "[T]" when {
      val tpe = OptionType(ListType(OptionType(IntType)))

      "List[T]" should {
        "Contains values" in check(tpe, List(Some(1), Some(2)), Map("data" → Map("nest" → Map("test" → List(1, 2)))))
        "Contains None" in check(tpe, List(Some(1), None, Some(2)), Map("data" → Map("nest" → Map("test" → List(1, null, 2)))))
        "Contains null" in check(tpe, List(1, null, 2), Map("data" → Map("nest" → Map("test" → List(1, null, 2)))))
        "Returns None" in check(tpe, None, Map("data" → Map("nest" → Map("test" → null))))
        "Returns null" in check(tpe, Value(null), Map("data" → Map("nest" → Map("test" → null))))
      }

      "Future[List[T]]" should {
        "Contains values" in check(tpe,
          success(List(Some(1), Some(2))),
          Map("data" → Map("nest" → Map("test" → List(1, 2)))))
        "Contains None" in check(tpe,
          success(List(Some(1), None, Some(2))),
          Map("data" → Map("nest" → Map("test" → List(1, null, 2)))))
        "Contains null" in check(tpe,
          success(List(1, null, 2)),
          Map("data" → Map("nest" → Map("test" → List(1, null, 2)))))
        "Returns None" in check(tpe, success(None), Map("data" → Map("nest" → Map("test" → null))))
        "Returns null" in check(tpe, FutureValue(success(null)), Map("data" → Map("nest" → Map("test" → null))))
        "Rejected" in check(tpe,
          FutureValue(Future.failed(new IllegalStateException("Boom"))),
          Map(
            "data" → Map("nest" → Map("test" → null)),
            "errors" → List(Map("message" → "Boom", "path" → List("nest", "test"), "locations" → List(Map("line" → 1, "column" → 10))))))
      }
    }

    "[T]!" when {
      val tpe = ListType(OptionType(IntType))

      "List[T]" should {
        "Contains values" in check(tpe, List(Some(1), Some(2)), Map("data" → Map("nest" → Map("test" → List(1, 2)))))
        "Contains None" in check(tpe, List(Some(1), None, Some(2)), Map("data" → Map("nest" → Map("test" → List(1, null, 2)))))
        "Contains null" in check(tpe, List(1, null, 2), Map("data" → Map("nest" → Map("test" → List(1, null, 2)))))
        "Returns null" in check(tpe, Value(null), Map(
          "data" → Map("nest" → null),
          "errors" → List(Map(
            "message" → "Cannot return null for non-nullable type",
            "path" → List("nest", "test"),
            "locations" → List(Map("line" → 1, "column" → 10))))))
      }

      "Future[List[T]]" should {
        "Contains values" in check(tpe,
          success(List(Some(1), Some(2))),
          Map("data" → Map("nest" → Map("test" → List(1, 2)))))
        "Contains None" in check(tpe,
          success(List(Some(1), None, Some(2))),
          Map("data" → Map("nest" → Map("test" → List(1, null, 2)))))
        "Contains null" in check(tpe,
          success(List(1, null, 2)),
          Map("data" → Map("nest" → Map("test" → List(1, null, 2)))))
        "Returns null" in check(tpe, FutureValue(success(null)), Map(
          "data" → Map("nest" → null),
          "errors" → List(Map(
            "message" → "Cannot return null for non-nullable type",
            "path" → List("nest", "test"),
            "locations" → List(Map("line" → 1, "column" → 10))))))
        "Rejected" in check(tpe,
          FutureValue(Future.failed(new IllegalStateException("Boom"))),
          Map(
            "data" → Map("nest" → null),
            "errors" → List(Map(
              "message" → "Boom",
              "path" → List("nest", "test"),
              "locations" → List(Map("line" → 1, "column" → 10))))))
      }
    }

    "[T!]" when {
      val tpe = OptionType(ListType(IntType))

      "List[T]" should {
        "Contains values" in check(tpe, List(1, 2), Map("data" → Map("nest" → Map("test" → List(1, 2)))))
        "Contains null" in check(tpe, List(1, null, 2), Map(
          "data" → Map("nest" → Map("test" → null)),
          "errors" → List(Map(
            "message" → "Cannot return null for non-nullable type",
            "path" → List("nest", "test"),
            "locations" → List(Map("line" → 1, "column" → 10))))))
        "Returns null" in check(tpe, Value(null), Map("data" → Map("nest" → Map("test" → null))))
      }

      "Future[List[T]]" should {
        "Contains values" in check(tpe,
          success(List(1, 2)),
          Map("data" → Map("nest" → Map("test" → List(1, 2)))))
        "Contains null" in check(tpe,
          success(List(1, null, 2)),
          Map(
            "data" → Map("nest" → Map("test" → null)),
            "errors" → List(Map(
              "message" → "Cannot return null for non-nullable type",
              "path" → List("nest", "test"),
              "locations" → List(Map("line" → 1, "column" → 10))))))
        "Returns null" in check(tpe, FutureValue(success(null)), Map("data" → Map("nest" → Map("test" → null))))
        "Rejected" in check(tpe,
          FutureValue(Future.failed(new IllegalStateException("Boom"))),
          Map(
            "data" → Map("nest" → Map("test" → null)),
            "errors" → List(Map("message" → "Boom", "path" → List("nest", "test"), "locations" → List(Map("line" → 1, "column" → 10))))))
      }
    }

    "[T!]!" when {
      val tpe = ListType(IntType)

      "List[T]" should {
        "Contains values" in check(tpe, List(1, 2), Map("data" → Map("nest" → Map("test" → List(1, 2)))))
        "Contains null" in check(tpe, List(1, null, 2), Map(
          "data" → Map("nest" → null),
          "errors" → List(Map(
            "message" → "Cannot return null for non-nullable type",
            "path" → List("nest", "test"),
            "locations" → List(Map("line" → 1, "column" → 10))))))
        "Returns null" in check(tpe, Value(null), Map(
          "data" → Map("nest" → null),
          "errors" → List(Map(
            "message" → "Cannot return null for non-nullable type",
            "path" → List("nest", "test"),
            "locations" → List(Map("line" → 1, "column" → 10))))))
      }

      "Future[List[T]]" should {
        "Contains values" in check(tpe,
          success(List(1, 2)),
          Map("data" → Map("nest" → Map("test" → List(1, 2)))))
        "Contains null" in check(tpe,
          success(List(1, null, 2)),
          Map(
            "data" → Map("nest" → null),
            "errors" → List(Map(
              "message" → "Cannot return null for non-nullable type",
              "path" → List("nest", "test"),
              "locations" → List(Map("line" → 1, "column" → 10))))))
        "Returns null" in check(tpe, FutureValue(success(null)), Map(
          "data" → Map("nest" → null),
          "errors" → List(Map(
            "message" → "Cannot return null for non-nullable type",
            "path" → List("nest", "test"),
            "locations" → List(Map("line" → 1, "column" → 10))))))
        "Rejected" in check(tpe,
          FutureValue(Future.failed(new IllegalStateException("Boom"))),
          Map(
            "data" → Map("nest" → null),
            "errors" → List(Map("message" → "Boom", "path" → List("nest", "test"), "locations" → List(Map("line" → 1, "column" → 10))))))
      }
    }
  }
}
