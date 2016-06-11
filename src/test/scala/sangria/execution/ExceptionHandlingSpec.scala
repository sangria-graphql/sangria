package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.marshalling.ResultMarshaller
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.{OutputMatchers, FutureResultSupport}

import scala.concurrent.Future
import scala.util.{Failure, Try, Success}

import scala.concurrent.ExecutionContext.Implicits.global

class ExceptionHandlingSpec extends WordSpec with Matchers with FutureResultSupport with OutputMatchers {
  val TestType = ObjectType("Test", fields[Unit, Unit](
    Field("success", OptionType(StringType), resolve = _ ⇒ "Yay"),
    Field("trySuccess", OptionType(StringType), resolve = _ ⇒ Success("try!")),
    Field("tryError", OptionType(StringType), resolve = _ ⇒ Failure(new IllegalStateException("try boom!"))),
    Field("error", OptionType(StringType), resolve = _ ⇒ throw new IllegalStateException("Boom!")),
    Field("futureError", OptionType(StringType), resolve = _ ⇒ Future.failed[String](new IllegalStateException("Boom!")))
  ))

  val schema = Schema(TestType)

  "Exception handling" should {
    "obfuscate unexpected exceptions" in {
      val out = captureStdErr {
        val Success(doc) = QueryParser.parse("""
        {
          success
          tryError
          trySuccess
          error
          futureError
        }
        """)

        Executor.execute(schema, doc).await should be  (
          Map(
            "data" → Map(
              "success" → "Yay",
              "trySuccess" → "try!",
              "tryError" → null,
              "error" → null,
              "futureError" → null),
            "errors" → List(
              Map(
                "message" → "Internal server error",
                "path" → List("error"),
                "locations" → List(Map("line" → 6, "column" → 11))),
              Map(
                "message" → "Internal server error",
                "path" → List("tryError"),
                "locations" → List(Map("line" → 4, "column" → 11))),
              Map(
                "message" → "Internal server error",
                "path" → List("futureError"),
                "locations" → List(Map("line" → 7, "column" → 11))))))
      }

      out should include ("java.lang.IllegalStateException: Boom!")
    }

    "provide user-defined exception handling mechanism" in {
      val Success(doc) = QueryParser.parse("""
        {
          error
          futureError
        }
        """)

      val exceptionHandler: Executor.ExceptionHandler = {
        case (m, e: IllegalStateException) ⇒ HandledException(e.getMessage)
      }

      Executor.execute(schema, doc, exceptionHandler = exceptionHandler).await should be  (
        Map(
          "data" → Map(
            "error" → null,
            "futureError" → null),
          "errors" → List(
            Map(
              "message" → "Boom!",
              "path" → List("error"),
              "locations" → List(Map("line" → 3, "column" → 11))),
            Map(
              "message" → "Boom!",
              "path" → List("futureError"),
              "locations" → List(Map("line" → 4, "column" → 11))))))
    }

    "provide user-defined exception handling mechanism which allows to provide additional fields" in {
      val Success(doc) = QueryParser.parse("""
        {
          error
          futureError
        }
        """)

      val exceptionHandler: Executor.ExceptionHandler = {
        case (m, e: IllegalStateException) ⇒
          HandledException(e.getMessage,
            Map("foo" → m.arrayNode(Vector(m.scalarNode("bar", "String", Set.empty), m.scalarNode(1234, "Int", Set.empty))), "baz" → m.scalarNode("Test", "String", Set.empty)))
      }

      Executor.execute(schema, doc, exceptionHandler = exceptionHandler).await should be  (
        Map(
          "data" → Map(
            "error" → null,
            "futureError" → null),
          "errors" → List(
            Map(
              "message" → "Boom!",
              "path" → List("error"),
              "foo" → List("bar", 1234),
              "baz" → "Test",
              "locations" → List(Map("line" → 3, "column" → 11))),
            Map(
              "message" → "Boom!",
              "path" → List("futureError"),
              "foo" → List("bar", 1234),
              "baz" → "Test",
              "locations" → List(Map("line" → 4, "column" → 11))))))
    }
  }

}
