package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.integration.ResultMarshaller
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.{OutputMatchers, AwaitSupport}

import scala.concurrent.Future
import scala.util.{Failure, Try, Success}

import scala.concurrent.ExecutionContext.Implicits.global

class ExceptionHandlingSpec extends WordSpec with Matchers with AwaitSupport with OutputMatchers {
  val TestType = ObjectType("Test", fields[Unit, Unit](
    Field("success", OptionType(StringType), resolve = _ => "Yay"),
    Field("trySuccess", OptionType(StringType), resolve = _ => Success("try!")),
    Field("tryError", OptionType(StringType), resolve = _ => Failure(new IllegalStateException("try boom!"))),
    Field("error", OptionType(StringType), resolve = _ => throw new IllegalStateException("Boom!")),
    Field("futureError", OptionType(StringType), resolve = _ => Future.failed[String](new IllegalStateException("Boom!")))
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

        Executor(schema).execute(doc).await should be  (
          Map(
            "data" -> Map(
              "success" -> "Yay",
              "trySuccess" -> "try!",
              "tryError" -> null,
              "error" -> null,
              "futureError" -> null),
            "errors" -> List(
              Map(
                "message" -> "Internal server error",
                "field" -> "error",
                "locations" -> List(Map("line" -> 6, "column" -> 11))),
              Map(
                "message" -> "Internal server error",
                "field" -> "tryError",
                "locations" -> List(Map("line" -> 4, "column" -> 11))),
              Map(
                "message" -> "Internal server error",
                "field" -> "futureError",
                "locations" -> List(Map("line" -> 7, "column" -> 11))))))
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

      val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
        case (m, e: IllegalStateException) => HandledException(e.getMessage)
      }

      Executor(schema, exceptionHandler = exceptionHandler).execute(doc).await should be  (
        Map(
          "data" -> Map(
            "error" -> null,
            "futureError" -> null),
          "errors" -> List(
            Map(
              "message" -> "Boom!",
              "field" -> "error",
              "locations" -> List(Map("line" -> 3, "column" -> 11))),
            Map(
              "message" -> "Boom!",
              "field" -> "futureError",
              "locations" -> List(Map("line" -> 4, "column" -> 11))))))
    }

    "provide user-defined exception handling mechanism which allows to provide additional fields" in {
      val Success(doc) = QueryParser.parse("""
        {
          error
          futureError
        }
        """)

      val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
        case (m, e: IllegalStateException) =>
          HandledException(e.getMessage,
            Map("foo" -> m.arrayNode(Seq(m.stringNode("bar"), m.intNode(1234))), "baz" -> m.stringNode("Test")))
      }

      Executor(schema, exceptionHandler = exceptionHandler).execute(doc).await should be  (
        Map(
          "data" -> Map(
            "error" -> null,
            "futureError" -> null),
          "errors" -> List(
            Map(
              "message" -> "Boom!",
              "field" -> "error",
              "foo" -> List("bar", 1234),
              "baz" -> "Test",
              "locations" -> List(Map("line" -> 3, "column" -> 11))),
            Map(
              "message" -> "Boom!",
              "field" -> "futureError",
              "foo" -> List("bar", 1234),
              "baz" -> "Test",
              "locations" -> List(Map("line" -> 4, "column" -> 11))))))
    }
  }

}
