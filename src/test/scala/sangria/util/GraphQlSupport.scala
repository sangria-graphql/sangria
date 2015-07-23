package sangria.util

import org.scalatest.Matchers
import sangria.execution.{Executor, ResultMarshaller}
import sangria.parser.QueryParser
import sangria.schema.Schema

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

trait GraphQlSupport extends AwaitSupport with Matchers {
  def schema: Schema[_, _]

  def executeTestQuery[T](data: T, query: String) = {
    val Success(doc) = QueryParser.parse(query)

    val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node] = {
      case (m, e: IllegalStateException) => m.mapNode(Seq("message" -> m.stringNode(e.getMessage)))
    }

    Executor(schema.asInstanceOf[Schema[Unit, T]], data, exceptionHandler = exceptionHandler).execute(doc).await
  }

  def check[T](data: T, query: String, expected: Any) = {
    executeTestQuery(data, query) should be (expected)
  }

  def check[T](data: T, query: String, expectedData: Map[String, Any], expectedErrors: List[Map[String, Any]]) = {
    val result = executeTestQuery(data, query).asInstanceOf[Map[String, Any]]

    result("data") should be (expectedData)

    val errors = result.getOrElse("errors", Nil).asInstanceOf[List[Any]]

    errors should have size (expectedErrors.size)

    expectedErrors foreach (expected => errors should contain (expected))
  }

  def checkContainsErrors[T](data: T, query: String, expectedData: Map[String, Any], expectedErrorStrings: List[String]) = {
    val result = executeTestQuery(data, query).asInstanceOf[Map[String, Any]]

    result("data") should be (expectedData)

    val errors = result.getOrElse("errors", Nil).asInstanceOf[List[Map[String, Any]]]

    errors should have size (expectedErrorStrings.size)

    expectedErrorStrings foreach { expected =>
      withClue(s"Expected error not found: $expected.") {
        errors exists (_("message").asInstanceOf[String] contains expected) should be(true)
      }
    }
  }
}
