package sangria.util

import org.scalatest.Matchers
import sangria.execution.{Executor, ResultMarshaller}
import sangria.parser.QueryParser
import sangria.schema.Schema

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global

trait GraphQlSupport extends AwaitSupport with Matchers {
  def schema: Schema[_, _]

  def check[T](data: T, query: String, expected: Any) = {
    val Success(doc) = QueryParser.parse(query)

    val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node] = {
      case (m, e: IllegalStateException) => m.mapNode(Seq("message" -> m.stringNode(e.getMessage)))
    }

    Executor(schema.asInstanceOf[Schema[Unit, T]], data, exceptionHandler = exceptionHandler).execute(doc).await should be (expected)
  }

  def check[T](data: T, query: String, expectedData: Map[String, Any], expectedErrors: List[Map[String, Any]]) = {
    val Success(doc) = QueryParser.parse(query)

    val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node] = {
      case (m, e: IllegalStateException) => m.mapNode(Seq("message" -> m.stringNode(e.getMessage)))
    }

    val result = Executor(schema.asInstanceOf[Schema[Unit, T]], data, exceptionHandler = exceptionHandler).execute(doc).await.asInstanceOf[Map[String, Any]]

    result("data") should be (expectedData)

    val errors = result.get("errors").getOrElse(Nil).asInstanceOf[List[Any]]

    errors should have size(expectedErrors.size)

    expectedErrors foreach (expected => errors should contain (expected))
  }
}
