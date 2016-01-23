package sangria.util

import org.scalatest.Matchers
import sangria.execution.{HandledException, Executor}
import sangria.marshalling.{InputUnmarshaller, ResultMarshaller}
import sangria.parser.QueryParser
import sangria.schema.{DeferredResolver, Schema}
import sangria.validation.QueryValidator

import spray.json.{JsValue, JsObject}

import scala.util.Success

import scala.concurrent.ExecutionContext.Implicits.global
import sangria.marshalling.sprayJson.SprayJsonInputUnmarshaller

object SimpleGraphQlSupport extends AwaitSupport with Matchers {
  def executeTestQuery[T, A: InputUnmarshaller](schema: Schema[_, _], data: T, query: String, args: A, userContext: Any = (), resolver: DeferredResolver[Any] = DeferredResolver.empty, validateQuery: Boolean = true) = {
    val Success(doc) = QueryParser.parse(query)

    val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
      case (m, e: IllegalStateException) ⇒ HandledException(e.getMessage)
    }

    Executor(
      schema.asInstanceOf[Schema[Any, T]],
      data,
      exceptionHandler = exceptionHandler,
      userContext = userContext,
      queryValidator = if (validateQuery) QueryValidator.default else QueryValidator.empty,
      deferredResolver = resolver).execute(doc.copy(sourceMapper = None), variables = args).await
  }

  def check[T](schema: Schema[_, _], data: T, query: String, expected: Any, args: JsValue = JsObject.empty, userContext: Any = (), resolver: DeferredResolver[Any] = DeferredResolver.empty, validateQuery: Boolean = true): Unit = {
    executeTestQuery(schema, data, query, args, userContext, resolver, validateQuery) should be (expected)
  }

  def checkErrors[T](schema: Schema[_, _], data: T, query: String, expectedData: Map[String, Any], expectedErrors: List[Map[String, Any]], args: JsValue = JsObject.empty, userContext: Any = (), resolver: DeferredResolver[Any] = DeferredResolver.empty, validateQuery: Boolean = true): Unit = {
    val result = executeTestQuery(schema, data, query, args, userContext, resolver, validateQuery).asInstanceOf[Map[String, Any]]

    result("data") should be (expectedData)

    val errors = result.getOrElse("errors", Vector.empty).asInstanceOf[Seq[Any]]

    errors should have size expectedErrors.size

    expectedErrors foreach (expected ⇒ errors should contain (expected))
  }

  def checkContainsErrors[T](schema: Schema[_, _], data: T, query: String, expectedData: Map[String, Any], expectedErrorStrings: List[(String, List[Pos])], args: JsValue = JsObject.empty, validateQuery: Boolean = true): Unit = {
    val result = executeTestQuery(schema, data, query, args, validateQuery = validateQuery).asInstanceOf[Map[String, Any]]

    result("data") should be (expectedData)

    val errors = result.getOrElse("errors", Vector.empty).asInstanceOf[Seq[Map[String, Any]]]

    errors should have size expectedErrorStrings.size

    expectedErrorStrings foreach { case(expected, pos) ⇒
      withClue(s"Expected error not found: $expected${pos map (p ⇒ s" (line ${p.line}, column ${p.col})") mkString ","}. Actual:\n$errors") {
        errors exists { error ⇒
          val message = error("message").asInstanceOf[String]

          message.contains(expected) && {
            error.get("locations") match {
              case None if pos.nonEmpty ⇒ false
              case None ⇒ true
              case Some(locs: Seq[Map[String, Any]] @unchecked) ⇒
                locs.map(loc ⇒ Pos(loc("line").asInstanceOf[Int], loc("column").asInstanceOf[Int])) == pos
            }
          }
        } should be(true)
      }
    }
  }
}

trait GraphQlSupport extends AwaitSupport with Matchers {
  def schema: Schema[_, _]

  def executeTestQuery[T, A: InputUnmarshaller](data: T, query: String, args: A, userContext: Any = (), resolver: DeferredResolver[Any] = DeferredResolver.empty, validateQuery: Boolean = true): Unit =
    SimpleGraphQlSupport.executeTestQuery(schema, data, query, args, userContext, resolver, validateQuery)

  def check[T](data: T, query: String, expected: Any, args: JsValue = JsObject.empty, userContext: Any = (), resolver: DeferredResolver[Any] = DeferredResolver.empty, validateQuery: Boolean = true): Unit =
    SimpleGraphQlSupport.check(schema, data, query, expected, args, userContext, resolver, validateQuery)

  def checkErrors[T](data: T, query: String, expectedData: Map[String, Any], expectedErrors: List[Map[String, Any]], args: JsValue = JsObject.empty, userContext: Any = (), resolver: DeferredResolver[Any] = DeferredResolver.empty, validateQuery: Boolean = true): Unit =
    SimpleGraphQlSupport.checkErrors(schema, data, query, expectedData, expectedErrors, args, userContext, resolver, validateQuery)

  def checkContainsErrors[T](data: T, query: String, expectedData: Map[String, Any], expectedErrorStrings: List[(String, List[Pos])], args: JsValue = JsObject.empty, validateQuery: Boolean = true): Unit =
    SimpleGraphQlSupport.checkContainsErrors(schema, data, query, expectedData, expectedErrorStrings, args, validateQuery)
}

case class Pos(line: Int, col: Int)