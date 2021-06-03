package sangria.util

import sangria.ast.AstLocation
import sangria.execution.deferred.DeferredResolver
import sangria.execution.{ExceptionHandler, Executor, HandledException, WithViolations}
import sangria.marshalling.InputUnmarshaller
import sangria.parser.{QueryParser, SourceMapper}
import sangria.schema.Schema
import sangria.validation.{AstNodeLocation, AstNodeViolation, QueryValidator, Violation}
import spray.json.{JsObject, JsValue}

import com.twitter.util.Return

import sangria.marshalling.sprayJson.SprayJsonInputUnmarshaller
import org.scalatest.matchers.should.Matchers

object SimpleGraphQlSupport extends FutureResultSupport with Matchers {
  def executeTestQuery[T, A: InputUnmarshaller](
      schema: Schema[_, _],
      data: T,
      query: String,
      args: A,
      userContext: Any = (),
      resolver: DeferredResolver[Any] = DeferredResolver.empty,
      validateQuery: Boolean = true) = {
    val Return(doc) = QueryParser.parse(query)

    val exceptionHandler = ExceptionHandler { case (m, e) =>
      HandledException(e.getMessage)
    }

    Executor
      .execute(
        schema.asInstanceOf[Schema[Any, T]],
        doc.copy(sourceMapper = None),
        userContext,
        data,
        variables = args,
        exceptionHandler = exceptionHandler,
        queryValidator = if (validateQuery) QueryValidator.default else QueryValidator.empty,
        deferredResolver = resolver
      )
      .awaitAndRecoverQueryAnalysisScala
  }

  def check[T](
      schema: Schema[_, _],
      data: T,
      query: String,
      expected: Any,
      args: JsValue = JsObject.empty,
      userContext: Any = (),
      resolver: DeferredResolver[_] = DeferredResolver.empty,
      validateQuery: Boolean = true) = {
    val res = executeTestQuery(
      schema,
      data,
      query,
      args,
      userContext,
      resolver.asInstanceOf[DeferredResolver[Any]],
      validateQuery)

    withClue("Result: \n" + DebugUtil.prettyRender(res) + "\n") {
      res should be(expected)
    }

    res
  }

  def checkErrors[T](
      schema: Schema[_, _],
      data: T,
      query: String,
      expectedData: Map[String, Any],
      expectedErrors: List[Map[String, Any]],
      args: JsValue = JsObject.empty,
      userContext: Any = (),
      resolver: DeferredResolver[Any] = DeferredResolver.empty,
      validateQuery: Boolean = true): Unit = {
    val result = executeTestQuery(schema, data, query, args, userContext, resolver, validateQuery)
      .asInstanceOf[Map[String, Any]]

    result("data") should be(expectedData)

    val errors = result.getOrElse("errors", Vector.empty).asInstanceOf[Seq[Any]]

    errors should have size expectedErrors.size

    expectedErrors.foreach(expected => errors should contain(expected))
  }

  def checkContainsErrors[T](
      schema: Schema[_, _],
      data: T,
      query: String,
      expectedData: Map[String, Any],
      expectedErrorStrings: Seq[(String, Seq[Pos])],
      args: JsValue = JsObject.empty,
      userContext: Any = (),
      resolver: DeferredResolver[_] = DeferredResolver.empty,
      validateQuery: Boolean = true
  ): Unit = {
    val result = executeTestQuery(
      schema,
      data,
      query,
      args,
      validateQuery = validateQuery,
      userContext = userContext,
      resolver = resolver.asInstanceOf[DeferredResolver[Any]]).asInstanceOf[Map[String, Any]]

    result("data") should be(expectedData)

    val errors = result.getOrElse("errors", Vector.empty).asInstanceOf[Seq[Map[String, Any]]]

    val violations =
      errors.map { error =>
        val message = error("message").asInstanceOf[String]
        val locs =
          error.get("locations") match {
            case Some(locs: Seq[Map[String, Any]] @unchecked) =>
              locs
                .map(loc =>
                  AstLocation(0, loc("line").asInstanceOf[Int], loc("column").asInstanceOf[Int]))
                .toList
            case _ => Nil
          }

        StubViolation(message, None, locs)
      }

    assertViolations(violations.toVector, expectedErrorStrings: _*)
  }

  def renderViolations(violations: Vector[Violation]) = {
    val renderedHelpers =
      violations.zipWithIndex
        .map { case (v, idx) =>
          v match {
            case n: AstNodeLocation =>
              "\"" + n.simpleErrorMessage + "\" -> Seq(" + n.locations
                .map(l => s"Pos(${l.line}, ${l.column})")
                .mkString(", ") + ")"
            case n => n.errorMessage
          }
        }
        .mkString(",\n")

    val rendered =
      violations.zipWithIndex
        .map { case (v, idx) =>
          s"(${idx + 1}) " + v.errorMessage
        }
        .mkString("\n\n")

    "Actual violations:\n\n" + renderedHelpers + "\n\n" + rendered + "\n\n"
  }

  def assertViolations(errors: Vector[Violation], expectedErrors: (String, Seq[Pos])*) =
    withClue("Should not validate") {
      withClue(renderViolations(errors)) {
        errors should have size expectedErrors.size
      }

      expectedErrors.foreach { case (expected, pos) =>
        withClue(s"Expected error not found: $expected${pos
          .map(p => s" (line ${p.line}, column ${p.col})")
          .mkString("; ")}. ${renderViolations(errors)}") {
          errors.exists { error =>
            error.errorMessage.contains(expected) && {
              val errorPositions = error.asInstanceOf[AstNodeViolation].locations

              errorPositions should have size pos.size

              errorPositions.zip(pos).forall { case (actualPos, expectedPos) =>
                expectedPos.line == actualPos.line && expectedPos.col == actualPos.column
              }
            }
          } should be(true)
        }
      }
    }

  def checkContainsViolations(execute: => Unit, expected: (String, Seq[Pos])*) =
    assertViolations(intercept[WithViolations](execute).violations, expected: _*)

  case class StubViolation(
      message: String,
      sourceMapper: Option[SourceMapper],
      locations: List[AstLocation])
      extends AstNodeViolation {
    lazy val simpleErrorMessage = message
  }
}

trait GraphQlSupport extends FutureResultSupport with Matchers {
  def schema: Schema[_, _]

  def executeTestQuery[T, A: InputUnmarshaller](
      data: T,
      query: String,
      args: A,
      userContext: Any = (),
      resolver: DeferredResolver[Any] = DeferredResolver.empty,
      validateQuery: Boolean = true): Unit =
    SimpleGraphQlSupport.executeTestQuery(
      schema,
      data,
      query,
      args,
      userContext,
      resolver,
      validateQuery)

  def check[T](
      data: T,
      query: String,
      expected: Any,
      args: JsValue = JsObject.empty,
      userContext: Any = (),
      resolver: DeferredResolver[Any] = DeferredResolver.empty,
      validateQuery: Boolean = true): Unit =
    SimpleGraphQlSupport.check(
      schema,
      data,
      query,
      expected,
      args,
      userContext,
      resolver,
      validateQuery)

  def checkErrors[T](
      data: T,
      query: String,
      expectedData: Map[String, Any],
      expectedErrors: List[Map[String, Any]],
      args: JsValue = JsObject.empty,
      userContext: Any = (),
      resolver: DeferredResolver[Any] = DeferredResolver.empty,
      validateQuery: Boolean = true): Unit =
    SimpleGraphQlSupport.checkErrors(
      schema,
      data,
      query,
      expectedData,
      expectedErrors,
      args,
      userContext,
      resolver,
      validateQuery)

  def checkContainsErrors[T](
      data: T,
      query: String,
      expectedData: Map[String, Any],
      expectedErrorStrings: Seq[(String, Seq[Pos])],
      args: JsValue = JsObject.empty,
      validateQuery: Boolean = true): Unit =
    SimpleGraphQlSupport.checkContainsErrors(
      schema,
      data,
      query,
      expectedData,
      expectedErrorStrings,
      args = args,
      validateQuery = validateQuery)
}

case class Pos(line: Int, col: Int)
