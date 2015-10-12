package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.integration.ResultMarshaller
import sangria.parser.QueryParser
import sangria.schema._
import sangria.ast
import sangria.util.AwaitSupport
import sangria.validation.StringCoercionViolation

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class ComplexityMeasurementSpec extends WordSpec with Matchers with AwaitSupport {
  val TestScalar = ScalarType[String]("TestScalar",
    complexity = 2.5D,
    coerceOutput = s ⇒ ast.StringValue(s),
    coerceUserInput = {
      case s: String ⇒ Right(s)
      case _ ⇒ Left(StringCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(id, _) ⇒ Right(id)
      case _ ⇒ Left(StringCoercionViolation)
    })

  lazy val TestType: ObjectType[Unit, Unit] = ObjectType("Test", () ⇒ fields[Unit, Unit](
    Field("scalar", StringType, resolve = _ ⇒ "tests"),
    Field("scalarCustom", StringType,
      complexity = Some((_, c) ⇒ 3.0D + c),
      resolve = _ ⇒ "testsc"),
    Field("scalarArgs", StringType,
      arguments = Argument("foo", StringType) :: Nil,
      resolve = _ ⇒ "testsa"),
    Field("complexScalar", TestScalar, resolve = _ ⇒ "testcs"),
    Field("nestList", ListType(TestType),
      arguments = Argument("size", IntType) :: Nil,
      complexity = Some((args, c) ⇒ 1.1D + args.arg[Int]("size") * c),
      resolve = ctx ⇒ (1 to ctx.arg[Int]("size")) map (_ ⇒ ())),
    Field("nest", TestType, resolve = _ ⇒ ())
  ))

  val schema = Schema(TestType)

  "ComplexityMeasurement" should {
    "perform basic calculation with overridden `complexity` function" in {
      val Success(query) = QueryParser.parse("""
        {
          scalar
          nestList(size: 3) {
            complexScalar
            nest {
              cc: scalarCustom
              dd: scalarCustom
            }

            foo: nest {
              cc: scalarCustom
              dd: scalarCustom
            }
          }
        }
      """)

      var complexity = 0.0D

      val captureComplexity = (c: Double) ⇒ complexity = c

      Executor.execute(schema, query, measureComplexity = Some(captureComplexity)).await should be (
        Map("data" ->
          Map(
            "scalar" -> "tests",
            "nestList" -> List(
              Map(
                "complexScalar" -> "testcs",
                "nest" -> Map("cc" -> "testsc", "dd" -> "testsc"),
                "foo" -> Map("cc" -> "testsc", "dd" -> "testsc")),
              Map(
                "complexScalar" -> "testcs",
                "nest" -> Map("cc" -> "testsc", "dd" -> "testsc"),
                "foo" -> Map("cc" -> "testsc", "dd" -> "testsc")),
              Map(
                "complexScalar" -> "testcs",
                "nest" -> Map("cc" -> "testsc", "dd" -> "testsc"),
                "foo" -> Map("cc" -> "testsc", "dd" -> "testsc"))))))

      complexity should be (54.6)
    }

    "ability to reject too complex queries" in {
      val Success(query) = QueryParser.parse("""
        {
          scalar
          cs1: complexScalar
          cs2: complexScalar
          cs3: complexScalar
          cs4: complexScalar
        }
        """)

      val rejectComplexity = (c: Double) ⇒
        if (c > 14)
          throw new IllegalArgumentException(s"Too complex query: max allowed complexity is 14.0, but got $c")
        else ()

      val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
        case (m, e: IllegalArgumentException) ⇒ HandledException(e.getMessage)
      }

      Executor.execute(schema, query,
          exceptionHandler = exceptionHandler,
          measureComplexity = Some(rejectComplexity)).await should be (
        Map(
          "data" -> null,
          "errors" -> List(
            Map("message" -> "Too complex query: max allowed complexity is 14.0, but got 15.0"))))
    }
  }
}
