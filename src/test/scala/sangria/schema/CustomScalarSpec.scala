package sangria.schema

import java.text.SimpleDateFormat
import java.util.Date

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.util.Pos
import sangria.util.SimpleGraphQlSupport._
import sangria.validation.ValueCoercionViolation

import scala.util.{Failure, Success, Try}

class CustomScalarSpec extends WordSpec with Matchers {
  "Schema" should {
    "allow to define custom scalar types" in {
      val dateFormat = new SimpleDateFormat("yyyy-MM-dd")

      case object DateCoercionViolation extends ValueCoercionViolation("Date value expected")

      def parseDate(s: String) = Try(dateFormat.parse(s)) match {
        case Success(d) ⇒ Right(d)
        case Failure(error) ⇒ Left(DateCoercionViolation)
      }

      val DateType = ScalarType[Date]("Date",
        description = Some("An example of date scalar type"),
        coerceOutput = (d, _) ⇒ dateFormat.format(d),
        coerceUserInput = {
          case s: String ⇒ parseDate(s)
          case _ ⇒ Left(DateCoercionViolation)
        },
        coerceInput = {
          case ast.StringValue(s, _, _) ⇒ parseDate(s)
          case _ ⇒ Left(DateCoercionViolation)
        })

      val DateArg = Argument("dateInput", DateType)

      val QueryType = ObjectType("Query", fields[Unit, Unit](
        Field("foo", DateType,
          arguments = DateArg :: Nil,
          resolve = ctx ⇒ {
            val date: Date = ctx.arg(DateArg)
            new Date(date.getTime + 1000 * 60 * 60 * 24 * 5)
          })
      ))

      val schema = Schema(QueryType)

      check(schema, (),
        """
          {
            foo(dateInput: "2015-05-11")
          }
        """,
        Map("data" → Map("foo" → "2015-05-16"))
      )

      checkContainsErrors(schema, (),
        """
          {
            foo(dateInput: "2015-05-test")
          }
        """,
        null,
        List("""Argument 'dateInput' expected type 'Date!' but got: "2015-05-test". Reason: Date value expected""" → List(Pos(3, 28)))
      )
    }
  }
}
