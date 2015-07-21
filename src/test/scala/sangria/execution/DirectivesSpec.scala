package sangria.execution

import language.postfixOps

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._

import scala.concurrent.Await
import scala.util.Success
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class DirectivesSpec extends WordSpec with Matchers {

  case class TestSubject(a: Option[String], b: Option[String])

  val schema = Schema(ObjectType("TestType", List[Field[Unit, TestSubject]](
    Field("a", OptionType(StringType), resolve = _.value.a),
    Field("b", OptionType(StringType), resolve = _.value.b)
  )))

  val data = TestSubject(Some("a"), Some("b"))

  def executeTestQuery(query: String) = {
    val Success(doc) = QueryParser.parse(query)

    Await.result(Executor(schema, data).execute(doc), 5 seconds)
  }

  "Execute: handles directives" when {
    "directives are not used" should {
      "execute basic query" in {
        executeTestQuery("{ a, b }") should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }
    }

    "used with scalar values" should {
      "if true includes scalar" in {
        executeTestQuery("{ a, b @include(if: true) }") should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "if false omits on scalar" in {
        executeTestQuery("{ a, b @include(if: false) }") should be (Map("data" -> Map("a" -> "a")))
      }

      "unless false includes scalar" in {
        executeTestQuery("{ a, b @skip(if: false) }") should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless true omits scalar" in {
        executeTestQuery("{ a, b @skip(if: true) }") should be (Map("data" -> Map("a" -> "a")))
      }
    }

    "used on fragment spread" should {
      "if false omits fragment spread" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag @include(if: false)
             }
             fragment Frag on TestType {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a")))
      }

      "if true includes fragment spread" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag @include(if: true)
             }
             fragment Frag on TestType {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless false includes fragment spread" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag @skip(if: false)
             }
             fragment Frag on TestType {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless true omits fragment spread" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag @skip(if: true)
             }
             fragment Frag on TestType {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a")))
      }
    }

    "used on inline fragment" should {
      "if false omits inline fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ... on TestType @include(if: false) {
                 b
               }
             }
             fragment Frag on TestType {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a")))
      }

      "if true includes inline fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ... on TestType @include(if: true) {
                 b
               }
             }
             fragment Frag on TestType {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless false includes inline fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ... on TestType @skip(if: false) {
                 b
               }
             }
             fragment Frag on TestType {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless true includes inline fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ... on TestType @skip(if: true) {
                 b
               }
             }
             fragment Frag on TestType {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a")))
      }
    }

    "used on fragments" should {
      "if false omits fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @include(if: false) {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a")))
      }

      "if true includes fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @include(if: true) {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless false includes fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @skip(if: false) {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless true omits fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @skip(if: true) {
               b
             }
          """) should be (Map("data" -> Map("a" -> "a")))
      }
    }
  }
}