package sangria.execution

import sangria.util.FutureResultSupport

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation.QueryValidator

import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class DirectivesSpec extends WordSpec with Matchers with FutureResultSupport {

  case class TestSubject(a: Option[String], b: Option[String])

  val schema = Schema(ObjectType("TestType", fields[Unit, TestSubject](
    Field("a", OptionType(StringType), resolve = _.value.a),
    Field("b", OptionType(StringType), resolve = _.value.b)
  )))

  val data = TestSubject(Some("a"), Some("b"))

  def executeTestQuery(query: String) = {
    val Success(doc) = QueryParser.parse(query)

    Executor.execute(schema, doc, root = data, queryValidator = QueryValidator.empty).await
  }

  "Execute: handles directives" when {
    "works without directives" should {
      "basic query works" in {
        executeTestQuery("{ a, b }") should be (Map("data" → Map("a" → "a", "b" → "b")))
      }
    }

    "works on scalars" should {
      "if true includes scalar" in {
        executeTestQuery("{ a, b @include(if: true) }") should be (Map("data" → Map("a" → "a", "b" → "b")))
      }

      "if false omits on scalar" in {
        executeTestQuery("{ a, b @include(if: false) }") should be (Map("data" → Map("a" → "a")))
      }

      "unless false includes scalar" in {
        executeTestQuery("{ a, b @skip(if: false) }") should be (Map("data" → Map("a" → "a", "b" → "b")))
      }

      "unless true omits scalar" in {
        executeTestQuery("{ a, b @skip(if: true) }") should be (Map("data" → Map("a" → "a")))
      }
    }

    "works on fragment spreads" should {
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
          """) should be (Map("data" → Map("a" → "a")))
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
          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
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
          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
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
          """) should be (Map("data" → Map("a" → "a")))
      }
    }

    "works on inline fragment" should {
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
          """) should be (Map("data" → Map("a" → "a")))
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
          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
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
          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
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
          """) should be (Map("data" → Map("a" → "a")))
      }
    }

    "works on anonymous inline fragment" should {
      "if false omits anonymous inline fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ... @include(if: false) {
                 b
               }
             }
          """) should be (Map("data" → Map("a" → "a")))
      }

      "if true includes anonymous inline fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ... @include(if: true) {
                 b
               }
             }
          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
      }

      "unless false includes anonymous inline fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ... @skip(if: false) {
                 b
               }
             }
          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
      }

      "unless true includes anonymous inline fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ... @skip(if: true) {
                 b
               }
             }
          """) should be (Map("data" → Map("a" → "a")))
      }
    }

    // TODO: clarify fragment definition location for `include` and `skip` directives: https://github.com/graphql/graphql-js/pull/317#discussion_r57322168
//    "works on fragment" should {
//      "if false omits fragment" in {
//        executeTestQuery(
//          """
//             query Q {
//               a
//               ...Frag
//             }
//             fragment Frag on TestType @include(if: false) {
//               b
//             }
//          """) should be (Map("data" → Map("a" → "a")))
//      }
//
//      "if true includes fragment" in {
//        executeTestQuery(
//          """
//             query Q {
//               a
//               ...Frag
//             }
//             fragment Frag on TestType @include(if: true) {
//               b
//             }
//          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
//      }
//
//      "unless false includes fragment" in {
//        executeTestQuery(
//          """
//             query Q {
//               a
//               ...Frag
//             }
//             fragment Frag on TestType @skip(if: false) {
//               b
//             }
//          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
//      }
//
//      "unless true omits fragment" in {
//        executeTestQuery(
//          """
//             query Q {
//               a
//               ...Frag
//             }
//             fragment Frag on TestType @skip(if: true) {
//               b
//             }
//          """) should be (Map("data" → Map("a" → "a")))
//      }
//
//      "include `true` includes inline fragments without type condition" in {
//        executeTestQuery(
//          """
//             query Q {
//               ... {
//                 a
//               }
//               ... @include(if: true) {
//                 b
//               }
//             }
//          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
//      }
//      "include `false` omits inline fragments without type condition" in {
//        executeTestQuery(
//          """
//             query Q {
//               ... {
//                 a
//               }
//               ... @include(if: false) {
//                 b
//               }
//             }
//          """) should be(Map("data" → Map("a" → "a")))
//      }
//    }
  }
}