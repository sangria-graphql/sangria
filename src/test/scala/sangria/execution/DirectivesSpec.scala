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

  val FragDefIncludeDirective = Directive("fragDefInclude",
    description = Some("Directs the executor to include this fragment definition only when the `if` argument is true."),
    arguments = IfArg :: Nil,
    locations = Set(DirectiveLocation.FragmentDefinition),
    shouldInclude = ctx ⇒ ctx.arg(IfArg))

  val schema = Schema(ObjectType("TestType", fields[Unit, TestSubject](
    Field("a", OptionType(StringType), resolve = _.value.a),
    Field("b", OptionType(StringType), resolve = _.value.b)
  )), directives = BuiltinDirectives :+ FragDefIncludeDirective)

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

    "works on fragment" should {
      "if false omits fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @fragDefInclude(if: false) {
               b
             }
          """) should be (Map("data" → Map("a" → "a")))
      }

      "if true includes fragment" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @fragDefInclude(if: true) {
               b
             }
          """) should be (Map("data" → Map("a" → "a", "b" → "b")))
      }

      "if false omits fragment (unsupported location)" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @include(if: false) {
               b
             }
          """) should be (Map("data" → Map("a" → "a")))
      }

      "if true omits fragment (unsupported location)" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @include(if: true) {
               b
             }
          """) should be (Map("data" → Map("a" → "a")))
      }

      "unless false omits fragment (unsupported location)" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @skip(if: false) {
               b
             }
          """) should be (Map("data" → Map("a" → "a")))
      }

      "unless true omits fragment (unsupported location)" in {
        executeTestQuery(
          """
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @skip(if: true) {
               b
             }
          """) should be (Map("data" → Map("a" → "a")))
      }
    }

    "works with skip and include directives" should {
      "include and no skip" in {
        executeTestQuery("{ a, b @include(if: true) @skip(if: false) }") should be (Map("data" → Map("a" → "a", "b" → "b")))
      }

      "include and skip" in {
        executeTestQuery("{ a, b @include(if: true) @skip(if: true) }") should be (Map("data" → Map("a" → "a")))
      }

      "no include or skip" in {
        executeTestQuery("{ a, b @include(if: false) @skip(if: false) }") should be (Map("data" → Map("a" → "a")))
      }
    }
  }
}