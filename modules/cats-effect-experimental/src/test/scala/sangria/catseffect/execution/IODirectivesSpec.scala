package sangria.catseffect.execution

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import org.scalatest.TryValues._
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.catseffect.execution.IOExecutionScheme._
import sangria.catseffect.schema.AsyncValue._
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation.QueryValidator

/** An [[IO]] counterpart of `sangria.execution.DirectivesSpec`, checking that directive handling
  * (`@include`/`@skip`/fragment-definition directives) still works correctly when field resolvers
  * use [[IO]] instead of a plain value.
  */
class IODirectivesSpec extends AnyWordSpec with Matchers {

  case class TestSubject(a: Option[String], b: Option[String])

  val FragDefIncludeDirective = Directive(
    "fragDefInclude",
    description = Some(
      "Directs the executor to include this fragment definition only when the `if` argument is true."),
    arguments = IfArg :: Nil,
    locations = Set(DirectiveLocation.FragmentDefinition),
    shouldInclude = ctx => ctx.arg(IfArg)
  )

  val schema = Schema(
    ObjectType(
      "TestType",
      fields[Unit, TestSubject](
        Field("a", OptionType(StringType), resolve = c => IO(c.value.a)),
        Field("b", OptionType(StringType), resolve = c => IO(c.value.b))
      )
    ),
    directives = BuiltinDirectives :+ FragDefIncludeDirective
  )

  val data = TestSubject(Some("a"), Some("b"))

  def executeTestQuery(query: String): Any = {
    val doc = QueryParser.parse(query).success.value

    val result: IO[Any] =
      Executor.execute(schema, doc, root = data, queryValidator = QueryValidator.empty)
    result.unsafeRunSync()
  }

  "Execute: handles directives" when {
    "works without directives" should {
      "basic query works" in {
        executeTestQuery("{ a, b }") must be(Map("data" -> Map("a" -> "a", "b" -> "b")))
      }
    }

    "works on scalars" should {
      "if true includes scalar" in {
        executeTestQuery("{ a, b @include(if: true) }") must be(
          Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "if false omits on scalar" in {
        executeTestQuery("{ a, b @include(if: false) }") must be(Map("data" -> Map("a" -> "a")))
      }

      "unless false includes scalar" in {
        executeTestQuery("{ a, b @skip(if: false) }") must be(
          Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless true omits scalar" in {
        executeTestQuery("{ a, b @skip(if: true) }") must be(Map("data" -> Map("a" -> "a")))
      }
    }

    "works on fragment spreads" should {
      "if false omits fragment spread" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag @include(if: false)
             }
             fragment Frag on TestType {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }

      "if true includes fragment spread" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag @include(if: true)
             }
             fragment Frag on TestType {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless false includes fragment spread" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag @skip(if: false)
             }
             fragment Frag on TestType {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless true omits fragment spread" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag @skip(if: true)
             }
             fragment Frag on TestType {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }
    }

    "works on inline fragment" should {
      "if false omits inline fragment" in {
        executeTestQuery("""
             query Q {
               a
               ... on TestType @include(if: false) {
                 b
               }
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }

      "if true includes inline fragment" in {
        executeTestQuery("""
             query Q {
               a
               ... on TestType @include(if: true) {
                 b
               }
             }
          """) must be(Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless false includes inline fragment" in {
        executeTestQuery("""
             query Q {
               a
               ... on TestType @skip(if: false) {
                 b
               }
             }
          """) must be(Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless true includes inline fragment" in {
        executeTestQuery("""
             query Q {
               a
               ... on TestType @skip(if: true) {
                 b
               }
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }
    }

    "works on anonymous inline fragment" should {
      "if false omits anonymous inline fragment" in {
        executeTestQuery("""
             query Q {
               a
               ... @include(if: false) {
                 b
               }
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }

      "if true includes anonymous inline fragment" in {
        executeTestQuery("""
             query Q {
               a
               ... @include(if: true) {
                 b
               }
             }
          """) must be(Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless false includes anonymous inline fragment" in {
        executeTestQuery("""
             query Q {
               a
               ... @skip(if: false) {
                 b
               }
             }
          """) must be(Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "unless true includes anonymous inline fragment" in {
        executeTestQuery("""
             query Q {
               a
               ... @skip(if: true) {
                 b
               }
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }
    }

    "works on fragment" should {
      "if false omits fragment" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @fragDefInclude(if: false) {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }

      "if true includes fragment" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @fragDefInclude(if: true) {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "if false omits fragment (unsupported location)" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @include(if: false) {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }

      "if true omits fragment (unsupported location)" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @include(if: true) {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }

      "unless false omits fragment (unsupported location)" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @skip(if: false) {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }

      "unless true omits fragment (unsupported location)" in {
        executeTestQuery("""
             query Q {
               a
               ...Frag
             }
             fragment Frag on TestType @skip(if: true) {
               b
             }
          """) must be(Map("data" -> Map("a" -> "a")))
      }
    }

    "works with skip and include directives" should {
      "include and no skip" in {
        executeTestQuery("{ a, b @include(if: true) @skip(if: false) }") must be(
          Map("data" -> Map("a" -> "a", "b" -> "b")))
      }

      "include and skip" in {
        executeTestQuery("{ a, b @include(if: true) @skip(if: true) }") must be(
          Map("data" -> Map("a" -> "a")))
      }

      "no include or skip" in {
        executeTestQuery("{ a, b @include(if: false) @skip(if: false) }") must be(
          Map("data" -> Map("a" -> "a")))
      }
    }
  }
}
