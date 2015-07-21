package sangria.util

import org.scalatest.{Matchers, WordSpec}
import sangria.TestData.{FriendsResolver, CharacterRepo}
import sangria.execution.Executor
import sangria.parser.{SyntaxError, QueryParser}
import sangria.schema.TestSchema

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

class TemporarySpec extends WordSpec with Matchers {

  "Foo" should {
    "values and defers" in {
      val ast = QueryParser.parse(
        s"""
           query testQ ($$xx: Boolean!) {
             #${(1 to 1000) map ("field" + _) mkString " "}

             ... on Query {
               getDroid
             }

             ...Foo
           }

           fragment Foo on Query {
             human(id: "1003") @include(if: $$xx) {id, name, friends{name}}
           }
        """
      ) match {
        case Success(v) => v
        case Failure(error: SyntaxError) =>
          println(error.formattedError)
          throw error
        case Failure(e) => throw e
      }

      val vars = Map("xx" -> true)

      {
        import sangria.integration.Json4sSupport._
        import org.json4s.native.JsonMethods._

        println("Json4s marshalling:\n")
        println(pretty(render(Await.result(
          Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
              .execute(ast, arguments = Some(vars)), Duration.Inf))))
      }

      {
        import sangria.integration.SprayJsonSupport._

        println("\nSprayJson marshalling:\n")
        println(Await.result(
          Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
              .execute(ast, arguments = Some(vars)), Duration.Inf).prettyPrint)
      }
    }

    "projections" in {
      val ast = QueryParser.parse(
        s"""
           query testQ ($$xx: Boolean!) {
             #${(1 to 1000) map ("field" + _) mkString " "}

             ... on Query {
               getDroid
             }

             ...Foo
           }

           fragment Foo on Query {
             human(id: "1003") @include(if: $$xx) {id, name, friends{name}}
           }
        """
      ) match {
        case Success(v) => v
        case Failure(error: SyntaxError) =>
          println(error.formattedError)
          throw error
        case Failure(e) => throw e
      }

      val vars = Map("xx" -> true)

      {
        import sangria.integration.SprayJsonSupport._

        println("\nSprayJson marshalling:\n")
        println(Await.result(
          Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
              .execute(ast, arguments = Some(vars)), Duration.Inf).prettyPrint)
      }
    }
  }

}
