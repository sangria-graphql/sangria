package sangria.util

import org.scalatest.{Matchers, WordSpec}
import sangria.TestData.CharacterRepo
import sangria.execution.Executor
import sangria.parser.{SyntaxError, QueryParser}
import sangria.schema.TestSchema

import scala.collection.immutable.RedBlackTree
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

class TemporarySpec extends WordSpec with Matchers {

  "Foo" should {
    "foo" in {
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
             human(id: "1003") @include(if: $$xx)
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

      println(Await.result(Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo).execute(ast, arguments = Some(vars)), Duration.Inf).foo)
    }
  }

}
