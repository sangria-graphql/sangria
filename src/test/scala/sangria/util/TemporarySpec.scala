package sangria.util

import org.scalatest.{Matchers, WordSpec}
import sangria.TestData.CharacterRepo
import sangria.execution.Executor
import sangria.parser.{SyntaxError, QueryParser}
import sangria.schema.TestSchema

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

class TemporarySpec extends WordSpec with Matchers {

  "Foo" should {
    "foo" in {
      val ast = QueryParser.parse(
        """
           query testQ ($xx: Boolean!) {
             ... on Query {
               getDroid
             }

             ...Foo
           }

           fragment Foo on Query {
             getHuman @include(if: $xx)
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
