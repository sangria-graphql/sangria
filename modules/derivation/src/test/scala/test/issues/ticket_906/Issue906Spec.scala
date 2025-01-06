package test.issues.ticket_906

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.util._

import scala.concurrent.ExecutionContext.Implicits.global

import Issue906Context._

final class Issue906Spec extends AnyWordSpec with Matchers with FutureResultSupport {

  "Derivation" should {
    "Support Future values" in {

      val query = QueryParser
        .parse(
          s"""
             query SomeTest {
                 mySample {
                   myFutureString
                 }
             }
            """
        )
        .fold(throw _, identity)

      Executor.execute(MySample.schema, query, MyRepository.sample).await should be(
        Map(
          "data" -> Map(
            "mySample" -> Map(
              "myFutureString" -> "Hello World"
            )
          )
        )
      )

    }
  }

}
