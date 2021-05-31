package sangria.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import sangria.ast.Document
import sangria.execution.Executor
import sangria.execution.experimental.Executor2
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation.QueryValidator
import scala.annotation.tailrec
import scala.concurrent.Await
import scala.concurrent.duration.Duration

// TODO: figure out if there is any interplay between Scope.Thread and the
// ExecutionContext used with Executor
@State(Scope.Thread)
class ExecutorBenchmark {
  import ExecutorBenchmark._

  @Param(Array("Default", "Experimental"))
  var executorType: String = _

  @Param(Array("150"))
  var depth: Int = _

  @Param(Array("150"))
  var breadth: Int = _

  private var executor: Executor[UCtx, Any] = _
  private var query: Document = _

  @Setup
  def setup(): Unit = {
    query = mkQuery(breadth, depth)
    val schema = mkSchema(breadth)

    executorType match {
      case "Default" =>
        executor = Executor.Default(
          schema,
          // This benchmark focuses on performance of the underlying Resolver.
          // Let's disable query validation for maximum speed.
          queryValidator = QueryValidator.empty
        )(scala.concurrent.ExecutionContext.global)

      case "Experimental" =>
        executor = Executor2(
          schema,
          queryValidator = QueryValidator.empty
        )(scala.concurrent.ExecutionContext.global)
    }
  }

  @Benchmark
  def bench(bh: Blackhole): Unit = {
    val fut = executor.execute(query, (), ())
    val result = Await.result(fut, Duration.Inf)
    bh.consume(result)
  }

  private def mkSchema(breadth: Int): Schema[UCtx, Any] = {
    lazy val branch: ObjectType[UCtx, Any] = ObjectType[UCtx, Any](
      "Branch",
      () => {
        val bf = mkField("branch", branch, ctx => ())
        val leaves =
          (1 to breadth).map(i => mkField(leafFieldName(i), StringType, ctx => i.toString)).toList
        bf :: leaves
      }
    )

    Schema(
      branch.rename("Query")
    )
  }

  private def mkQuery(breadth: Int, depth: Int): Document = {
    @tailrec
    def loop(acc: StringBuilder, d: Int): String =
      d match {
        case 0 => acc.toString
        case x =>
          val leafSelectionSet = (1 to breadth).map(i => leafFieldName(i)).mkString(" ")
          loop(
            acc.append(s" branch { $leafSelectionSet "),
            x - 1
          )
      }

    val q = "query { " + loop(new StringBuilder, depth) + (" }" * depth) + "}"
    QueryParser.parse(q).get
  }

  private def leafFieldName(i: Int): String = s"leaf_$i"

  private def mkField(
      name: String,
      fieldType: OutputType[_],
      resolve: Context[UCtx, Any] => Action[UCtx, Any]): Field[UCtx, Any] =
    new Field(
      name,
      fieldType,
      None,
      Nil,
      resolve,
      None,
      Nil,
      None,
      () => Nil,
      Vector.empty,
      Vector.empty)
}

private object ExecutorBenchmark {
  type UCtx = Unit
}
