package sangria.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import sangria.schema.Schema

@State(Scope.Thread)
class ExecutorBenchmark {
  import ExecutorBenchmark._

  @Param(Array("Old", "New"))
  var executorType: String = _

  @Param(Array("1", "10", "100"))
  var depth: Int = _

  @Param(Array("1", "10", "100"))
  var breadth: Int = _

  private var schema: Schema[UCtx, Any] = _

  @Setup
  def setup(): Unit = {
    // TODO: build schema with configured depth+breadth and setup Executor
  }

  @Benchmark
  def bench(bh: Blackhole): Unit = ()
}

private object ExecutorBenchmark {
  class UCtx
}
