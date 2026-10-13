package sangria.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import sangria.parser.QueryParser

@State(Scope.Thread)
class ParseHugeQueryBenchmark {
  // number of top-level fields; each has arguments, an alias and a nested selection
  @Param(Array("1000", "10000", "50000"))
  var size: Int = _

  var query: String = _

  @Setup
  def setup(): Unit =
    query = (0 until size)
      .map(i =>
        s"""f$i: field(id: $i, name: "n$i", list: [1, 2, 3]) { a b @include(if: true) ...F }""")
      .mkString("query Huge($v: Int) {\n", "\n", "\n}\nfragment F on T { x y z }")

  @Benchmark
  def parseHugeQuery(bh: Blackhole): Unit = bh.consume(QueryParser.parse(query).get)
}
