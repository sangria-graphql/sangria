package sangria.benchmarks

import org.openjdk.jmh.annotations._
import sangria.util.StringUtil

@State(Scope.Thread)
class StringUtilBenchmark {
  @Param(Array("empty input", "empty options", "abc", "description", "length"))
  var testCase: String = _

  var input: String = _
  var options: Seq[String] = _
  var expected: Seq[String] = _

  @Setup
  def setup(): Unit =
    testCase match {
      case "empty input" =>
        input = ""
        options = Seq("ab", "e")
        expected = Seq("e")
      case "empty options" =>
        input = "abc"
        options = Nil
        expected = Nil
      case "abc" =>
        input = "abc"
        options = Seq("a", "ab", "abc")
        expected = Seq("abc", "ab")
      case "description" =>
        input = "description"
        options = "description" :: (1 to 50).map(i => s"ab_$i").toList
        expected = Seq("description")
      case "length" =>
        input = "length"
        options = "length" :: (1 to 50).map(i => s"name_$i").toList
        expected = Seq("length")
    }

  @Benchmark
  def benchCurrent(): Unit = {
    val results = StringUtil.suggestionList(input, options)
    assert(results == expected, s"results = $results, expected = $expected")
  }
}
