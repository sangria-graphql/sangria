package sangria.benchmarks

import org.openjdk.jmh.annotations._
import org.openjdk.jmh.infra.Blackhole
import sangria.ast.Document
import sangria.execution.Executor
import sangria.parser.QueryParser
import sangria.schema._

import scala.concurrent.Await
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

@State(Scope.Thread)
class ParseExecuteBenchmark {
  case class Item(id: Int, name: String)

  val ItemType: ObjectType[Unit, Item] = ObjectType(
    "Item",
    fields[Unit, Item](
      Field("id", IntType, resolve = _.value.id),
      Field("name", StringType, resolve = _.value.name)))

  val schema: Schema[Unit, Unit] = Schema(
    ObjectType(
      "Query",
      fields[Unit, Unit](
        Field(
          "items",
          ListType(ItemType),
          arguments = Argument("n", IntType) :: Nil,
          resolve = c => List.tabulate(c.arg[Int]("n"))(i => Item(i, s"item$i")))
      )
    ))

  @Param(Array("10", "1000"))
  var size: Int = _

  var queryString: String = _
  var document: Document = _

  @Setup
  def setup(): Unit = {
    queryString = s"query { items(n: $size) { id name } }"
    document = QueryParser.parse(queryString).get
  }

  @Benchmark
  def parse(bh: Blackhole): Unit = bh.consume(QueryParser.parse(queryString))

  @Benchmark
  def execute(bh: Blackhole): Unit =
    bh.consume(Await.result(Executor.execute(schema, document), 10.seconds))
}
