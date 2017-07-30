package sangria.execution.batch

import scala.language.higherKinds
import org.scalatest.{Matchers, WordSpec}
import sangria.macros._
import sangria.marshalling._
import sangria.schema._
import sangria.util.FutureResultSupport
import spray.json._

class BatchExecutorSpec extends WordSpec with Matchers with FutureResultSupport {
  val IdsArg = Argument("ids", ListInputType(IntType))
  val IdArg = Argument("id", IntType)
  val NameArg = Argument("name", StringType)

  val DataType = ObjectType("Data", fields[Unit, Int](
    Field("id", IntType, resolve = _.value)))

  lazy val QueryType: ObjectType[Unit, Unit] = ObjectType("Query", () ⇒ fields[Unit, Unit](
    Field("ids", ListType(IntType), resolve = _ ⇒ List(1, 2)),
    Field("ids1", ListType(IntType), resolve = _ ⇒ List(4, 5)),
    Field("ids2", ListType(IntType), resolve = _ ⇒ Nil),
    Field("greet", StringType,
      arguments = NameArg :: Nil,
      resolve = c ⇒ s"Hello, ${c arg NameArg}!"),
    Field("nested", QueryType, resolve = _ ⇒ ()),
    Field("stuff", ListType(DataType),
      arguments = IdsArg :: Nil,
      resolve = _.arg(IdsArg)),
    Field("single", DataType,
      arguments = IdArg :: Nil,
      resolve = _.arg(IdArg)),
    Field("stuff1", StringType,
      arguments = IdsArg :: Nil,
      resolve = _.arg(IdsArg).mkString(", "))
  ))

  val schema = Schema(QueryType, directives = BuiltinDirectives :+ BatchExecutor.ExportDirective)

  "BatchExecutor" should {
    "Batch multiple queries and ensure correct execution order" in {
      val query =
        gql"""
          query q1 {
            ids @export(as: "ids")
            foo: ids @export(as: "foo")
            nested {
              ...Foo
            }
          }

          fragment Foo on Query {
            ids1 @export(as: "ids")
            aaa: ids1 @export(as: "ids")
          }

          query q2($$name: String!) {
            stuff(ids: $$ids) {id}

            ...Bar

            single(id: $$foo) {id}

            greet(name: $$name)
          }

          fragment Bar on Query {
            stuff1(ids: $$ids)
          }

          query q3 {
            ids2 @export(as: "ids")
            stuff(ids: 2) {id}
          }
        """

      val vars = ScalaInput.scalaInput(Map(
        "ids" → Vector(111, 222, 444),
        "bar" → Map("a" → "hello", "b" → "world"),
        "name" → "Bob"))

      import monix.execution.Scheduler.Implicits.global
      import sangria.execution.ExecutionScheme.Stream
      import sangria.marshalling.sprayJson._
      import sangria.streaming.monix._

      val res = BatchExecutor.executeBatch(schema, query,
        operationNames = List("q1", "q2", "q3"),
        variables = vars)

      val expectedResults = Set(
        """
        {
          "data": {
            "ids": [1, 2],
            "foo": [1, 2],
            "nested": {
              "ids1": [4, 5],
              "aaa": [4, 5]
            }
          }
        }
        """,
        """
        {
          "data": {
            "ids2": [],
            "stuff": [
              {"id": 2}
            ]
          }
        }
        """,
        """
        {
          "data": {
            "stuff": [
              {"id": 1},
              {"id": 2},
              {"id": 4},
              {"id": 5},
              {"id": 4},
              {"id": 5},
              {"id": 111},
              {"id": 222},
              {"id": 444}
            ],
            "stuff1": "1, 2, 4, 5, 4, 5, 111, 222, 444",
            "single": { "id": 1 },
            "greet": "Hello, Bob!"
          }
        }
        """).map(_.parseJson)

      res.toListL.runAsync.await.toSet should be (expectedResults)
    }
  }
}
