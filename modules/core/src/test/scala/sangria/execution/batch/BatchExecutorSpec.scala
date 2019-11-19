package sangria.execution.batch

import scala.concurrent.ExecutionContext
import cats.effect.{ContextShift, IO}
import org.scalatest.{Matchers, WordSpec}
import sangria.macros._
import sangria.marshalling._
import sangria.schema._
import sangria.util.{FutureResultSupport, Pos}
import spray.json._
import sangria.marshalling.sprayJson._
import sangria.util.Fs2Support._
import sangria.util.SimpleGraphQlSupport._

class BatchExecutorSpec extends WordSpec with Matchers with FutureResultSupport {
  implicit val ec: ExecutionContext = ExecutionContext.global
  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  val IdsArg = Argument("ids", ListInputType(IntType))
  val IdArg = Argument("id", IntType)
  val NameArg = Argument("name", StringType)
  val NamesArg = Argument("names", ListInputType(StringType))

  val DataType = ObjectType("Data", fields[Unit, (Int, String)](
    Field("id", IntType, resolve = _.value._1),
    Field("name", StringType, resolve = _.value._2)))

  lazy val DataInputType = InputObjectType("DataInput", List(
    InputField("id", IntType),
    InputField("name", StringType)))

  val DataArg = Argument("data", ListInputType(DataInputType))

  lazy val QueryType: ObjectType[Unit, Unit] = ObjectType("Query", () => fields[Unit, Unit](
    Field("ids", ListType(IntType), resolve = _ => List(1, 2)),
    Field("ids1", ListType(IntType), resolve = _ => List(4, 5)),
    Field("ids2", ListType(IntType), resolve = _ => Nil),
    Field("name1", StringType, resolve = _ => "some name 1"),
    Field("name2", OptionType(StringType), resolve = _ => "some name 2"),
    Field("greet", StringType,
      arguments = NameArg :: Nil,
      resolve = c => s"Hello, ${c arg NameArg}!"),
    Field("greetAll", StringType,
      arguments = NamesArg :: Nil,
      resolve = c => s"Hello, ${c arg NamesArg mkString " and "}!"),
    Field("nested", QueryType, resolve = _ => ()),
    Field("stuff", ListType(DataType),
      arguments = IdsArg :: Nil,
      resolve = _.arg(IdsArg).map(id => id -> s"data #$id")),
    Field("single", DataType,
      arguments = IdArg :: Nil,
      resolve = _.withArgs(IdArg)(id => id -> s"data #$id")),
    Field("stuff1", StringType,
      arguments = IdsArg :: Nil,
      resolve = _.arg(IdsArg).mkString(", "))
  ))

  lazy val MutationType = ObjectType("Mutation", fields[Unit, Unit](
    Field("createData", ListType(DataType),
      arguments = DataArg :: Nil,
      resolve = _.withArgs(DataArg)(_.map(d => d("id").asInstanceOf[Int] -> d("name").asInstanceOf[String])))))

  val schema = Schema(QueryType, Some(MutationType),
    directives = BuiltinDirectives :+ BatchExecutor.ExportDirective)

  "BatchExecutor" should {
    import sangria.execution.ExecutionScheme.Stream

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
        "ids" -> Vector(111, 222, 444),
        "bar" -> Map("a" -> "hello", "b" -> "world"),
        "name" -> "Bob"))

      val res = BatchExecutor.executeBatch(schema, query,
        operationNames = List("q1", "q2", "q3"),
        variables = vars)

      res.compile.toVector.unsafeRunSync.toSet should be (
        Set(
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
          """).map(_.parseJson))
    }

    "take the first element of the list" in {
      val query =
        gql"""
          query q1 {
            name2 @export(as: "name")
            nested {
              ...Foo
            }
          }

          fragment Foo on Query {
            name1 @export(as: "name")
          }

          query q2 {
            greet(name: $$name)
          }

          query q3 {
            ...Bar
          }

          fragment Bar on Query {
            greetAll(names: $$name)
          }

          query q4 {
            greet(name: $$name)
          }
        """

      val res = BatchExecutor.executeBatch(schema, query,
        operationNames = List("q3", "q1", "q2"),
        middleware = BatchExecutor.OperationNameExtension :: Nil)

      res.compile.toVector.unsafeRunSync.toSet should be (
        Set(
          """
          {
            "data": {
              "name2": "some name 2",
              "nested": {
                "name1": "some name 1"
              }
            },
            "extensions": {
              "batch": {
                "operationName": "q1"
              }
            }
          }
          """,
          """
          {
            "data": {
              "greet": "Hello, some name 2!"
            },
            "extensions": {
              "batch": {
                "operationName": "q2"
              }
            }
          }
          """,
          """
          {
            "data": {
              "greetAll": "Hello, some name 2 and some name 1!"
            },
            "extensions": {
              "batch": {
                "operationName": "q3"
              }
            }
          }
          """).map(_.parseJson))
    }

    "handle complex objects" in {
      val query =
        gql"""
          query q1 {
            stuff(ids: [1, 2]) @export(as: "myData") {
              id
              name
            }
            nested {
              ...Foo
            }
          }

          fragment Foo on Query {
            stuff(ids: [3, 4]) @export(as: "myData") {
              id
              name
            }
          }

          mutation q2 {
            createData(data: $$myData) {id name}
          }
        """

      val res = BatchExecutor.executeBatch(schema, query, operationNames = List("q1", "q2"))

      res.compile.toVector.unsafeRunSync.toSet should be (
        Set(
          """
          {
            "data": {
              "stuff": [{
                "id": 1,
                "name": "data #1"
              }, {
                "id": 2,
                "name": "data #2"
              }],
              "nested": {
                "stuff": [{
                  "id": 3,
                  "name": "data #3"
                }, {
                  "id": 4,
                  "name": "data #4"
                }]
              }
            }
          }
          """,
          """
          {
            "data": {
              "createData": [{
                "id": 1,
                "name": "data #1"
              }, {
                "id": 2,
                "name": "data #2"
              }, {
                "id": 3,
                "name": "data #3"
              }, {
                "id": 4,
                "name": "data #4"
              }]
            }
          }
          """).map(_.parseJson))
    }

    "produce type inference errors if same variables are used with incompatible types" in {
      val query =
        gql"""
          query q1 {
            ids @export(as: "ids")
          }

          query q2 {
            stuff(ids: $$ids) {id}
            single(id: $$ids) {id}

            greet(name: $$ids)
          }
        """

      checkContainsViolations(
        BatchExecutor.executeBatch(schema, query, operationNames = List("q1", "q2")).compile.toVector.unsafeRunSync,
        "Inferred variable '$ids' in operation 'q2' is used with two conflicting types: '[Int!]!' and 'Int!'." -> List(Pos(7, 24), Pos(8, 24)),
        "Inferred variable '$ids' in operation 'q2' is used with two conflicting types: '[Int!]!' and 'String!'." -> List(Pos(7, 24), Pos(10, 25)))
    }

    "not allow circular dependencies" in {
      val query =
        gql"""
          query q1 {
            ids @export(as: "from1")
            nested {
              ...Foo
            }
          }

          fragment Foo on Query {
            ids1 @export(as: "from1")
            stuff(ids: $$from3) {id}
          }

          query q2 {
            stuff(ids: $$from1) {id}

            ids @export(as: "from2")
          }

          query q3 {
            ids @export(as: "from3")
            stuff(ids: $$from2) {id}
          }
        """

      checkContainsViolations(
        BatchExecutor.executeBatch(schema, query, operationNames = List("q1", "q2", "q3")).compile.toVector.unsafeRunSync,
        "Operation 'q1' has a circular dependency at path 'q1($from3) -> q3($from2) -> q2($from1) -> q1'." -> List(Pos(2, 11)),
        "Operation 'q3' has a circular dependency at path 'q3($from2) -> q2($from1) -> q1($from3) -> q3'." -> List(Pos(20, 11)),
        "Operation 'q2' has a circular dependency at path 'q2($from1) -> q1($from3) -> q3($from2) -> q2'." -> List(Pos(14, 11)))
    }
  }

  "BatchExecutor (with single result)" should {
    import sangria.execution.ExecutionScheme.Extended


    "able to return a single result" in {
      val query =
        gql"""
          query q1 {
            stuff(ids: [1, 2]) @export(as: "myData") {
              id
              name
            }
            nested {
              ...Foo
            }
          }

          fragment Foo on Query {
            stuff(ids: [3, 4]) @export(as: "myData") {
              id
              name
            }
          }

          mutation q2 {
            createData(data: $$myData) {id name}
          }
        """

      val res = BatchExecutor.executeBatch(schema, query, operationNames = List("q1", "q2"))

      res.await.result should be (
        """
        {
          "data": {
            "stuff": [{
              "id": 1,
              "name": "data #1"
            }, {
              "id": 2,
              "name": "data #2"
            }],
            "nested": {
              "stuff": [{
                "id": 3,
                "name": "data #3"
              }, {
                "id": 4,
                "name": "data #4"
              }]
            }
          }
        }
        """.parseJson)
    }
  }
}
