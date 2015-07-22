package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.AwaitSupport

import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class ExecutorSpec extends WordSpec with Matchers with AwaitSupport {
  class TestSubject {
    def a: Option[String] = Some("Apple")
    def b: Option[String] = Some("Banana")
    def c: Option[String] = Some("Cookie")
    def d: Option[String] = Some("Donut")
    def e: Option[String] = Some("Egg")
    val f: Option[String] = Some("Fish")
    def deep: Option[DeepTestSubject] = Some(new DeepTestSubject)
    def pic(size: Option[Int]) = "Pic of size: " + (size getOrElse 50)
    def future: Future[Option[TestSubject]] = Future.successful(Some(new TestSubject))
  }

  class DeepTestSubject {
    def a: Option[String] = Some("Already Been Done")
    def b: Option[String] = Some("Boring")
    def c: List[Option[String]] = Some("Contrived") :: None :: Some("Confusing") :: Nil
    def deeper: List[Option[TestSubject]] = Some(new TestSubject) :: null :: Some(new TestSubject) :: Nil
  }

  val DeepDataType = ObjectType("DeepDataType", () => List[Field[Unit, DeepTestSubject]](
    Field("a", OptionType(StringType), resolve = _.value.a),
    Field("b", OptionType(StringType), resolve = _.value.b),
    Field("c", OptionType(ListType(OptionType(StringType))), resolve = _.value.c),
    Field("deeper", OptionType(ListType(OptionType(DataType))), resolve = _.value.deeper)
  ))

  val DataType: ObjectType[Unit, TestSubject] = ObjectType("DataType", () => List[Field[Unit, TestSubject]](
    Field("a", OptionType(StringType), resolve = _.value.a),
    Field("b", OptionType(StringType), resolve = _.value.b),
    Field("c", OptionType(StringType), resolve = _.value.c),
    Field("d", OptionType(StringType), resolve = _.value.d),
    Field("e", OptionType(StringType), resolve = _.value.e),
    Field("f", OptionType(StringType), resolve = _.value.f),
    Field("pic", OptionType(StringType),
      arguments = Argument("size", OptionInputType(StringType)) :: Nil,
      resolve = ctx => ctx.value.pic(ctx.argOpt[Int]("size"))),
    Field("deep", OptionType(DeepDataType), resolve = _.value.deep),
    Field("future", OptionType(DataType), resolve = _.value.future)
  ))

  val ParallelFragmentType: ObjectType[Unit, Unit] = ObjectType("Type", () => List[Field[Unit, Unit]](
    Field("a", OptionType(StringType), resolve = _ => "Apple"),
    Field("b", OptionType(StringType), resolve = _ => "Banana"),
    Field("c", OptionType(StringType), resolve = _ => "Cherry"),
    Field("deep", OptionType(ParallelFragmentType), resolve = _ => ())
  ))

  "Execute: Handles basic execution tasks" should {
    "execute arbitrary code" in {
      val Success(doc) = QueryParser.parse("""
        query Example($size: Int) {
          a,
          b,
          x: c
          ...c
          f
          ...on DataType {
            pic(size: $size)
            future {
              a
            }
          }
          deep {
            a
            b
            c
            deeper {
              a
              b
            }
          }
        }

        fragment c on DataType {
          d
          e
        }
      """)

      val expected = Map(
        "data" -> Map(
          "a" -> "Apple",
          "b" -> "Banana",
          "x" -> "Cookie",
          "d" -> "Donut",
          "e" -> "Egg",
          "f" -> "Fish",
          "pic" -> "Pic of size: 100",
          "future" -> Map("a" -> "Apple"),
          "deep" -> Map(
            "a" -> "Already Been Done",
            "b" -> "Boring",
            "c" -> List("Contrived", null, "Confusing"),
            "deeper" -> List(
              Map("a" -> "Apple", "b" -> "Banana"),
              null,
              Map("a" -> "Apple", "b" -> "Banana")
            )
          )
        )
      )

      val schema = Schema(DataType)

      Executor(schema, new TestSubject).execute(doc, arguments = Some(Map("size" -> 100))).await should be (expected)
    }


    "merge parallel fragments" in {
      val schema = Schema(ParallelFragmentType)

      val Success(doc) = QueryParser.parse("""
        { a, ...FragOne, ...FragTwo }

        fragment FragOne on Type {
          b
          deep { b, deeper: deep { b } }
        }

        fragment FragTwo on Type {
          c
          deep { c, deeper: deep { c } }
        }
      """)

      val expected = Map(
        "data" -> Map(
          "a" -> "Apple",
          "b" -> "Banana",
          "c" -> "Cherry",
            "deep" -> Map(
            "b" -> "Banana",
            "c" -> "Cherry",
              "deeper" -> Map(
              "b" -> "Banana",
              "c" -> "Cherry")))
      )

      Executor(schema).execute(doc).await should be (expected)
    }

    "threads context correctly" in {
      case class Thing(a: Option[String])

      var resolvedCtx: Option[String] = None

      val schema = Schema(ObjectType("Type", List[Field[Unit, Thing]](
        Field("a", OptionType(StringType), resolve = ctx => {resolvedCtx = ctx.value.a; ctx.value.a}))))

      val Success(doc) = QueryParser.parse("query Example { a }")
      Executor(schema, Thing(Some("thing"))).execute(doc).await should be (Map("data" -> Map("a" -> "thing")))
      resolvedCtx should be (Some("thing"))
    }

    "correctly threads arguments" in {
      var resolvedArgs: Map[String, Any] = Map.empty

      val schema = Schema(ObjectType("Type", List[Field[Unit, Unit]](
        Field("b", OptionType(StringType),
          arguments = Argument("numArg", OptionInputType(IntType)) :: Argument("stringArg", OptionInputType(StringType)) :: Nil,
          resolve = ctx => {resolvedArgs = ctx.args; None}))))

      val Success(doc) = QueryParser.parse("""
        query Example {
          b(numArg: 123, stringArg: "foo")
        }
      """)

      Executor(schema).execute(doc).await
      resolvedArgs should be (Map("numArg" -> 123, "stringArg" -> "foo"))
    }

    "null out error subtrees" in {
      class Data {
        def sync = "sync"
        def syncError = throw new IllegalStateException("Error getting syncError")
        def async = Future.successful("async")
        def asyncReject: Future[String] = Future.failed(new IllegalStateException("Error getting asyncReject"))
        def asyncError: Future[String] = Future {
          throw new IllegalStateException("Error getting asyncError")
        }
      }

      val schema = Schema(ObjectType("Type", List[Field[Unit, Data]](
        Field("sync", OptionType(StringType), resolve = _.value.sync),
        Field("syncError", OptionType(StringType), resolve = _.value.syncError),
        Field("async", OptionType(StringType), resolve = _.value.async),
        Field("asyncReject", OptionType(StringType), resolve = ctx => ctx.value.asyncReject),
        Field("asyncError", OptionType(StringType), resolve = _.value.asyncError),
        Field("syncDeferError", OptionType(StringType),
          resolve = ctx => DeferredValue(throw new IllegalStateException("Error getting syncDeferError"))),
        Field("asyncDeferError", OptionType(StringType),
          resolve = _ => DeferredFutureValue(Future.failed(throw new IllegalStateException("Error getting asyncDeferError"))))
      )))

      val Success(doc) = QueryParser.parse("""
        {
          sync,
             syncError,
           async,
          asyncReject,
           asyncDeferError
              asyncError
              syncDeferError
        }""")

      val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), ResultMarshaller#Node] = {
        case (m, e: IllegalStateException) => m.mapNode(Seq("message" -> m.stringNode(e.getMessage)))
      }

      val result = Executor(schema, new Data, exceptionHandler = exceptionHandler).execute(doc).await.asInstanceOf[Map[String, Any]]

      val data = result("data")
      val errors = result("errors").asInstanceOf[List[_]]

      data should be (Map(
        "sync" -> "sync",
        "syncError" -> null,
        "async" -> "async",
        "asyncReject" -> null,
        "asyncError" -> null,
        "asyncDeferError" -> null,
        "syncDeferError" -> null
      ))

      errors should (have(size(5)) and
          contain(Map(
            "field" -> "syncError",
            "locations" -> List(Map("line" -> 4, "column" -> 14)),
            "message" -> "Error getting syncError")) and
          contain(Map(
            "field" -> "asyncReject",
            "locations" -> List(Map("line" -> 6, "column" -> 11)),
            "message" -> "Error getting asyncReject")) and
          contain(Map(
            "message" -> "Error getting asyncDeferError",
            "field" -> "asyncDeferError",
            "locations" -> List(Map("line" -> 7, "column" -> 12)))) and
          contain(Map(
            "message" -> "Error getting syncDeferError",
            "field" -> "syncDeferError",
            "locations" -> List(Map("line" -> 9, "column" -> 15)))) and
          contain(Map(
            "field" -> "asyncError",
            "locations" -> List(Map("line" -> 8, "column" -> 15)),
            "message" -> "Error getting asyncError")))
    }

    "use the inline operation if no operation is provided" in {
      val schema = Schema(ObjectType("Type", List[Field[Unit, Unit]](
        Field("a", OptionType(StringType), resolve = _ => "b"))))
      val Success(doc) = QueryParser.parse("{ a }")

      Executor(schema).execute(doc).await should be (Map("data" -> Map("a" -> "b")))
    }

    "use the only operation if no operation is provided" in {
      val schema = Schema(ObjectType("Type", List[Field[Unit, Unit]](
        Field("a", OptionType(StringType), resolve = _ => "b"))))
      val Success(doc) = QueryParser.parse("query Example { a }")

      Executor(schema).execute(doc).await should be (Map("data" -> Map("a" -> "b")))
    }

    "throw if no operation is provided with multiple operations" in {
      val schema = Schema(ObjectType("Type", List[Field[Unit, Unit]](
        Field("a", OptionType(StringType), resolve = _ => "b"))))
      val Success(doc) = QueryParser.parse("query Example { a } query OtherExample { a }")

      Executor(schema).execute(doc).await should be (
        Map("errors" -> List(Map("message" -> "Must provide operation name if query contains multiple operations"))))
    }

    "use the query schema for queries" in {
      val schema = Schema(
        ObjectType("Q", List[Field[Unit, Unit]](Field("a", OptionType(StringType), resolve = _ => "b"))),
        Some(ObjectType("M", List[Field[Unit, Unit]](Field("c", OptionType(StringType), resolve = _ => "d")))))
      val Success(doc) = QueryParser.parse("query Q { a } mutation M { c }")

      Executor(schema).execute(doc, Some("Q")).await should be  (Map("data" -> Map("a" -> "b")))
    }

    "avoid recursion" in {
      val schema = Schema(ObjectType("Type", List[Field[Unit, Unit]](
        Field("a", OptionType(StringType), resolve = _ => "b"))))

      val Success(doc) = QueryParser.parse("""
        query Q {
          a
          ...Frag
          ...Frag
        }

        fragment Frag on Type {
          a,
          ...Frag
        }
      """)

      Executor(schema).execute(doc, Some("Q")).await should be  (Map("data" -> Map("a" -> "b")))
    }

    "not include illegal fields in output" in {
      val schema = Schema(
        ObjectType("Q", List[Field[Unit, Unit]](Field("a", OptionType(StringType), resolve = _ => "b"))),
        Some(ObjectType("M", List[Field[Unit, Unit]](Field("c", OptionType(StringType), resolve = _ => "d")))))
      val Success(doc) = QueryParser.parse("mutation M { thisIsIllegalDontIncludeMe }")

      Executor(schema).execute(doc).await should be  (Map("data" -> Map()))
    }
  }
}
