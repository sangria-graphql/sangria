package sangria.execution

import language.postfixOps

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.schema._

import scala.concurrent.{Await, Future}
import scala.util.Success
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

class ExecutorSpec extends WordSpec with Matchers {

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

  val schema = Schema(DataType)

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

      Await.result(Executor(schema, new TestSubject).execute(doc, arguments = Some(Map("size" -> 100))), 5 seconds) should be (expected)
    }


//    "merge parallel fragments" in {
//      val Type = ObjectType("Type", List[])
//    }
  }
}
