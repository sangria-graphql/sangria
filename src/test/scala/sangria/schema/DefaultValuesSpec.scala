package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.integration.ToInput
import sangria.macros._
import sangria.util.AwaitSupport
import scala.concurrent.ExecutionContext.Implicits.global

class DefaultValuesSpec extends WordSpec with Matchers with AwaitSupport {
  def check[Default](inputType: InputType[_], defaultValue: Default, expectedResult: Any, expectedDefault: String)(implicit ev: ToInput[Default, _]) = {
    import sangria.integration.sprayJson._
    import spray.json._

    class CaptureCtx(var arg: Option[Any] = None)

    val arg = Argument("test", OptionInputType(inputType), defaultValue = defaultValue)

    val QueryType = ObjectType("Query", fields[CaptureCtx, Unit](
      Field("foo", StringType,
        arguments = arg :: Nil,
        resolve = ctx => {
          ctx.ctx.arg = Some(ctx.arg[Any]("test"))
          "result"
        })
    ))

    val schema = Schema(QueryType)

    val query = graphql"{foo}"

    val ctx = new CaptureCtx

    Executor.execute(schema, query, userContext = ctx).await should be (JsObject("data" -> JsObject("foo" -> JsString("result"))))

    ctx.arg should be (Some(expectedResult))

    val introspectionQuery =
      graphql"""
        {
          __schema {
            queryType {
              fields {
                args {
                  defaultValue
                }
              }
            }
          }
        }
      """

    Executor.execute(schema, introspectionQuery, userContext = ctx).await should be (
      JsObject("data" ->
        JsObject("__schema" ->
          JsObject("queryType" ->
            JsObject("fields" -> JsArray(
              JsObject("args" -> JsArray(
                JsObject("defaultValue" -> JsString(expectedDefault))))))))))

  }

  def complexInputType[S, C](sharesDefault: S, commentsDefault: C)(implicit sev: ToInput[S, _], cev: ToInput[C, _]) = {
    val SharesType = InputObjectType("Shares", fields = List(
      InputField("twitter", OptionInputType(LongType), defaultValue = 123),
      InputField("facebook", OptionInputType(LongType), defaultValue = 1)
    ))

    val CommentType = InputObjectType("Comment", fields = List(
      InputField("author", OptionInputType(StringType), defaultValue = "anonymous"),
      InputField("text", StringType),
      InputField("likes", OptionInputType(BigDecimalType), defaultValue = BigDecimal("1.5"))
    ))

    val BlogType = InputObjectType("Blog", fields = List(
      InputField("title", StringType),
      InputField("text", OptionInputType(StringType), defaultValue = "Hello World!"),
      InputField("views", OptionInputType(IntType), defaultValue = 12),
      InputField("tags", OptionInputType(ListInputType(StringType)), defaultValue = List("beginner", "scala")),
      InputField("shares", OptionInputType(SharesType), defaultValue = sharesDefault),
      InputField("comments", OptionInputType(ListInputType(CommentType)), defaultValue = commentsDefault)
    ))

    BlogType
  }

  "Default values" when {
    "used with NotNull input types" should {
      "not allow default values for NotNull arguments" in {
        an [IllegalArgumentException] should be thrownBy Argument("boom", IntType, defaultValue = 1)
      }

      "not allow default values for NotNull input fields" in {
        an [IllegalArgumentException] should be thrownBy InputField("boom", IntType, defaultValue = 1)
      }
    }

    "used with scala map-like data structures" should {
      "default Int" in check(IntType,
        defaultValue = 1,
        expectedResult = 1,
        expectedDefault = "1")

      "default Long" in check(LongType,
        defaultValue = 13545436553654L,
        expectedResult = 13545436553654L,
        expectedDefault = "13545436553654")

      "default BigDecimal" in check(BigDecimalType,
        defaultValue = BigDecimal("47656823564532764576325476352742.764576437"),
        expectedResult = BigDecimal("47656823564532764576325476352742.764576437"),
        expectedDefault = "47656823564532764576325476352742.764576437")

      "default BigInt" in check(BigIntType,
        defaultValue = BigInt("47656823564532764576325476352742"),
        expectedResult = BigInt("47656823564532764576325476352742"),
        expectedDefault = "47656823564532764576325476352742")

      "default Float" in check(FloatType,
        defaultValue = 234.05D,
        expectedResult = 234.05D,
        expectedDefault = "234.05")

      "default String" in check(StringType,
        defaultValue = "Hello",
        expectedResult = "Hello",
        expectedDefault = "\"Hello\"")

      "default Boolean" in check(BooleanType,
        defaultValue = true,
        expectedResult = true,
        expectedDefault = "true")

      "default scala list of Int" in check(ListInputType(IntType),
        defaultValue = List(1, 2, 4),
        expectedResult = List(1, 2, 4),
        expectedDefault = "[1,2,4]")

      "default scala list of String" in check(ListInputType(StringType),
        defaultValue = Vector("Hello", "World"),
        expectedResult = List("Hello", "World"),
        expectedDefault = "[\"Hello\",\"World\"]")

      val ScalaInputType = complexInputType(
        sharesDefault = Map("twitter" -> 78),
        commentsDefault = List(Map("text" -> "Foo"), Map("text" -> "bar", "likes" -> 3.2D)))

      "default scala complex object" in check(
        ScalaInputType,
          defaultValue = Map("title" -> "Post #1", "text" -> "Amazing!", "comments" -> List(Map("text" -> "First! :P"))),
          expectedResult = Map(
            "title" -> "Post #1",
            "text" -> "Amazing!",
            "tags" -> List("beginner", "scala"),
            "views" -> 12,
            "shares" -> Map("twitter" -> 78, "facebook" -> 1),
            "comments" -> List(Map("author" -> "anonymous", "text" -> "First! :P", "likes" -> 1.5))),
          expectedDefault = "{\"tags\":[\"beginner\",\"scala\"],\"text\":\"Amazing!\",\"shares\":{\"twitter\":78,\"facebook\":1},\"views\":12,\"title\":\"Post #1\",\"comments\":[{\"author\":\"anonymous\",\"text\":\"First! :P\",\"likes\":1.5}]}")
    }

    "used with JSON values" should {
      import spray.json._
      import sangria.integration.sprayJson.sprayJsonToInput

      "default Int" in check(IntType,
        defaultValue = JsNumber(1),
        expectedResult = 1,
        expectedDefault = "1")

      "default Long" in check(LongType,
        defaultValue = JsNumber(17465784658743L),
        expectedResult = 17465784658743L,
        expectedDefault = "17465784658743")

      "default BigDecimal" in check(BigDecimalType,
        defaultValue = JsNumber(BigDecimal("1746578465874346587465843.48564736578436")),
        expectedResult = BigDecimal("1746578465874346587465843.48564736578436"),
        expectedDefault = "1746578465874346587465843.48564736578436")

      "default String" in check(StringType,
        defaultValue = JsString("Bananas!"),
        expectedResult = "Bananas!",
        expectedDefault = "\"Bananas!\"")

      "default scala list of Int" in check(ListInputType(IntType),
        defaultValue = JsArray(JsNumber(1), JsNumber(23), JsNumber(56)),
        expectedResult = List(1, 23, 56),
        expectedDefault = "[1,23,56]")

      "default scala list of String" in check(ListInputType(StringType),
        defaultValue = JsArray(JsString("foo"), JsString("bar")),
        expectedResult = List("foo", "bar"),
        expectedDefault = "[\"foo\",\"bar\"]")

      val JsonInputType = complexInputType(
        sharesDefault = JsObject("twitter" -> JsNumber(78)),
        commentsDefault = """[{"text": "Foo"}, {"text": "bar", "likes": 3.2}]""".parseJson)

      "default scala complex object" in check(
        JsonInputType,
        defaultValue = """{"title": "Post #1", "text": "Amazing!", "comments": [{"text": "First! :P"}]}""".parseJson,
        expectedResult = Map(
          "title" -> "Post #1",
          "text" -> "Amazing!",
          "tags" -> List("beginner", "scala"),
          "views" -> 12,
          "shares" -> Map("twitter" -> 78, "facebook" -> 1),
          "comments" -> List(Map("author" -> "anonymous", "text" -> "First! :P", "likes" -> 1.5))),
        expectedDefault = "{\"tags\":[\"beginner\",\"scala\"],\"text\":\"Amazing!\",\"shares\":{\"twitter\":78,\"facebook\":1},\"views\":12,\"title\":\"Post #1\",\"comments\":[{\"author\":\"anonymous\",\"text\":\"First! :P\",\"likes\":1.5}]}")
    }
  }
}
