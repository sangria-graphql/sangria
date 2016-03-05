package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.marshalling.{InputUnmarshaller, ResultMarshaller}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.macros._
import sangria.util.FutureResultSupport
import sangria.validation.QueryValidator
import InputUnmarshaller.mapVars

import scala.concurrent.Future
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class ExecutorSpec extends WordSpec with Matchers with FutureResultSupport {
  class TestSubject {
    def a: Option[String] = Some("Apple")
    def b: Option[String] = Some("Banana")
    def c: Option[String] = Some("Cookie")
    def d: Option[String] = Some("Donut")
    def e: Option[String] = Some("Egg")
    val f: Option[String] = Some("Fish")
    def deep: Option[DeepTestSubject] = Some(new DeepTestSubject)
    def deepColor(c: String): DeepTestSubject = new DeepTestSubject(c)
    def pic(size: Option[Int]) = "Pic of size: " + (size getOrElse 50)
    def future: Future[Option[TestSubject]] = Future.successful(Some(new TestSubject))
  }

  class DeepTestSubject(val color: String = "none") {
    def a: Option[String] = Some("Already Been Done")
    def b: Option[String] = Some("Boring")
    def c: List[Option[String]] = Some("Contrived") :: None :: Some("Confusing") :: Nil
    def deeper: List[Option[TestSubject]] = Some(new TestSubject) :: null :: Some(new TestSubject) :: Nil
  }

  case class Ctx(color: String = "green")

  case class LightColor(subj: TestSubject, color: String) extends Deferred[DeepTestSubject]
  case class FailColor(subj: TestSubject, color: String) extends Deferred[DeepTestSubject]

  class LightColorResolver extends DeferredResolver[Any] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Any) = deferred map {
      case LightColor(v, c) ⇒ Future.successful(v.deepColor("light" + c))
      case FailColor(v, c) ⇒ Future.failed(new IllegalStateException("error in resolver"))
    }
  }

  val DeepDataType = ObjectType("DeepDataType", () ⇒ fields[Ctx, DeepTestSubject](
    Field("a", OptionType(StringType), resolve = _.value.a),
    Field("b", OptionType(StringType), resolve = _.value.b),
    Field("c", OptionType(ListType(OptionType(StringType))), resolve = _.value.c),
    Field("color", StringType, resolve = _.value.color),
    Field("ctxColor", OptionType(StringType), resolve = _.ctx.color),
    Field("deeper", OptionType(ListType(OptionType(DataType))), resolve = _.value.deeper)
  ))

  val DataType: ObjectType[Ctx, TestSubject] = ObjectType("DataType", () ⇒ fields[Ctx, TestSubject](
    Field("a", OptionType(StringType), resolve = _.value.a),
    Field("b", OptionType(StringType), resolve = _.value.b),
    Field("c", OptionType(StringType), resolve = _.value.c),
    Field("d", OptionType(StringType), resolve = _.value.d),
    Field("e", OptionType(StringType), resolve = _.value.e),
    Field("f", OptionType(StringType), resolve = _.value.f),
    Field("ctxUpdating", DeepDataType, resolve =
      ctx ⇒ UpdateCtx(ctx.value.deepColor("blue"))(v ⇒ ctx.ctx.copy(color = v.color))),
    Field("ctxUpdatingFut", DeepDataType, resolve =
      ctx ⇒ UpdateCtx(Future.successful(ctx.value.deepColor("orange")))(v ⇒ ctx.ctx.copy(color = v.color))),
    Field("ctxUpdatingDef", DeepDataType, resolve =
      ctx ⇒ UpdateCtx(LightColor(ctx.value, "magenta"))(v ⇒ ctx.ctx.copy(color = v.color))),
    Field("ctxUpdatingDefFut", DeepDataType, resolve =
      ctx ⇒ UpdateCtx(DeferredFutureValue(Future.successful(LightColor(ctx.value, "red"))))(v ⇒ ctx.ctx.copy(color = v.color))),
    Field("def", DeepDataType, resolve = ctx ⇒ LightColor(ctx.value, "magenta")),
    Field("defFut", DeepDataType, resolve = ctx ⇒ DeferredFutureValue(Future.successful(LightColor(ctx.value, "red")))),
    Field("defFail", OptionType(DeepDataType), resolve = ctx ⇒ FailColor(ctx.value, "magenta")),
    Field("defFutFail", OptionType(DeepDataType), resolve = ctx ⇒ DeferredFutureValue(Future.successful(FailColor(ctx.value, "red")))),
    Field("pic", OptionType(StringType),
      arguments = Argument("size", OptionInputType(IntType)) :: Nil,
      resolve = ctx ⇒ ctx.value.pic(ctx.argOpt[Int]("size"))),
    Field("deep", OptionType(DeepDataType), resolve = _.value.deep),
    Field("future", OptionType(DataType), resolve = _.value.future)
  ))

  val ParallelFragmentType: ObjectType[Unit, Unit] = ObjectType("Type", () ⇒ fields[Unit, Unit](
    Field("a", OptionType(StringType), resolve = _ ⇒ "Apple"),
    Field("b", OptionType(StringType), resolve = _ ⇒ "Banana"),
    Field("c", OptionType(StringType), resolve = _ ⇒ "Cherry"),
    Field("d", StringType, resolve = _ ⇒ "Door"),
    Field("deep", OptionType(ParallelFragmentType), resolve = _ ⇒ ())
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
        "data" → Map(
          "a" → "Apple",
          "b" → "Banana",
          "x" → "Cookie",
          "d" → "Donut",
          "e" → "Egg",
          "f" → "Fish",
          "pic" → "Pic of size: 100",
          "future" → Map("a" → "Apple"),
          "deep" → Map(
            "a" → "Already Been Done",
            "b" → "Boring",
            "c" → List("Contrived", null, "Confusing"),
            "deeper" → List(
              Map("a" → "Apple", "b" → "Banana"),
              null,
              Map("a" → "Apple", "b" → "Banana")
            )
          )
        )
      )

      val schema = Schema(DataType)

      Executor(schema, new TestSubject, Ctx()).execute(doc, variables = mapVars(Map("size" → 100))).await should be (expected)
    }

    "respect max depth level" in {
      val Success(doc) = QueryParser.parse("""
        query Foo {
          d1: deep {
            deep {
              deep {
                deep {
                  deep {
                    a
                    deep {
                      b
                    }
                  }
                }
              }
            }
          }
          d2: deep {
            deep {
              deep {
                deep {
                  deep {
                    a
                    deep {
                      b
                      d
                      c
                    }
                  }
                }
              }
            }
          }
        }
        """)

      val expected = Map(
        "data" → Map(
          "d1" → Map(
            "deep" → Map(
              "deep" → Map(
                "deep" → Map(
                  "deep" → Map(
                    "a" → "Apple",
                    "deep" → Map(
                      "b" → null
                    )
                  )
                )
              )
            )
          ),
          "d2" → Map(
            "deep" → Map(
              "deep" → Map(
                "deep" → Map(
                  "deep" → Map(
                    "a" → "Apple",
                    "deep" → null
                  )
                )
              )
            )
          )
        ),
        "errors" → List(
          Map(
            "message" → "Max query depth 6 is reached.",
            "field" → "d1.deep.deep.deep.deep.deep.b",
            "locations" → List(Map("line" → 10, "column" → 23))
          ),
          Map(
            "message" → "Max query depth 6 is reached.",
            "field" → "d2.deep.deep.deep.deep.deep.b",
            "locations" → List(Map("line" → 24, "column" → 23))
          ),
          Map(
            "message" → "Max query depth 6 is reached.",
            "field" → "d2.deep.deep.deep.deep.deep.d",
            "locations" → List(Map("line" → 25, "column" → 23))
          )
        )
      )

      val schema = Schema(ParallelFragmentType)

      Executor.execute(schema, doc, maxQueryDepth = Some(6)).await should be (expected)
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
        "data" → Map(
          "a" → "Apple",
          "b" → "Banana",
          "c" → "Cherry",
            "deep" → Map(
            "b" → "Banana",
            "c" → "Cherry",
              "deeper" → Map(
              "b" → "Banana",
              "c" → "Cherry")))
      )

      Executor(schema).execute(doc).await should be (expected)
    }

    "threads context correctly" in {
      case class Thing(a: Option[String])

      var resolvedCtx: Option[String] = None

      val schema = Schema(ObjectType("Type", fields[Unit, Thing](
        Field("a", OptionType(StringType), resolve = ctx ⇒ {resolvedCtx = ctx.value.a; ctx.value.a}))))

      val Success(doc) = QueryParser.parse("query Example { a }")

      Executor(schema, Thing(Some("thing"))).execute(doc).await should be (Map("data" → Map("a" → "thing")))
      resolvedCtx should be (Some("thing"))
    }

    "correctly threads arguments" in {
      var resolvedArgs: Map[String, Any] = Map.empty

      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("b", OptionType(StringType),
          arguments = Argument("numArg", OptionInputType(IntType)) :: Argument("stringArg", OptionInputType(StringType)) :: Nil,
          resolve = ctx ⇒ {resolvedArgs = ctx.args.raw; None}))))

      val Success(doc) = QueryParser.parse("""
        query Example {
          b(numArg: 123, stringArg: "foo")
        }
      """)

      Executor(schema).execute(doc).await
      resolvedArgs should be (Map("numArg" → Some(123), "stringArg" → Some("foo")))
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

      val schema = Schema(ObjectType("Type", fields[Unit, Data](
        Field("sync", OptionType(StringType), resolve = _.value.sync),
        Field("syncError", OptionType(StringType), resolve = _.value.syncError),
        Field("async", OptionType(StringType), resolve = _.value.async),
        Field("asyncReject", OptionType(StringType), resolve = ctx ⇒ ctx.value.asyncReject),
        Field("asyncError", OptionType(StringType), resolve = _.value.asyncError),
        Field("syncDeferError", OptionType(StringType),
          resolve = ctx ⇒ DeferredValue(throw new IllegalStateException("Error getting syncDeferError"))),
        Field("asyncDeferError", OptionType(StringType),
          resolve = _ ⇒ DeferredFutureValue(Future.failed(throw new IllegalStateException("Error getting asyncDeferError"))))
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

      val exceptionHandler: Executor.ExceptionHandler = {
        case (m, e: IllegalStateException) ⇒ HandledException(e.getMessage)
      }

      val result = Executor(schema, new Data, exceptionHandler = exceptionHandler).execute(doc).await.asInstanceOf[Map[String, Any]]

      val data = result("data")
      val errors = result("errors").asInstanceOf[Seq[_]]

      data should be (Map(
        "sync" → "sync",
        "syncError" → null,
        "async" → "async",
        "asyncReject" → null,
        "asyncError" → null,
        "asyncDeferError" → null,
        "syncDeferError" → null
      ))

      errors should (have(size(5)) and
          contain(Map(
            "field" → "syncError",
            "locations" → List(Map("line" → 4, "column" → 14)),
            "message" → "Error getting syncError")) and
          contain(Map(
            "field" → "asyncReject",
            "locations" → List(Map("line" → 6, "column" → 11)),
            "message" → "Error getting asyncReject")) and
          contain(Map(
            "message" → "Error getting asyncDeferError",
            "field" → "asyncDeferError",
            "locations" → List(Map("line" → 7, "column" → 12)))) and
          contain(Map(
            "message" → "Error getting syncDeferError",
            "field" → "syncDeferError",
            "locations" → List(Map("line" → 9, "column" → 15)))) and
          contain(Map(
            "field" → "asyncError",
            "locations" → List(Map("line" → 8, "column" → 15)),
            "message" → "Error getting asyncError")))
    }

    "use the inline operation if no operation is provided" in {
      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("a", OptionType(StringType), resolve = _ ⇒ "b"))))
      val Success(doc) = QueryParser.parse("{ a }")

      Executor(schema).execute(doc).await should be (Map("data" → Map("a" → "b")))
    }

    "use the only operation if no operation is provided" in {
      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("a", OptionType(StringType), resolve = _ ⇒ "b"))))
      val Success(doc) = QueryParser.parse("query Example { a }")

      Executor(schema).execute(doc).await should be (Map("data" → Map("a" → "b")))
    }

    "throw if no operation is provided with multiple operations" in {
      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("a", OptionType(StringType), resolve = _ ⇒ "b"))))
      val Success(doc) = QueryParser.parse("query Example { a } query OtherExample { a }")

      val error = intercept [OperationSelectionError] (Executor(schema).execute(doc).await)

      error.getMessage should be ("Must provide operation name if query contains multiple operations")
    }

    "use correct schema type schema for operation" in {
      val schema = Schema(
        ObjectType("Q", fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ ⇒ "b"))),
        Some(ObjectType("M", fields[Unit, Unit](Field("c", OptionType(StringType), resolve = _ ⇒ "d")))),
        Some(ObjectType("S", fields[Unit, Unit](Field("e", OptionType(StringType), resolve = _ ⇒ "f")))))
      val Success(doc) = QueryParser.parse("query Q { a } mutation M { c } subscription S { e }")

      Executor(schema).execute(doc, Some("Q")).await should be (Map("data" → Map("a" → "b")))
      Executor(schema).execute(doc, Some("M")).await should be (Map("data" → Map("c" → "d")))
      Executor(schema).execute(doc, Some("S")).await should be (Map("data" → Map("e" → "f")))
    }

    "avoid recursion" in {
      val schema = Schema(ObjectType("Type", fields[Unit, Unit](
        Field("a", OptionType(StringType), resolve = _ ⇒ "b"))))

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

      Executor(schema, queryValidator = QueryValidator.empty).execute(doc, Some("Q")).await should be  (Map("data" → Map("a" → "b")))
    }

    "not include illegal fields in output" in {
      val schema = Schema(
        ObjectType("Q", fields[Unit, Unit](Field("a", OptionType(StringType), resolve = _ ⇒ "b"))),
        Some(ObjectType("M", fields[Unit, Unit](Field("c", OptionType(StringType), resolve = _ ⇒ "d")))))
      val Success(doc) = QueryParser.parse("mutation M { thisIsIllegalDontIncludeMe }")

      Executor(schema, queryValidator = QueryValidator.empty).execute(doc).await should be  (Map("data" → Map()))
    }

    "update context in query operations" in {
      val Success(doc) = QueryParser.parse("""
        query Q {
          ctxUpdating {
            ctxColor
          }

          ctxUpdatingFut {
            ctxColor
          }

          ctxUpdatingDef {
            ctxColor
          }

          ctxUpdatingDefFut {
            ctxColor
          }
        }
        """)

      val schema = Schema(DataType)

      Executor(schema, new TestSubject, Ctx(), deferredResolver = new LightColorResolver).execute(doc).await should be  (
        Map(
          "data" → Map(
            "ctxUpdating" → Map("ctxColor" → "blue"),
            "ctxUpdatingFut" → Map("ctxColor" → "orange"),
            "ctxUpdatingDef" → Map("ctxColor" → "lightmagenta"),
            "ctxUpdatingDefFut" → Map("ctxColor" → "lightred"))))
    }

    "resolve deferred values correctly" in {
      val query = graphql"""
        {
          def { color }
          defFut { color }
        }
        """

      val schema = Schema(DataType)

      Executor.execute(schema, query, root = new TestSubject, userContext = Ctx(), deferredResolver = new LightColorResolver).recover {
        case error: ErrorWithResolver ⇒ error.resolveError
      }

      Executor.execute(schema, query, root = new TestSubject, userContext = Ctx(), deferredResolver = new LightColorResolver).await should be  (
        Map(
          "data" → Map(
            "def" → Map("color" → "lightmagenta"),
            "defFut" → Map("color" → "lightred"))))
    }

    "resolve deferred values correctly in presence of errors" in {
      val Success(doc) = QueryParser.parse("""
        {
          defFail { color }
          defFutFail { color }
        }
        """)

      val schema = Schema(DataType)

      val exceptionHandler: Executor.ExceptionHandler = {
        case (m, e: IllegalStateException) ⇒ HandledException(e.getMessage)
      }

      Executor.execute(schema, doc,
        root = new TestSubject,
        userContext = Ctx(),
        deferredResolver = new LightColorResolver,
        exceptionHandler = exceptionHandler).await should be  (
          Map(
            "data" → Map(
              "defFail" → null,
              "defFutFail" → null),
            "errors" → List(
              Map(
                "message" → "error in resolver",
                "field" → "defFail",
                "locations" → List(Map("line" → 3, "column" → 11))),
              Map(
                "message" → "error in resolver",
                "field" → "defFutFail",
                "locations" → List(Map("line" → 4, "column" → 11))))))
    }
  }
}
