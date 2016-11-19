package sangria.execution

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.Document
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.macros._
import sangria.marshalling.ResultMarshaller
import sangria.schema._
import sangria.util.{DebugUtil, FutureResultSupport}

import scala.collection.concurrent.TrieMap
import scala.collection.mutable.ArrayBuffer
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Failure, Success}

class MiddlewareSpec extends WordSpec with Matchers with FutureResultSupport {
  class QueryMiddleware extends Middleware[Count] {
    type QueryVal = String

    def beforeQuery(context: MiddlewareQueryContext[Count, _, _]) = {
      context.ctx.count.incrementAndGet()
      "Context stuff"
    }

    def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Count, _, _]) = {
      context.ctx.count.incrementAndGet()
      context.ctx.context = Some(queryVal)
    }
  }

  case object Cached extends FieldTag

  class CachingMiddleware extends Middleware[Any] with MiddlewareAfterField[Any] {
    type QueryVal = TrieMap[String, Action[Any, _]]
    type FieldVal = Boolean

    def beforeQuery(context: MiddlewareQueryContext[Any, _, _]) =
      TrieMap()

    def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Any, _, _]) = ()

    def cacheKey(ctx: Context[Any, _]) = ctx.parentType.name + "." + ctx.field.name

    val noCache = (false, None)

    def beforeField(cache: QueryVal, mctx: MiddlewareQueryContext[Any, _, _], ctx: Context[Any, _]) = {
      val key = cacheKey(ctx)

      if (ctx.field.tags.contains(Cached))
        cache.contains(key) → cache.get(cacheKey(ctx))
      else
        noCache
    }

    def afterField(cache: QueryVal, fromCache: FieldVal, value: Any, mctx: MiddlewareQueryContext[Any, _, _], ctx: Context[Any, _]) = {
      if (ctx.field.tags.contains(Cached) && !fromCache)
        cache += cacheKey(ctx) → Value(value)

      None
    }
  }

  class Count {
    val count = new AtomicInteger(0)
    var context: Option[String] = None
    var metrics: TrieMap[String, List[Long]] = TrieMap()
  }

  class FieldMetrics extends Middleware[Count] with MiddlewareAfterField[Count] with MiddlewareErrorField[Count] {
    type QueryVal = TrieMap[String, List[Long]]
    type FieldVal = Long

    def beforeQuery(context: MiddlewareQueryContext[Count, _, _]) =
      TrieMap()

    def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Count, _, _]) = {
      context.ctx.metrics = queryVal
    }

    def beforeField(queryVal: QueryVal, mctx: MiddlewareQueryContext[Count, _, _], ctx: Context[Count, _]) = {
      if (ctx.field.name == "errorInBefore") throw new IllegalStateException("oops!")

      continue(System.currentTimeMillis())
    }

    def afterField(queryVal: QueryVal, fieldVal: FieldVal, value: Any, mctx: MiddlewareQueryContext[Count, _, _], ctx: Context[Count, _]) = {
      if (ctx.field.name == "errorInAfter") throw new IllegalStateException("oops!")

      queryVal.synchronized {
        val key = ctx.parentType.name + "." + ctx.field.name
        val list = queryVal.getOrElse(key, Nil)

        queryVal.update(key, list :+ (System.currentTimeMillis() - fieldVal))

        if (value == "nothing special") Some("something very special!") else None
      }
    }

    def fieldError(queryVal: QueryVal, fieldVal: FieldVal, error: Throwable, mctx: MiddlewareQueryContext[Count, _, _], ctx: Context[Count, _]) =
      queryVal.synchronized {
        val key = ctx.parentType.name + "." + ctx.field.name
        val list = queryVal.getOrElse(key, Nil)
        val errors = queryVal.getOrElse("errors", Nil)

        queryVal.update(key, list :+ (System.currentTimeMillis() - fieldVal))
        queryVal.update("errors", errors :+ 1L)
      }
  }

  val TestObject: ObjectType[Count, Unit] = ObjectType("Test", () ⇒ fields[Count, Unit](
    Field("error", OptionType(StringType), resolve = _ ⇒ throw new IllegalStateException("boom")),
    Field("futureError", OptionType(StringType), resolve = _ ⇒ Future.failed[Option[String]](new IllegalStateException("boom"))),
    Field("defError", OptionType(StringType), resolve = _ ⇒ Fail),
    Field("someString", StringType, resolve = _ ⇒ "nothing special"),
    Field("errorInAfter", OptionType(StringType), resolve = _ ⇒ "everything ok here"),
    Field("errorInBefore", OptionType(StringType), resolve = _ ⇒ "everything ok here"),
    Field("anotherString", StringType, resolve = _ ⇒ "foo"),
    Field("cachedId", IntType, tags = Cached :: Nil, resolve = _.ctx.count.incrementAndGet()),
    Field("delay30", StringType, resolve = _ ⇒ Future {
      Thread.sleep(30)
      "slept for 30ms"
    }),
    Field("nested", TestObject, resolve = _ ⇒ ())
  ))

  case object Fail extends Deferred[String]

  class BrokenResolver extends DeferredResolver[Any] {
    def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) = deferred map {
      case Fail ⇒ Future.failed(new IllegalStateException("error in resolver"))
    }
  }

  val exceptionHandler: Executor.ExceptionHandler = {
    case (m, e: IllegalStateException) ⇒ HandledException(e.getMessage)
  }

  val schema = Schema(TestObject, Some(TestObject))

  "Query" should {
    "support before and after at query level " in {
      val query = graphql"{someString, delay30}"
      val ctx = new Count

      Executor.execute(schema, query, userContext = ctx, middleware = new QueryMiddleware :: Nil).await

      ctx.count.get() should be (2)
      ctx.context should be (Some("Context stuff"))
    }
    
    "allow to prevent resolve call from `beforeField`" in {
      val query =
        graphql"""
          {
            cachedId
            someString
            nested {
              cachedId
              nested {
                cachedId
              }
            }
            foo: nested {
              cachedId
              nested {
                cachedId
              }
            }
          }
        """

      val ctx = new Count

      val res = Executor.execute(schema, query, userContext = ctx, middleware = new CachingMiddleware :: Nil).await

      res.asInstanceOf[Map[String, Any]]("data") should be (Map(
        "cachedId" → 1,
        "foo" → 1,
        "someString" → "nothing special",
        "nested" → Map(
          "cachedId" → 1,
          "nested" → Map(
            "cachedId" → 1)),
        "foo" → Map(
          "cachedId" → 1,
          "nested" → Map(
            "cachedId" → 1))))

      ctx.count.get() should be (1)
    }

    "field middleware is called for all possible actions" in {
      def error(message: String) = new IllegalStateException(message)

      case object ED extends Deferred[Option[String]]
      case object SD extends Deferred[Option[String]]

      class Resolver extends DeferredResolver[Any] {
        def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) = deferred map {
          case ED ⇒ Future.failed(error("deferred error"))
          case SD ⇒ Future.successful(Some("deferred success"))
        }
      }

      val TestObject = ObjectType("Test", () ⇒ fields[Unit, Unit](
        Field("e1", OptionType(StringType), resolve = _ ⇒ Value(throw error("e1 error"))),
        Field("e2", OptionType(StringType), resolve = _ ⇒ TryValue(Failure(error("e2 error")))),
        Field("e3", OptionType(StringType), resolve = _ ⇒ FutureValue(Future.failed(error("e3 error")))),
        Field("e4", OptionType(StringType), resolve = _ ⇒ DeferredValue(ED)),
        Field("e5", OptionType(StringType), resolve = _ ⇒ DeferredFutureValue(Future.successful(ED))),
        Field("e6", OptionType(StringType), resolve = _ ⇒ DeferredFutureValue(Future.failed(error("e6 error")))),
        Field("e7", OptionType(StringType), resolve = _ ⇒ PartialValue(Some("e7 success"), Vector(error("e71 error"), error("e72 error")))),
        Field("e8", OptionType(StringType), resolve = _ ⇒ PartialFutureValue(Future.successful(PartialValue[Unit, Option[String]](Some("e8 success"), Vector(error("e81 error"), error("e82 error")))))),
        Field("e9", OptionType(StringType), resolve = _ ⇒ PartialFutureValue(Future.failed(error("e9")))),

        Field("s1", OptionType(StringType), resolve = _ ⇒ Value(Some("s1 success"))),
        Field("s2", OptionType(StringType), resolve = _ ⇒ TryValue(Success(Some("s2 success")))),
        Field("s3", OptionType(StringType), resolve = _ ⇒ FutureValue(Future.successful(Some("s3 success")))),
        Field("s4", OptionType(StringType), resolve = _ ⇒ DeferredValue(SD)),
        Field("s5", OptionType(StringType), resolve = _ ⇒ DeferredFutureValue(Future.successful(SD)))
      ))

      val schema = Schema(TestObject)

      case class Capture(before: ArrayBuffer[String], after: TrieMap[String, Set[String]], error: TrieMap[String, Set[String]])

      class ErrorCapturingMiddleware extends Middleware[Any] with MiddlewareAfterField[Any] with MiddlewareErrorField[Any] {
        type QueryVal = Capture
        type FieldVal = Unit

        def beforeQuery(context: MiddlewareQueryContext[Any, _, _]) =
          Capture(ArrayBuffer.empty, TrieMap(), TrieMap())

        def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Any, _, _]) = ()

        def beforeField(queryVal: QueryVal, mctx: MiddlewareQueryContext[Any, _, _], ctx: Context[Any, _]) = {
          queryVal.before += ctx.field.name
          continue
        }

        def afterField(queryVal: QueryVal, fieldVal: Unit, value: Any, mctx: MiddlewareQueryContext[Any, _, _], ctx: Context[Any, _]) = {
          val v = queryVal.after.getOrElseUpdate(ctx.field.name, Set.empty)

          queryVal.after(ctx.field.name) = v + value.asInstanceOf[Option[String]].get
          None
        }

        def fieldError(queryVal: QueryVal, fieldVal: Unit, error: Throwable, mctx: MiddlewareQueryContext[Any, _, _], ctx: Context[Any, _]) = {
          val v = queryVal.error.getOrElseUpdate(ctx.field.name, Set.empty)

          queryVal.error(ctx.field.name) = v + error.getMessage
        }
      }

      val query = graphql"{e1, e2, e3, e4, e5, e6, e7, e8, e9, s1, s2, s3, s4, s5}"

      import sangria.execution.ExecutionScheme.Extended

      val res = Executor.execute(schema, query,
        middleware = new ErrorCapturingMiddleware :: Nil,
        deferredResolver = new Resolver,
        exceptionHandler = exceptionHandler).await

      res.result.asInstanceOf[Map[String, Any]]("data") should be (Map(
        "e1" → null,
        "e2" → null,
        "e3" → null,
        "e4" → null,
        "e5" → null,
        "e6" → null,
        "e7" → "e7 success",
        "e8" → "e8 success",
        "e9" → null,
        "s1" → "s1 success",
        "s2" → "s2 success",
        "s3" → "s3 success",
        "s4" → "deferred success",
        "s5" → "deferred success"))

      val capture = res.middlewareVals.head._1.asInstanceOf[Capture]

      capture.before.toSet should be (Set(
        "e1", "e2", "e3", "e4", "e5", "e6", "e7", "e8", "e9", "s1", "s2", "s3", "s4", "s5"))

      capture.after should be (TrieMap(
        "e7" → Set("e7 success"),
        "e8" → Set("e8 success"),
        "s1" → Set("s1 success"),
        "s2" → Set("s2 success"),
        "s3" → Set("s3 success"),
        "s4" → Set("deferred success"),
        "s5" → Set("deferred success")))

      capture.error should be (TrieMap(
        "e1" → Set("e1 error"),
        "e2" → Set("e2 error"),
        "e3" → Set("e3 error"),
        "e4" → Set("deferred error"),
        "e5" → Set("deferred error"),
        "e6" → Set("e6 error"),
        "e7" → Set("e71 error", "e72 error"),
        "e8" → Set("e81 error", "e82 error"),
        "e9" → Set("e9")))
    }

    behave like properFieldLevelMiddleware(
      graphql"""
        {
          someString
          anotherString
          a: someString
          errorInBefore
          error
          nested {
            futureError
            someString
            defError
            delay30
            error

            nested {error defError}
          }
          errorInAfter
        }
      """)
  }

  "Mutation" should {
    behave like properFieldLevelMiddleware(
      graphql"""
        mutation Foo {
          someString
          anotherString
          a: someString
          errorInBefore
          error
          nested {
            futureError
            someString
            defError
            delay30
            error

            nested {error defError}
          }
          errorInAfter
        }
      """)
  }

  def properFieldLevelMiddleware(query: Document) = {
    "should support before and after at field level " in {
      val ctx = new Count

      val res = Executor.execute(schema, query,
        userContext = ctx,
        middleware = new FieldMetrics :: Nil,
        deferredResolver = new BrokenResolver,
        exceptionHandler = exceptionHandler).await

      res.asInstanceOf[Map[String, Any]]("data") should be (Map(
        "anotherString" → "foo",
        "a" → "something very special!",
        "someString" → "something very special!",
        "error" → null,
        "errorInAfter" → null,
        "errorInBefore" → null,
        "nested" → Map(
          "someString" → "something very special!",
          "delay30" → "slept for 30ms",
          "error" → null,
          "futureError" → null,
          "defError" → null,
          "nested" → Map(
            "error" → null,
            "defError" → null))))

      ctx.metrics.mapValues(_.size) should be (Map(
        "errors" → 7,
        "Test.delay30" → 1,
        "Test.nested" → 2,
        "Test.someString" → 3,
        "Test.anotherString" → 1,
        "Test.error" → 3,
        "Test.futureError" → 1,
        "Test.defError" → 2,
        "Test.errorInAfter" → 1
      ))
    }
  }
}
