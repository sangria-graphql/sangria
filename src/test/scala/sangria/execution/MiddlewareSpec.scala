package sangria.execution

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.Document
import sangria.integration.ResultMarshaller
import sangria.macros._
import sangria.schema._
import sangria.util.AwaitSupport

import scala.collection.mutable.{Map => MutableMap}
import scala.concurrent.Future

import scala.concurrent.ExecutionContext.Implicits.global

class MiddlewareSpec extends WordSpec with Matchers with AwaitSupport {
  class QueryMiddleware extends Middleware {
    type QueryVal = String

    def userCtx(c: MiddlewareQueryContext[_, _]) = c.executor.userContext.asInstanceOf[Count]

    def beforeQuery(context: MiddlewareQueryContext[_, _]) = {
      userCtx(context).count.incrementAndGet()
      "Context stuff"
    }

    def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[_, _]) = {
      userCtx(context).count.incrementAndGet()
      userCtx(context).context = Some(queryVal)
    }
  }

  case object Cacheed extends FieldTag

  class CachingMiddleware extends Middleware with MiddlewareAfterField {
    type QueryVal = MutableMap[String, Action[_, _]]
    type FieldVal = Boolean

    def beforeQuery(context: MiddlewareQueryContext[_, _]) =
      MutableMap()

    def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[_, _]) = ()

    def cacheKey(ctx: Context[_, _]) = ctx.parentType.name + "." + ctx.field.name

    val noCache = (false, None)

    def beforeField(cache: QueryVal, mctx: MiddlewareQueryContext[_, _], ctx: Context[_, _]) = {
      val key = cacheKey(ctx)

      if (ctx.field.tags.contains(Cacheed))
        cache.contains(key) -> cache.get(cacheKey(ctx))
      else
        noCache
    }

    def afterField(cache: QueryVal, fromCache: FieldVal, value: Any, mctx: MiddlewareQueryContext[_, _], ctx: Context[_, _]) = {
      if (ctx.field.tags.contains(Cacheed) && !fromCache)
        cache += cacheKey(ctx) -> Value(value)

      None
    }
  }

  class Count {
    val count = new AtomicInteger(0)
    var context: Option[String] = None
    var metrics: MutableMap[String, List[Long]] = MutableMap()
  }

  class FieldMetrics extends Middleware with MiddlewareAfterField with MiddlewareErrorField {
    type QueryVal = MutableMap[String, List[Long]]
    type FieldVal = Long

    def beforeQuery(context: MiddlewareQueryContext[_, _]) =
      MutableMap()

    def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[_, _]) = {
      context.executor.userContext.asInstanceOf[Count].metrics = queryVal
    }

    def beforeField(queryVal: QueryVal, mctx: MiddlewareQueryContext[_, _], ctx: Context[_, _]) = {
      if (ctx.field.name == "errorInBefore") throw new IllegalStateException("oops!")

      continue(System.currentTimeMillis())
    }

    def afterField(queryVal: QueryVal, fieldVal: FieldVal, value: Any, mctx: MiddlewareQueryContext[_, _], ctx: Context[_, _]) = {
      if (ctx.field.name == "errorInAfter") throw new IllegalStateException("oops!")

      queryVal.synchronized {
        val key = ctx.parentType.name + "." + ctx.field.name
        val list = queryVal.getOrElse(key, Nil)

        queryVal.update(key, list :+ (System.currentTimeMillis() - fieldVal))

        if (value == "nothing special") Some("something very special!") else None
      }
    }

    def fieldError(queryVal: QueryVal, fieldVal: FieldVal, error: Throwable, mctx: MiddlewareQueryContext[_, _], ctx: Context[_, _]) =
      queryVal.synchronized {
        val key = ctx.parentType.name + "." + ctx.field.name
        val list = queryVal.getOrElse(key, Nil)
        val errors = queryVal.getOrElse("errors", Nil)

        queryVal.update(key, list :+ (System.currentTimeMillis() - fieldVal))
        queryVal.update("errors", errors :+ 1L)
      }
  }

  val TestObject: ObjectType[Count, Unit] = ObjectType("Test", () => fields[Count, Unit](
    Field("error", OptionType(StringType), resolve = _ => throw new IllegalStateException("boom")),
    Field("futureError", OptionType(StringType), resolve = _ => Future.failed[Option[String]](new IllegalStateException("boom"))),
    Field("defError", OptionType(StringType), resolve = _ => Fail),
    Field("someString", StringType, resolve = _ => "nothing special"),
    Field("errorInAfter", OptionType(StringType), resolve = _ => "everything ok here"),
    Field("errorInBefore", OptionType(StringType), resolve = _ => "everything ok here"),
    Field("anotherString", StringType, resolve = _ => "foo"),
    Field("cachedId", IntType, tags = Cacheed :: Nil, resolve = _.ctx.count.incrementAndGet()),
    Field("delay30", StringType, resolve = _ => Future {
      Thread.sleep(30)
      "slept for 30ms"
    }),
    Field("nested", TestObject, resolve = _ => ())
  ))

  case object Fail extends Deferred[String]

  class BrokenResolver extends DeferredResolver[Any] {
    def resolve(deferred: List[Deferred[Any]], ctx: Any) = deferred map {
      case Fail => Future.failed(new IllegalStateException("error in resolver"))
    }
  }

  val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
    case (m, e: IllegalStateException) => HandledException(e.getMessage)
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
        "cachedId" -> 1,
        "foo" -> 1,
        "someString" -> "nothing special",
        "nested" -> Map(
          "cachedId" -> 1,
          "nested" -> Map(
            "cachedId" -> 1)),
        "foo" -> Map(
          "cachedId" -> 1,
          "nested" -> Map(
            "cachedId" -> 1))))

      ctx.count.get() should be (1)
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
        "anotherString" -> "foo",
        "a" -> "something very special!",
        "someString" -> "something very special!",
        "error" -> null,
        "errorInAfter" -> null,
        "errorInBefore" -> null,
        "nested" -> Map(
          "someString" -> "something very special!",
          "delay30" -> "slept for 30ms",
          "error" -> null,
          "futureError" -> null,
          "defError" -> null,
          "nested" -> Map(
            "error" -> null,
            "defError" -> null))))

      ctx.metrics.mapValues(_.size) should be (Map(
        "errors" -> 7,
        "Test.delay30" -> 1,
        "Test.nested" -> 2,
        "Test.someString" -> 3,
        "Test.anotherString" -> 1,
        "Test.error" -> 3,
        "Test.futureError" -> 1,
        "Test.defError" -> 2,
        "Test.errorInAfter" -> 1
      ))
    }
  }
}
