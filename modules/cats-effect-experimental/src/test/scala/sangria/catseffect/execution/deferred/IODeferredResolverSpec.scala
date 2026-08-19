package sangria.catseffect.execution.deferred

import java.util.concurrent.atomic.AtomicInteger

import cats.effect.IO
import cats.effect.unsafe.implicits.global
import io.circe.Json
import org.scalatest.matchers.must.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.ast
import sangria.catseffect.execution.IOExecutionScheme._
import sangria.catseffect.schema.AsyncValue._
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.execution.{DeferredWithInfo, ExceptionHandler, Executor, HandledException}
import sangria.macros._
import sangria.marshalling.circe._
import sangria.schema._

import scala.concurrent.Future

/** An [[IO]] counterpart of `sangria.execution.deferred.DeferredResolverSpec`, checking that
  * [[DeferredResolver]] batching still works when some of the fields are resolved via [[IO]]
  * instead of a plain [[scala.concurrent.Future]].
  */
class IODeferredResolverSpec extends AnyWordSpec with Matchers {
  import IODeferredResolverSpec._

  "DeferredResolver" must {
    "result in a single resolution of once level" in {
      val query =
        gql"""
          {
            root {
              name
              children(count: 5) {
                children(count: 5) {
                  children(count: 5) {
                    children(count: 5) {
                      children(count: 5) {
                        name
                      }
                    }

                    childrenFut(count: 2) {
                      children(count: 2) {
                        name
                      }
                    }

                    self {
                      children(count: 3) {
                        children(count: 3) {
                          name
                        }
                      }
                    }

                    selfFut {
                      children(count: 3) {
                        children(count: 3) {
                          name
                        }
                      }
                    }
                  }
                }
              }
            }
          }
        """

      val resolver = exec(query)

      resolver.callsCount.get must be(6)
      resolver.valueCount.get must be(2157)
    }

    "do not wait for async values" in {
      val query =
        gql"""
          {
            root {
              name

              children(count: 3) {
                s1: selfFutComplex {
                  children(count: 5) {
                    children(count: 5) {
                      name
                    }
                  }
                }

                s2: selfFutComplex {
                  children(count: 5) {
                    children(count: 5) {
                      name
                    }
                  }
                }

                selfFut {
                  children(count: 5) {
                    children(count: 5) {
                      name
                    }
                  }
                }

                selfFut {
                  children(count: 5) {
                    children(count: 5) {
                      name
                    }
                  }
                }
              }
            }
          }
        """

      val resolver = exec(query)

      resolver.callsCount.get must be(16)
      resolver.valueCount.get must be(56)
    }

    "group complex/expensive deferred values together" in {
      val query =
        gql"""
          {
            rootFut {
              name

              c1: childrenComplex(count: 5) {
                self {
                  childrenFut(count: 5) {
                    name
                  }
                }
              }

              c2: childrenComplex(count: 5) {
                self {
                  childrenFut(count: 5) {
                    name
                  }
                }
              }

              childrenFut(count: 5) {
                self {
                  childrenFut(count: 5) {
                    name
                  }
                }
              }
            }
          }
        """

      val resolver = exec(query)

      resolver.callsCount.get must be(5)
      resolver.valueCount.get must be(19)
    }

    "failed queries should be handled appropriately" in {
      val query =
        gql"""
          {
            fail1 {name}
            root {name}
            fail2 {name}
          }
        """

      val json = execJson(query)

      (json \\ "data").head must be(
        Json.obj(
          "fail1" -> Json.Null,
          "root" -> Json.obj("name" -> Json.fromString("Cat root")),
          "fail2" -> Json.Null))

      val errors = (json \\ "errors").head.asArray.get
      errors must have size 2
      errors.foreach { error =>
        error.hcursor.get[String]("message").toOption must be(Some("foo"))
      }
    }

    "failed mutations should be handled appropriately" in {
      val query =
        gql"""
          mutation {
            fail1 {name}
            root {name}
            fail2 {name}
          }
        """

      val json = execJson(query)

      (json \\ "data").head must be(
        Json.obj(
          "fail1" -> Json.Null,
          "root" -> Json.obj("name" -> Json.fromString("Cat root")),
          "fail2" -> Json.Null))

      val errors = (json \\ "errors").head.asArray.get
      errors must have size 2
      errors.foreach { error =>
        error.hcursor.get[String]("message").toOption must be(Some("foo"))
      }
    }
  }
}

object IODeferredResolverSpec {
  private case class LoadCategories(ids: Seq[String]) extends Deferred[Seq[String]]

  private class MyDeferredResolver extends DeferredResolver[Any] {
    val callsCount = new AtomicInteger(0)
    val valueCount = new AtomicInteger(0)

    override val includeDeferredFromField
        : Option[(Field[_, _], Vector[ast.Field], Args, Double) => Boolean] =
      Some((_, _, _, complexity) => complexity < 100)

    override def groupDeferred[T <: DeferredWithInfo](deferred: Vector[T]): Vector[Vector[T]] = {
      val (expensive, cheap) = deferred.partition(_.complexity > 100)
      Vector(expensive, cheap)
    }

    override def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit
        ec: scala.concurrent.ExecutionContext): Vector[Future[Seq[String]]] = {
      callsCount.getAndIncrement()
      valueCount.addAndGet(deferred.size)

      deferred.map {
        case LoadCategories(ids) if ids contains "fail" =>
          Future.failed(new IllegalStateException("foo"))
        case LoadCategories(ids) => Future.successful(ids)
      }
    }
  }

  private lazy val CategoryType: ObjectType[Unit, String] = ObjectType(
    "Category",
    () =>
      fields[Unit, String](
        Field("name", StringType, resolve = c => s"Cat ${c.value}"),
        Field("descr", StringType, resolve = c => s"Cat ${c.value} descr"),
        Field("self", CategoryType, resolve = c => c.value),
        Field("selfFut", CategoryType, resolve = c => IO(c.value)),
        Field(
          "selfFutComplex",
          CategoryType,
          complexity = Some((_, _, _) => 1000),
          resolve = c => IO(c.value)),
        Field(
          "children",
          ListType(CategoryType),
          arguments = Argument("count", IntType) :: Nil,
          resolve = c => LoadCategories((1 to c.arg[Int]("count")).map(i => s"${c.value}.$i"))
        ),
        Field(
          "childrenComplex",
          ListType(CategoryType),
          complexity = Some((_, _, _) => 1000),
          arguments = Argument("count", IntType) :: Nil,
          resolve = c => LoadCategories((1 to c.arg[Int]("count")).map(i => s"${c.value}.$i"))
        ),
        Field(
          "childrenFut",
          ListType(CategoryType),
          arguments = Argument("count", IntType) :: Nil,
          resolve = c =>
            DeferredFutureValue(Future.successful(LoadCategories((1 to c.arg[Int]("count")).map(i =>
              s"${c.value}.$i"))))
        )
      )
  )

  private val QueryType = ObjectType(
    "Query",
    fields[Unit, Unit](
      Field(
        "root",
        CategoryType,
        resolve = _ => DeferredValue(LoadCategories(Seq("root"))).map(_.head)),
      Field(
        "rootFut",
        CategoryType,
        resolve =
          _ => DeferredFutureValue(Future.successful(LoadCategories(Seq("root")))).map(_.head)),
      Field(
        "fail1",
        OptionType(CategoryType),
        resolve = _ => DeferredValue(LoadCategories(Seq("fail"))).map(_.head)),
      Field(
        "fail2",
        OptionType(CategoryType),
        resolve = _ => DeferredValue(LoadCategories(Seq("fail"))).map(_.head))
    )
  )

  private val MutationType = ObjectType(
    "Mutation",
    fields[Unit, Unit](
      Field(
        "root",
        OptionType(CategoryType),
        resolve = _ => DeferredValue(LoadCategories(Seq("root"))).map(_.head)),
      Field(
        "fail1",
        OptionType(CategoryType),
        resolve = _ => DeferredValue(LoadCategories(Seq("fail"))).map(_.head)),
      Field(
        "fail2",
        OptionType(CategoryType),
        resolve = _ => DeferredValue(LoadCategories(Seq("fail"))).map(_.head))
    )
  )

  private val schema = Schema(QueryType, Some(MutationType))

  private val exceptionHandler = ExceptionHandler { case (_, e) => HandledException(e.getMessage) }

  private def exec(query: ast.Document): MyDeferredResolver = {
    val resolver = new MyDeferredResolver
    val result: IO[Json] =
      Executor.execute(
        schema,
        query,
        deferredResolver = resolver,
        exceptionHandler = exceptionHandler)
    result.unsafeRunSync()
    resolver
  }

  private def execJson(query: ast.Document): Json = {
    val resolver = new MyDeferredResolver
    val result: IO[Json] =
      Executor.execute(
        schema,
        query,
        deferredResolver = resolver,
        exceptionHandler = exceptionHandler)
    result.unsafeRunSync()
  }
}
