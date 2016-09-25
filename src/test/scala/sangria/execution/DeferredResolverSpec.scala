package sangria.execution

import java.util.concurrent.atomic.AtomicInteger

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.Document
import sangria.schema._
import sangria.macros._
import sangria.util.FutureResultSupport

import scala.concurrent.{ExecutionContext, Future}

class DeferredResolverSpec extends WordSpec with Matchers with FutureResultSupport {
  def deferredResolver(implicit ec: ExecutionContext) = {
    case class LoadCategories(ids: Seq[String]) extends Deferred[Seq[String]]

    lazy val CategoryType: ObjectType[Unit, String] = ObjectType("Category", () ⇒ fields[Unit, String](
      Field("name", StringType, resolve = c ⇒ s"Cat ${c.value}"),
      Field("descr", StringType, resolve = c ⇒ s"Cat ${c.value} descr"),
      Field("self", CategoryType, resolve = c ⇒ c.value),
      Field("selfFut", CategoryType, resolve = c ⇒ Future(c.value)),
      Field("children", ListType(CategoryType),
        arguments = Argument("count", IntType) :: Nil,
        resolve = c ⇒ LoadCategories((1 to c.arg[Int]("count")).map(i ⇒ s"${c.value}.$i"))),
      Field("childrenFut", ListType(CategoryType),
        arguments = Argument("count", IntType) :: Nil,
        resolve = c ⇒ DeferredFutureValue(Future.successful(
          LoadCategories((1 to c.arg[Int]("count")).map(i ⇒ s"${c.value}.$i")))))
    ))

    val QueryType = ObjectType("Query", fields[Unit, Unit](
      Field("root", CategoryType, resolve = _ ⇒ DeferredValue(LoadCategories(Seq("root"))).map(_.head)),
      Field("rootFut", CategoryType, resolve = _ ⇒ DeferredFutureValue(Future.successful(LoadCategories(Seq("root")))).map(_.head))
    ))

    class MyDeferredResolver extends DeferredResolver[Any] {
      val count = new AtomicInteger(0)

      def resolve(deferred: Vector[Deferred[Any]], ctx: Any) = {
        count.getAndIncrement()

        deferred.map {
          case LoadCategories(ids) ⇒ Future.successful(ids)
        }
      }
    }

    val schema = Schema(QueryType)

    def exec(query: Document) = {
      val resolver = new MyDeferredResolver
      val result = Executor.execute(schema, query, deferredResolver = resolver).await


      resolver → result
    }

    "foo" in {
      val query =
        graphql"""
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

      val (resolver, _) = exec(query)

      resolver.count.get should be (6)
    }
  }

  "DeferredResolver" when {
    "using standard execution context" should {
      behave like deferredResolver (ExecutionContext.Implicits.global)
    }

    "using sync execution context" should {
      behave like deferredResolver (sync.executionContext)
    }
  }
}