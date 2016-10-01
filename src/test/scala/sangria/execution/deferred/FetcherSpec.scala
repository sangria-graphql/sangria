package sangria.execution.deferred

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.macros._
import sangria.schema._
import sangria.util.{DebugUtil, FutureResultSupport}

import scala.concurrent.{ExecutionContext, Future}

class FetcherSpec extends WordSpec with Matchers with FutureResultSupport {
  case class Category(id: String, name: String, children: Seq[String])

  object Category {
    implicit val hasId = HasId[Category, String](_.id)
  }

  class CategoryRepo {
    val categories = Vector(
      Category("1", "Root", Vector("2", "3", "4")),
      Category("2", "Cat 2", Vector("5", "6")),
      Category("3", "Cat 3", Vector("7")),
      Category("4", "Cat 4", Vector.empty),
      Category("5", "Cat 5", Vector.empty),
      Category("6", "Cat 6", Vector.empty),
      Category("7", "Cat 7", Vector.empty),
      Category("8", "Cat 8", Vector("4", "5", "foo!")))

    def loadBulk(ids: Seq[String]): Future[Seq[Category]] =
      Future.successful(ids.flatMap(id ⇒ categories.find(_.id == id)))
  }

  val categoryFetcher =
    Fetcher((repo: CategoryRepo, ids: Seq[String]) ⇒ repo.loadBulk(ids))

  val categoryFetcherCaching =
    Fetcher.caching((repo: CategoryRepo, ids: Seq[String]) ⇒ repo.loadBulk(ids))

  def properFetcher(implicit ec: ExecutionContext) = {
    lazy val CategoryType: ObjectType[CategoryRepo, Category] = ObjectType("Category", () ⇒ fields(
      Field("id", StringType, resolve = c ⇒ c.value.id),
      Field("name", StringType, resolve = c ⇒ c.value.name),
      Field("self", CategoryType, resolve = c ⇒ c.value),
      Field("selfFut", CategoryType, resolve = c ⇒ Future(c.value)),
      Field("childrenSeq", ListType(CategoryType),
        resolve = c ⇒ categoryFetcher.getSeq(c.value.children)),
      Field("childrenSeqOpt", ListType(CategoryType),
        resolve = c ⇒ categoryFetcher.getSeqOpt(c.value.children)),
      Field("childrenFut", ListType(CategoryType),
        resolve = c ⇒ DeferredFutureValue(Future.successful(
          categoryFetcher.getSeq(c.value.children))))))

    val QueryType = ObjectType("Query", fields[CategoryRepo, Unit](
      Field("category", OptionType(CategoryType),
        arguments = Argument("id", StringType) :: Nil,
        resolve = c ⇒ categoryFetcher.getOpt(c.arg[String]("id"))),
      Field("root", CategoryType, resolve = _ ⇒ categoryFetcher.get("1")),
      Field("rootFut", CategoryType, resolve = _ ⇒
        DeferredFutureValue(Future.successful(categoryFetcher.get("1"))))))

    val schema = Schema(QueryType)

    val resolver = DeferredResolver.fetchers(categoryFetcher)
    val resolverCaching = DeferredResolver.fetchers(categoryFetcherCaching)

    "result in a single resolution of once level" in {
      val query =
        graphql"""
          {
            c1: category(id: "non-existing") {name}
            #c2: category(id: "8") {name childrenSeq {id}}
            c3: category(id: "8") {name childrenSeqOpt {id}}

            root {
              id
              name
              childrenSeq {
                id
                name
                childrenSeq {
                  id
                  name
                  childrenSeq {
                    id
                    name
                    childrenSeq {
                      id
                      name
                    }
                  }
                }
              }
            }
          }
        """

      val res = Executor.execute(schema, query, new CategoryRepo, deferredResolver = resolver).await

      res should be (
        Map(
          "data" → Map(
            "c1" → null,
            "c3" → Map(
              "name" → "Cat 8",
              "childrenSeqOpt" → Vector(
                Map(
                  "id" → "4"),
                Map(
                  "id" → "5"))),
            "root" → Map(
              "id" → "1",
              "name" → "Root",
              "childrenSeq" → Vector(
                Map(
                  "id" → "2",
                  "name" → "Cat 2",
                  "childrenSeq" → Vector(
                    Map(
                      "id" → "5",
                      "name" → "Cat 5",
                      "childrenSeq" → Vector.empty),
                    Map(
                      "id" → "6",
                      "name" → "Cat 6",
                      "childrenSeq" → Vector.empty))),
                Map(
                  "id" → "3",
                  "name" → "Cat 3",
                  "childrenSeq" → Vector(
                    Map(
                      "id" → "7",
                      "name" → "Cat 7",
                      "childrenSeq" → Vector.empty))),
                Map(
                  "id" → "4",
                  "name" → "Cat 4",
                  "childrenSeq" → Vector.empty))))))
    }
  }

  "Fetcher" when {
    "using standard execution context" should {
      behave like properFetcher (ExecutionContext.Implicits.global)
    }

    "using sync execution context" should {
      behave like properFetcher (sync.executionContext)
    }
  }
}