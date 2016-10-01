package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.parser.QueryParser
import sangria.schema._
import sangria.util.FutureResultSupport
import sangria.validation.QueryValidator
import sangria.macros._

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success
import scala.concurrent.ExecutionContext.Implicits.global

class ExecutorSchemaSpec extends WordSpec with Matchers with FutureResultSupport {
  case class Image(url: Option[String], width: Option[Int], height: Option[Int])

  case class Author(id: Option[String], name: Option[String], recentArticle: Option[String]) {
    def pic(width: Int, height: Int): Option[Image] = Some(Image(Some("cdn://123"), Some(width), Some(height)))
  }

  case class Article(
    id: String,
    isPublished: Option[Boolean],
    author: Option[Author],
    title: Option[String],
    body: Option[String],
    keywords: List[Option[String]])

  case class ArticleDeferred(id: String) extends Deferred[Option[Article]]

  val BlogImageType = ObjectType("Image", fields[Unit, Image](
    Field("url", OptionType(StringType), resolve = _.value.url),
    Field("width", OptionType(IntType), resolve = _.value.width),
    Field("height", OptionType(IntType), resolve = _.value.height)))

  val BlogAuthorType = ObjectType("Author", () ⇒ fields[Unit, Author](
    Field("id", OptionType(StringType), resolve = _.value.id),
    Field("name", OptionType(StringType), resolve = _.value.name),
    Field("pic", OptionType(BlogImageType),
      arguments = Argument("width", OptionInputType(IntType)) :: Argument("height", OptionInputType(IntType)) :: Nil,
      resolve = ctx ⇒ for {w ← ctx.argOpt[Int]("width"); h ← ctx.argOpt[Int]("height"); pic ← ctx.value.pic(w, h)} yield pic),
    Field("recentArticle", OptionType(BlogArticleType),
      resolve = ctx ⇒ ctx.value.recentArticle map (ra ⇒ DeferredValue(ArticleDeferred(ra))) getOrElse Value(None))))

  val BlogArticleType: ObjectType[Unit, Article] = ObjectType("Article", fields[Unit, Article](
    Field("id", StringType, resolve = _.value.id),
    Field("isPublished", OptionType(BooleanType), resolve = _.value.isPublished),
    Field("author", OptionType(BlogAuthorType), resolve = _.value.author),
    Field("title", OptionType(StringType), resolve = _.value.title),
    Field("body", OptionType(StringType), resolve = _.value.body),
    Field("keywords", OptionType(ListType(OptionType(StringType))), resolve = _.value.keywords)))

  val BlogQueryType = ObjectType("Query", fields[Unit, Unit](
    Field("article", OptionType(BlogArticleType),
      arguments = Argument("id", OptionInputType(IDType)) :: Nil,
      resolve = ctx ⇒ ctx.argOpt[String]("id") flatMap (id ⇒ article(id.toInt))),
    Field("feed", OptionType(ListType(OptionType(BlogArticleType))),
      resolve = _ ⇒ (1 to 10).toList.map(article))))

  val BlogSubscriptionType = ObjectType("Subscription", fields[Unit, Unit](
    Field("articleSubscribe", OptionType(BlogArticleType),
      arguments = Argument("id", OptionInputType(IDType)) :: Nil,
      resolve = ctx ⇒ ctx.argOpt[String]("id") flatMap (id ⇒ article(id.toInt)))))

  val BlogSchema = Schema(BlogQueryType, subscription = Some(BlogSubscriptionType))

  val JohnSmith = Author(id = Some("123"), name = Some("John Smith"), recentArticle = Some("1"))

  def article(id: Int): Option[Article] = Some(Article(
    id = s"$id",
    isPublished = Some(true),
    author = Some(JohnSmith),
    title = Some(s"My Article $id"),
    body = Some("This is a post"),
    keywords = List(Some("foo"), Some("bar"), None, Some("1"))))

  "Execute: Handles execution with a complex schema" should {
    "execute using a query type" in {
      val Success(doc) = QueryParser.parse("""
        {
          feed {
            id,
            title
          },
          article(id: "1") {
            ...articleFields,
            author {
              id,
              name,
              pic(width: 640, height: 480) {
                url,
                width,
                height
              },
              recentArticle {
                ...articleFields,
                keywords
              }
            }
          }
        }

        fragment articleFields on Article {
          id,
          isPublished,
          title,
          body,
          hidden,
          notdefined
        }
      """)

      val expected = Map(
        "data" → Map(
          "feed" → List(
            Map("id" → "1", "title" → "My Article 1"),
            Map("id" → "2", "title" → "My Article 2"),
            Map("id" → "3", "title" → "My Article 3"),
            Map("id" → "4", "title" → "My Article 4"),
            Map("id" → "5", "title" → "My Article 5"),
            Map("id" → "6", "title" → "My Article 6"),
            Map("id" → "7", "title" → "My Article 7"),
            Map("id" → "8", "title" → "My Article 8"),
            Map("id" → "9", "title" → "My Article 9"),
            Map("id" → "10", "title" → "My Article 10")),
          "article" → Map(
            "id" → "1",
            "isPublished" → true,
            "title" → "My Article 1",
            "body" → "This is a post",
            "author" → Map(
              "id" → "123",
              "name" → "John Smith",
              "pic" → Map(
                "url" → "cdn://123",
                "width" → 640,
                "height" → 480
              ),
              "recentArticle" → Map(
                "id" → "1",
                "isPublished" → true,
                "title" → "My Article 1",
                "body" → "This is a post",
                "keywords" → List("foo", "bar", null, "1")
              )
            )
          )
        )
      )

      val resolver = new DeferredResolver[Any] {
        def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) = deferred map {
          case ArticleDeferred(id) ⇒ Future.successful(article(id.toInt))
        }
      }

      Executor.execute(BlogSchema, doc, deferredResolver = resolver, queryValidator = QueryValidator.empty).await should be (expected)
    }

    "execute using subscription type" in {
      val query = graphql"""
       subscription NewArticles {
         articleSubscribe(id: "1") {
           ...articleFields,
           author {
             id,
             name,
             pic(width: 640, height: 480) {
               url,
               width,
               height
             },
             recentArticle {
               ...articleFields,
               keywords
             }
           }
         }
       }

       fragment articleFields on Article {
         id,
         isPublished,
         title,
         body,
         hidden,
         notdefined
       }
      """

      val resolver = new DeferredResolver[Any] {
        def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) = deferred map {
          case ArticleDeferred(id) ⇒ Future.successful(article(id.toInt))
        }
      }

      Executor.execute(BlogSchema, query, deferredResolver = resolver, queryValidator = QueryValidator.empty).await should be (Map(
        "data" → Map(
          "articleSubscribe" → Map(
            "id" → "1",
            "isPublished" → true,
            "title" → "My Article 1",
            "body" → "This is a post",
            "author" → Map(
              "id" → "123",
              "name" → "John Smith",
              "pic" → Map(
                "url" → "cdn://123",
                "width" → 640,
                "height" → 480
              ),
              "recentArticle" → Map(
                "id" → "1",
                "isPublished" → true,
                "title" → "My Article 1",
                "body" → "This is a post",
                "keywords" → List("foo", "bar", null, "1")
              )
            )
          )
        )
      ))
    }
  }
}