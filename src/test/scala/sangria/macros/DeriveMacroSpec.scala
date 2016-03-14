package sangria.macros

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.{FieldTag, Executor}
import sangria.introspection._
import sangria.schema._
import sangria.starWars.TestData.Human
import sangria.starWars.TestSchema
import sangria.util.FutureResultSupport
import scala.concurrent.ExecutionContext.Implicits.global

class DeriveMacroSpec extends WordSpec with Matchers with FutureResultSupport {
  trait Parent1 {
    def id: String
  }

  trait Parent2 {
    def list: List[String]
  }

  case class TestSubject(id: String, list: List[String] = Nil, excluded: Int) extends Parent1 with Parent2

  case object CachedTag extends FieldTag
  case object AuthorizedTag extends FieldTag
  case object FooTag extends FieldTag

  case class Comment(author: String, text: Option[String])
  case class Article(title: String, text: Option[String], tags: Option[Vector[String]], comments: Option[Vector[Option[Comment]]])

  @GraphQLName("MyQueryType")
  @GraphQLDescription("My type!")
  case class TestSubjectAnnotated(
    @GraphQLDescription("my id")
    @GraphQLDeprecated("No IDs anymore!")
    id: String,

    @GraphQLName("myList")
    @GraphQLFieldTags(CachedTag, AuthorizedTag)
    list: List[String] = Nil,

    @GraphQLExclude
    excluded: Int)

  val Parent1Type = InterfaceType("Parent1", fields[Unit, Parent1](
    Field("id", StringType, resolve = _.value.id)))

  val Parent2Type = InterfaceType("Parent2", fields[Unit, Parent2](
    Field("list", ListType(StringType), resolve = _.value.list)))

  "ObjectType derivation" should {
    "use class name and have no description by default" in {
      val tpe = deriveObjectType[Unit, TestSubject]()

      tpe.name should be ("TestSubject")
      tpe.description should be (None)
    }

    "allow to change name and description with config" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        Name("Foo"),
        Description("my desc"))

      tpe.name should be ("Foo")
      tpe.description should be (Some("my desc"))
    }

    "allow to change name and description with annotations" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated]()

      tpe.name should be ("MyQueryType")
      tpe.description should be (Some("My type!"))
    }

    "prioritize config over annotation for name and description" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated](
        Name("Foo"),
        Description("my desc"))

      tpe.name should be ("Foo")
      tpe.description should be (Some("my desc"))
    }

    "allow to specify interfaces via config" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        Interfaces(Parent1Type, Parent2Type))

      tpe.interfaces.map(_.name).sorted should be ("Parent1" :: "Parent2" :: Nil)
    }

    "allow to specify interfaces via multiple config entries" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        Interfaces(Parent1Type),
        Interfaces(Parent2Type))

      tpe.interfaces.map(_.name).sorted should be ("Parent1" :: "Parent2" :: Nil)
    }

    "expose of case class fields" in {
      val tpe = deriveObjectType[Unit, TestSubject]()
      val fields = tpe.fields

      fields should have size 3

      fields(0).name should be ("id")
      fields(0).fieldType should be (StringType)

      fields(1).name should be ("list")
      fields(1).fieldType should be (ListType(StringType))

      fields(2).name should be ("excluded")
      fields(2).fieldType should be (IntType)
    }

    "validate known field names" in {
      """deriveObjectType[Unit, TestSubject](IncludeFields("id", "list1"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](ExcludeFields("id1"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](DocumentField("id1", "foo"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](RenameField("id1", "foo"))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](FieldTags("id1", CachedTag))""" shouldNot compile
      """deriveObjectType[Unit, TestSubject](DeprecateField("id1", "test"))""" shouldNot compile
    }

    "respect whitelist and blacklist provided via config" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        IncludeFields("id", "list"),
        ExcludeFields("list"))

      tpe.fields should have size 1

      tpe.fields(0).name should be ("id")
    }

    "respect blacklist provided via annotations" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated](
        IncludeFields("id", "list", "excluded"),
        ExcludeFields("id"))

      tpe.fields should have size 1
      tpe.fields(0).name should be ("myList")
    }

    "allow to add new fields" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        IncludeFields("id"),
        AddFields(
          Field("foo", ListType(StringType), resolve = _.value.list),
          Field("bar", BooleanType, resolve = _ ⇒ true)))

      tpe.fields should have size 3

      tpe.fields(0).name should be ("id")
      tpe.fields(0).fieldType should be (StringType)

      tpe.fields(1).name should be ("foo")
      tpe.fields(1).fieldType should be (ListType(StringType))

      tpe.fields(2).name should be ("bar")
      tpe.fields(2).fieldType should be (BooleanType)
    }

    "allow to set name, description, deprecationReason and fieldTags with config" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        DocumentField("id", "the object ID", deprecationReason = Some("foo")),
        RenameField("id", "identifier"),
        RenameField("list", "colors"),
        DocumentField("list", "my colors"),
        DeprecateField("excluded", "bar"),
        FieldTags("list", CachedTag, AuthorizedTag))

      tpe.fields should have size 3

      tpe.fields(0).name should be ("identifier")
      tpe.fields(0).description should be (Some("the object ID"))
      tpe.fields(0).deprecationReason should be (Some("foo"))
      tpe.fields(0).tags should be (Nil)
      tpe.fields(0).fieldType should be (StringType)

      tpe.fields(1).name should be ("colors")
      tpe.fields(1).description should be (Some("my colors"))
      tpe.fields(1).deprecationReason should be (None)
      tpe.fields(1).tags should be (List(CachedTag, AuthorizedTag))
      tpe.fields(1).fieldType should be (ListType(StringType))

      tpe.fields(2).name should be ("excluded")
      tpe.fields(2).description should be (None)
      tpe.fields(2).deprecationReason should be (Some("bar"))
      tpe.fields(2).tags should be (Nil)
      tpe.fields(2).fieldType should be (IntType)
    }

    "allow to set name, description, deprecationReason and fieldTags with annotations" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated]()

      tpe.fields should have size 2

      tpe.fields(0).name should be ("id")
      tpe.fields(0).description should be (Some("my id"))
      tpe.fields(0).deprecationReason should be (Some("No IDs anymore!"))
      tpe.fields(0).tags should be (Nil)
      tpe.fields(0).fieldType should be (StringType)

      tpe.fields(1).name should be ("myList")
      tpe.fields(1).description should be (None)
      tpe.fields(1).deprecationReason should be (None)
      tpe.fields(1).tags should be (List(CachedTag, AuthorizedTag))
      tpe.fields(1).fieldType should be (ListType(StringType))
    }

    "prioritize field config name, description, deprecationReason and merge fieldTags" in {
      val tpe = deriveObjectType[Unit, TestSubjectAnnotated](
        RenameField("list", "fooBar"),
        DocumentField("id", "new descr", Some("new depr")),
        FieldTags("id", FooTag),
        FieldTags("list", FooTag))

      tpe.fields should have size 2

      tpe.fields(0).name should be ("id")
      tpe.fields(0).description should be (Some("new descr"))
      tpe.fields(0).deprecationReason should be (Some("new depr"))
      tpe.fields(0).tags should be (List(FooTag))
      tpe.fields(0).fieldType should be (StringType)

      tpe.fields(1).name should be ("fooBar")
      tpe.fields(1).description should be (None)
      tpe.fields(1).deprecationReason should be (None)
      tpe.fields(1).tags should be (List(FooTag, CachedTag, AuthorizedTag))
      tpe.fields(1).fieldType should be (ListType(StringType))
    }

    "be able to find other types via implicit GraphQL types" in {
      implicit val CommentType = deriveObjectType[Unit, Comment](
        DocumentField("author", "The comment author"),
        DocumentField("text", "Comment text"))

      val ArticleType = deriveObjectType[Unit, Article](
        RenameField("tags", "myTags"))

      val testArticle = Article("My First Article", Some("foo bar"), None,
        Some(Vector(Some(Comment("bob", None)), None, Some(Comment("jane", Some("yay!"))))))

      val query =
        graphql"""
          {
            title
            text
            myTags
            comments {
              author
              text
            }
          }
        """

      val schema = Schema(ArticleType)

      Executor.execute(schema, query, root = testArticle).await should be (Map(
        "data" → Map(
          "title" → "My First Article",
          "text" → "foo bar",
          "myTags" → null,
          "comments" → List(
            Map("author" → "bob", "text" → null),
            null,
            Map("author" → "jane", "text" → "yay!"))))
      )

      import sangria.parser.DeliveryScheme.Throw
      import sangria.marshalling.queryAst._

      val intro = IntrospectionParser.parse(Executor.execute(schema, sangria.introspection.introspectionQuery, root = testArticle).await)

      intro.queryType.name should be ("Article")

      val Some(articleIntro: IntrospectionObjectType) = intro.types.find(_.name == "Article")
      val Some(commentIntro: IntrospectionObjectType) = intro.types.find(_.name == "Comment")

      commentIntro.fields should have size 2

      commentIntro.fields(0).name should be ("author")
      commentIntro.fields(0).description should be (Some("The comment author"))

      commentIntro.fields(1).name should be ("text")
      commentIntro.fields(1).description should be (Some("Comment text"))

      articleIntro.fields(3).name should be ("comments")
      articleIntro.fields(3).tpe should be (IntrospectionListTypeRef(IntrospectionNamedTypeRef(TypeKind.Object, "Comment")))
    }
  }
}
