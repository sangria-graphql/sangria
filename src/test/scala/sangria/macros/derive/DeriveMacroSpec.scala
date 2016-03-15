package sangria.macros.derive

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.{Executor, FieldTag}
import sangria.introspection._
import sangria.marshalling.FromInput.CoercedScalaResult
import sangria.schema._
import sangria.macros._
import sangria.util.FutureResultSupport
import sangria.util.tag.@@

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

  case class Comment(author: String, text: Option[String], color: ColorAnnotated.Value = ColorAnnotated.Red)
  case class Article(title: String, text: Option[String], tags: Option[Vector[String]], comments: Option[Vector[Option[Comment]]], fruit: FruitAnnotated = RedAppleAnnotated)

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

  sealed trait Fruit

  case object RedApple extends Fruit
  case object SuperBanana extends Fruit
  case object MegaOrange extends Fruit

  sealed abstract class ExoticFruit(val score: Int) extends Fruit

  case object Guave extends ExoticFruit(123)

  object Color extends Enumeration {
    val Red, LightGreen, DarkBlue = Value
  }

  @GraphQLName("MyFruit")
  @GraphQLDescription("Very tasty fruit")
  sealed trait FruitAnnotated

  @GraphQLName("JustApple")
  @GraphQLDescription("The red one")
  case object RedAppleAnnotated extends FruitAnnotated

  @GraphQLExclude
  case object SuperBananaAnnotated extends FruitAnnotated

  @GraphQLDeprecated("Not tasty anymore")
  case object MegaOrangeAnnotated extends FruitAnnotated

  @GraphQLName("MyColor")
  @GraphQLDescription("Very nice color")
  object ColorAnnotated extends Enumeration {
    @GraphQLName("NormalRed")
    @GraphQLDescription("The red one")
    val Red = Value

    @GraphQLExclude
    val LightGreen = Value

    @GraphQLDeprecated("Don't like blue")
    val DarkBlue = Value

    val Dark1Blue_FooStuff = Value
  }

  "ObjectType derivation" should {
    "foo" in {
      implicit val c = deriveEnumType[Color.Value]()

      class FooBar {
        @GraphQLField
        @GraphQLName("foo")
        def hello(id: Int, songs: Seq[String])(colors: Seq[Color.Value]) = s"id = $id, songs = ${songs mkString ","}, cc = ${colors mkString ","}"
      }

      val tpe = deriveObjectType[Unit, FooBar](IncludeMethods("hello"))

      val schema = Schema(tpe)

//      Executor.execute(schema, graphql"""{foo(id: 2, songs: ["a", "b"], colors: [Red, LightGreen])}""", root = new FooBar).await should be (Map(
//        "data" → Map("hello" → "4")))

      // TODO: name and description tags, complexity config, default argument values, Context argument → then input type derivation should be easy as well
    }

    "use class name and have no description by default" in {
      val tpe = deriveObjectType[Unit, TestSubject]()

      tpe.name should be ("TestSubject")
      tpe.description should be (None)
    }

    "allow to change name and description with config" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        ObjectTypeName("Foo"),
        ObjectTypeDescription("my desc"))

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
        ObjectTypeName("Foo"),
        ObjectTypeDescription("my desc"))

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

    "expose case class fields" in {
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
      """deriveObjectType[Unit, TestSubject](FieldComplexity("id1", (_, _, _) ⇒ 1.0))""" shouldNot compile
      """deriveObjectType[Unit, TestSubjectAnnotated](ExcludeFields("id", "list", "excluded"))""" shouldNot compile
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

    "allow to override fields" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        OverrideField("id", Field("id", ListType(StringType), resolve = _.value.list)),
        OverrideField("list", Field("bar", BooleanType, resolve = _ ⇒ true)))

      tpe.fields should have size 3

      tpe.fields(0).name should be ("excluded")
      tpe.fields(0).fieldType should be (IntType)

      tpe.fields(1).name should be ("id")
      tpe.fields(1).fieldType should be (ListType(StringType))

      tpe.fields(2).name should be ("bar")
      tpe.fields(2).fieldType should be (BooleanType)
    }

    "allow to set field complexity with config" in {
      val tpe = deriveObjectType[Unit, TestSubject](
        FieldComplexity("id", (_, _, child) ⇒ child * 123.0))

      tpe.fields(0).complexity.get((), Args(Map.empty), 2D) should be (246.0)
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
      implicit val FruitType = deriveEnumType[FruitAnnotated]()
      implicit val ColorType = deriveEnumType[ColorAnnotated.Value]()

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
            fruit
            comments {
              author
              text
              color
            }
          }
        """

      val schema = Schema(ArticleType)

      Executor.execute(schema, query, root = testArticle).await should be (Map(
        "data" → Map(
          "title" → "My First Article",
          "text" → "foo bar",
          "myTags" → null,
          "fruit" → "JustApple",
          "comments" → List(
            Map("author" → "bob", "text" → null, "color" → "NormalRed"),
            null,
            Map("author" → "jane", "text" → "yay!", "color" → "NormalRed")))))

      import sangria.marshalling.queryAst._
      import sangria.parser.DeliveryScheme.Throw

      val intro = IntrospectionParser.parse(Executor.execute(schema, sangria.introspection.introspectionQuery, root = testArticle).await)

      intro.queryType.name should be ("Article")

      val Some(articleIntro: IntrospectionObjectType) = intro.types.find(_.name == "Article")
      val Some(commentIntro: IntrospectionObjectType) = intro.types.find(_.name == "Comment")

      commentIntro.fields should have size 3

      commentIntro.fields(0).name should be ("author")
      commentIntro.fields(0).description should be (Some("The comment author"))

      commentIntro.fields(1).name should be ("text")
      commentIntro.fields(1).description should be (Some("Comment text"))

      commentIntro.fields(2).name should be ("color")
      commentIntro.fields(2).description should be (None)

      articleIntro.fields(3).name should be ("comments")
      articleIntro.fields(3).tpe should be (IntrospectionListTypeRef(IntrospectionNamedTypeRef(TypeKind.Object, "Comment")))

      articleIntro.fields(4).name should be ("fruit")
      articleIntro.fields(4).tpe should be (IntrospectionNonNullTypeRef(IntrospectionNamedTypeRef(TypeKind.Enum, "MyFruit")))
    }

    "be able handle recursive types with field overrides" in {
      case class A(id: Int, b: B)
      case class B(name: String, a: A, b: B)

      implicit lazy val AType = deriveObjectType[Unit, A](
        OverrideField("b", Field("b", BType, resolve = _.value.b)))

      implicit lazy val BType: ObjectType[Unit, B] = deriveObjectType(
        OverrideField("a", Field("a", AType, resolve = _.value.a)),
        OverrideField("b", Field("b", BType, resolve = _.value.b)))

      val schema = Schema(AType)

      val query =
        graphql"{id, b {name, a {id}, b {name}} }"

      Executor.execute(schema, query, root = A(1, B("foo", A(2, null), B("bar", null, null)))).await should be (Map(
        "data" → Map("id" → 1, "b" → Map("name" → "foo", "a" → Map("id" → 2), "b" → Map("name" → "bar")))))
    }
  }

  "Singleton Enum derivation" should {
    "use enum name and have no description by default" in {
      val singletonEnum = deriveEnumType[Fruit]()
      val enum = deriveEnumType[Color.Value]()

      singletonEnum.name should be ("Fruit")
      singletonEnum.description should be (None)

      enum.name should be ("Color")
      enum.description should be (None)
    }

    "allow to change name and description with config" in {
      val singletonEnum = deriveEnumType[Fruit](
        EnumTypeName("Foo"),
        EnumTypeDescription("It's foo"))

      val enum = deriveEnumType[Color.Value](
        EnumTypeName("Bar"),
        EnumTypeDescription("It's bar"))

      singletonEnum.name should be ("Foo")
      singletonEnum.description should be (Some("It's foo"))

      enum.name should be ("Bar")
      enum.description should be (Some("It's bar"))
    }

    "allow to change name and description with annotations" in {
      val singletonEnum = deriveEnumType[FruitAnnotated]()
      val enum = deriveEnumType[ColorAnnotated.Value]()

      singletonEnum.name should be ("MyFruit")
      singletonEnum.description should be (Some("Very tasty fruit"))

      enum.name should be ("MyColor")
      enum.description should be (Some("Very nice color"))
    }

    "prioritize config over annotation for name and description" in {
      val singletonEnum = deriveEnumType[FruitAnnotated](
        EnumTypeName("Foo"),
        EnumTypeDescription("It's foo"))

      val enum = deriveEnumType[ColorAnnotated.Value](
        EnumTypeName("Bar"),
        EnumTypeDescription("It's bar"))

      singletonEnum.name should be ("Foo")
      singletonEnum.description should be (Some("It's foo"))

      enum.name should be ("Bar")
      enum.description should be (Some("It's bar"))
    }

    "expose all enum values" in {
      val singletonEnum = deriveEnumType[Fruit]()
      val enum = deriveEnumType[Color.Value]()

      singletonEnum.values.map(_.name).sorted should be (List("Guave", "MegaOrange", "RedApple", "SuperBanana"))
      singletonEnum.values.map(_.value).sortBy(_.toString) should be (List(Guave, MegaOrange, RedApple, SuperBanana))

      enum.values.map(_.name).sorted should be (List("DarkBlue", "LightGreen", "Red"))
      enum.values.map(_.value).sortBy(_.toString) should be (List(Color.DarkBlue, Color.LightGreen, Color.Red))
    }

    "validate known values and mutually exclusive props" in {
      """deriveEnumType[Fruit](IncludeValues("Test"))""" shouldNot compile
      """deriveEnumType[Color.Value](IncludeValues("Test"))""" shouldNot compile
      """deriveEnumType[Fruit](ExcludeValues("Test"))""" shouldNot compile
      """deriveEnumType[Color.Value](ExcludeValues("Test"))""" shouldNot compile
      """deriveEnumType[Fruit](DocumentValue("Test", "Fooo"))""" shouldNot compile
      """deriveEnumType[Fruit](DeprecateValue("Test", "Fooo"))""" shouldNot compile
      """deriveEnumType[Fruit](RenameValue("Test", "Fooo"))""" shouldNot compile
      """deriveEnumType[Fruit](UppercaseValues, RenameValue("RedApple", "Fooo"))""" shouldNot compile
      """deriveEnumType[Fruit](ExcludeValues("RedApple", "SuperBanana", "MegaOrange", "Guave"))""" shouldNot compile
    }

    "respect whitelist and blacklist provided via config" in {
      val singletonEnum = deriveEnumType[Fruit](
        IncludeValues("RedApple", "SuperBanana"),
        ExcludeValues("SuperBanana"))

      val enum = deriveEnumType[Color.Value](
        IncludeValues("Red", "DarkBlue"),
        ExcludeValues("Red"))

      singletonEnum.values.map(_.name) should be ("RedApple" :: Nil)
      enum.values.map(_.name) should be ("DarkBlue" :: Nil)
    }

    "respect blacklist provided via annotations" in {
      val singletonEnum = deriveEnumType[FruitAnnotated]()
      val enum = deriveEnumType[ColorAnnotated.Value]()

      singletonEnum.values.map(_.name).sorted should be ("JustApple" :: "MegaOrangeAnnotated" :: Nil)
      enum.values.map(_.name).sorted should be ("Dark1Blue_FooStuff" :: "DarkBlue" :: "NormalRed" :: Nil)
    }

    "uppercase GraphQL enum values" in {
      val singletonEnum = deriveEnumType[FruitAnnotated](UppercaseValues)
      val enum = deriveEnumType[ColorAnnotated.Value](UppercaseValues)

      singletonEnum.values.map(_.name).sorted should be ("JUST_APPLE" :: "MEGA_ORANGE_ANNOTATED" :: Nil)
      enum.values.map(_.name).sorted should be ("DARK1_BLUE_FOO_STUFF" :: "DARK_BLUE" :: "NORMAL_RED" :: Nil)
    }

    "allow to set name, description and deprecationReason with config" in {
      val singletonEnum = deriveEnumType[Fruit](
        DocumentValue("RedApple", "apple!", deprecationReason = Some("foo")),
        RenameValue("SuperBanana", "JustBanana"),
        RenameValue("Guave", "Yay"),
        DocumentValue("Guave", "my color"),
        DeprecateValue("MegaOrange", "not cool"))

      val enum = deriveEnumType[Color.Value](
        DocumentValue("Red", "apple!!", deprecationReason = Some("foo")),
        RenameValue("LightGreen", "JustGreen"),
        DocumentValue("DarkBlue", "my color"),
        DeprecateValue("DarkBlue", "nah"))

      singletonEnum.values.sortBy(_.name) should be (List(
        EnumValue("JustBanana", None, SuperBanana, None),
        EnumValue("MegaOrange", None, MegaOrange, Some("not cool")),
        EnumValue("RedApple", Some("apple!"), RedApple, Some("foo")),
        EnumValue("Yay", Some("my color"), Guave, None)))

      enum.values.sortBy(_.name) should be (List(
        EnumValue("DarkBlue", Some("my color"), Color.DarkBlue, Some("nah")),
        EnumValue("JustGreen", None, Color.LightGreen, None),
        EnumValue("Red", Some("apple!!"), Color.Red, Some("foo"))))
    }

    "allow to set name, description and deprecationReason with annotations" in {
      val singletonEnum = deriveEnumType[FruitAnnotated]()
      val enum = deriveEnumType[ColorAnnotated.Value]()

      singletonEnum.values.sortBy(_.name) should be (List(
        EnumValue("JustApple", Some("The red one"), RedAppleAnnotated, None),
        EnumValue("MegaOrangeAnnotated", None, MegaOrangeAnnotated, Some("Not tasty anymore"))))

      enum.values.sortBy(_.name) should be (List(
        EnumValue("Dark1Blue_FooStuff", None, ColorAnnotated.Dark1Blue_FooStuff, None),
        EnumValue("DarkBlue", None, ColorAnnotated.DarkBlue, Some("Don't like blue")),
        EnumValue("NormalRed", Some("The red one"), ColorAnnotated.Red, None)))
    }

    "prioritize field config name, description, deprecationReason" in {
      val singletonEnum = deriveEnumType[FruitAnnotated](
        RenameValue("RedAppleAnnotated", "FooBar"))

      val enum = deriveEnumType[ColorAnnotated.Value](
        UppercaseValues,
        DocumentValue("Red", "TestTest"))

      singletonEnum.values.sortBy(_.name) should be (List(
        EnumValue("FooBar", Some("The red one"), RedAppleAnnotated, None),
        EnumValue("MegaOrangeAnnotated", None, MegaOrangeAnnotated, Some("Not tasty anymore"))))

      enum.values.sortBy(_.name) should be (List(
        EnumValue("DARK1_BLUE_FOO_STUFF", None, ColorAnnotated.Dark1Blue_FooStuff, None),
        EnumValue("DARK_BLUE", None, ColorAnnotated.DarkBlue, Some("Don't like blue")),
        EnumValue("NORMAL_RED", Some("TestTest"), ColorAnnotated.Red, None)))
    }
  }
}