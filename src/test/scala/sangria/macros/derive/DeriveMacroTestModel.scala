package sangria.macros.derive

import sangria.execution.FieldTag
import sangria.execution.deferred.Deferred
import sangria.schema._

object DeriveMacroTestModel {
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

  object Color extends Enumeration {
    val Red, LightGreen, DarkBlue = Value
  }

  case class TestDefer(id: Int) extends Deferred[Option[List[Int]]]
}