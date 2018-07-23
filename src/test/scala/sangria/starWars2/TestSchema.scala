package sangria.starWars2

import sangria.execution.UserFacingError
import sangria.schema._

import scala.concurrent.Future

object TestSchema {
  case class PrivacyError(message: String) extends Exception(message) with UserFacingError

  val EpisodeEnum = EnumType(
    "Episode",
    Some("One of the films in the Star Wars Trilogy"),
    List(
      EnumValue("NEWHOPE",
        value = TestData.Episode.NEWHOPE,
        description = Some("Released in 1977.")),
      EnumValue("EMPIRE",
        value = TestData.Episode.EMPIRE,
        description = Some("Released in 1980.")),
      EnumValue("JEDI",
        value = TestData.Episode.JEDI,
        description = Some("Released in 1983."))))

  val Character: InterfaceType[Unit, TestData.Character] =
    InterfaceType(
      name = "Character",
      description = Some("A character in the Star Wars Trilogy"),
      fieldsFn = () ⇒ fields[Unit, TestData.Character](
        Field("id", StringType,
          Some("The id of the character."),
          resolve = _.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the character."),
          resolve = _.value.name),
        Field("friends", OptionType(ListType(OptionType(Character))),
          Some("The friends of the character, or an empty list if they have none."),
          resolve = ctx ⇒ TestData.DeferFriends(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e))),
        Field("secretBackstory", OptionType(StringType),
          Some("Where are they from and how they came to be who they are."),
          resolve = _ ⇒ throw PrivacyError("secretBackstory is secret."))),
      interfaces = Nil,
      manualPossibleInterfaces =
        () => List(Humanoid, Droid, HumanoDroid, MonoDroid),
      manualPossibleTypes =
        () => List(Human, PrimaryDroid, SecondaryDroid),
      astDirectives = Vector.empty,
      astNodes = Vector.empty)

  lazy val Humanoid: InterfaceType[Unit, TestData.Humanoid] =
    InterfaceType(
      name = "Humanoid",
      description = None,
      fieldsFn = () => fields[Unit, TestData.Humanoid](
        Field("id", StringType,
          Some("The id of the humanoid."),
          tags = ProjectionName("_id") :: Nil,
          resolve = _.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the humanoid."),
          resolve = _.value.name),
        Field("friends", OptionType(ListType(OptionType(Character))),
          Some("The friends of the humanoid, or an empty list if they have none."),
          resolve = (ctx) ⇒ TestData.DeferFriends(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e)))
      ),
      interfaces = List(Character),
      manualPossibleInterfaces =
        () => Nil,
      manualPossibleTypes =
        () => List(Human),
      astDirectives = Vector.empty,
      astNodes = Vector.empty)

  val Human =
    ObjectType(
      "Human",
      "A humanoid creature in the Star Wars universe.",
      interfaces[Unit, TestData.Human](Humanoid),
      fields[Unit, TestData.Human](
        Field("id", StringType,
          Some("The id of the human."),
          resolve = _.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the human."),
          resolve = _.value.name),
        Field("friends", OptionType(ListType(OptionType(Character))),
          Some("The friends of the human, or an empty list if they have none."),
          resolve = (ctx) ⇒ TestData.DeferFriends(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e))),
        Field("homePlanet", OptionType(StringType),
          Some("The home planet of the human, or null if unknown."),
          resolve = _.value.homePlanet)
      ))

  lazy val Droid: InterfaceType[Unit, TestData.Droid] =
    InterfaceType(
      name = "Droid",
      description = Some("A mechanical creature in the Star Wars universe."),
      fieldsFn = () => fields[Unit, TestData.Droid](
        Field("id", StringType,
          Some("The id of the droid."),
          tags = ProjectionName("_id") :: Nil,
          resolve = ctx => ctx.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the droid."),
          resolve = ctx ⇒ Future.successful(ctx.value.name)),
        Field("friends", OptionType(ListType(OptionType(Character))),
          Some("The friends of the droid, or an empty list if they have none."),
          resolve = ctx ⇒ TestData.DeferFriends(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e))),
        Field("primaryFunction", OptionType(StringType),
          Some("The primary function of the droid."),
          resolve = ctx => ctx.value.primaryFunction),
        Field("follows", OptionType(Character),
          Some("The character this droid follows"),
          resolve = ctx => ctx.value.follows)
      ),
      interfaces = List(Character),
      manualPossibleInterfaces =
        () => List(HumanoDroid, MonoDroid),
      manualPossibleTypes =
        () => List(PrimaryDroid, SecondaryDroid),
      astDirectives = Vector.empty,
      astNodes = Vector.empty)

  lazy val HumanoDroid: InterfaceType[Unit, TestData.HumanoDroid] =
    InterfaceType(
      name = "HumanoDroid",
      description = None,
      fieldsFn = () => fields[Unit, TestData.HumanoDroid](
        Field("id", StringType,
          Some("The id of the droid."),
          tags = ProjectionName("_id") :: Nil,
          resolve = ctx => ctx.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the droid."),
          resolve = ctx ⇒ Future.successful(ctx.value.name)),
        Field("friends", OptionType(ListType(OptionType(Character))),
          Some("The friends of the droid, or an empty list if they have none."),
          resolve = ctx ⇒ TestData.DeferFriends(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e))),
        Field("primaryFunction", OptionType(StringType),
          Some("The primary function of the droid."),
          resolve = ctx => ctx.value.primaryFunction),
        Field("follows", OptionType(Humanoid),
          Some("The character this droid follows"),
          resolve = ctx => ctx.value.follows)
      ),
      interfaces = List(Droid),
      manualPossibleInterfaces =
        () => Nil,
      manualPossibleTypes =
        () => List(PrimaryDroid),
      astDirectives = Vector.empty,
      astNodes = Vector.empty)

  lazy val MonoDroid: InterfaceType[Unit, TestData.MonoDroid] =
    InterfaceType(
      name = "MonoDroid",
      description = None,
      fieldsFn = () => fields[Unit, TestData.MonoDroid](
        Field("id", StringType,
          Some("The id of the droid."),
          tags = ProjectionName("_id") :: Nil,
          resolve = ctx => ctx.value.id),
        Field("name", OptionType(StringType),
          Some("The name of the droid."),
          resolve = ctx ⇒ Future.successful(ctx.value.name)),
        Field("friends", OptionType(ListType(OptionType(Character))),
          Some("The friends of the droid, or an empty list if they have none."),
          resolve = ctx ⇒ TestData.DeferFriends(ctx.value.friends)),
        Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
          Some("Which movies they appear in."),
          resolve = _.value.appearsIn map (e ⇒ Some(e))),
        Field("primaryFunction", OptionType(StringType),
          Some("The primary function of the droid."),
          resolve = ctx => ctx.value.primaryFunction),
        Field("follows", OptionType(Droid),
          Some("The character this droid follows"),
          resolve = ctx => ctx.value.follows)
      ),
      interfaces = List(Droid),
      manualPossibleInterfaces =
        () => Nil,
      manualPossibleTypes =
        () => List(SecondaryDroid),
      astDirectives = Vector.empty,
      astNodes = Vector.empty)

  lazy val PrimaryDroid = ObjectType(
    "PrimaryDroid",
    "A droid who follows a humanoid",
    interfaces[Unit, TestData.PrimaryDroid](HumanoDroid),
    fields[Unit, TestData.PrimaryDroid](
      Field("id", StringType,
        Some("The id of the droid."),
        tags = ProjectionName("_id") :: Nil,
        resolve = _.value.id),
      Field("name", OptionType(StringType),
        Some("The name of the droid."),
        resolve = ctx ⇒ Future.successful(ctx.value.name)),
      Field("friends", OptionType(ListType(OptionType(Character))),
        Some("The friends of the droid, or an empty list if they have none."),
        resolve = ctx ⇒ TestData.DeferFriends(ctx.value.friends)),
      Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
        Some("Which movies they appear in."),
        resolve = _.value.appearsIn map (e ⇒ Some(e))),
      Field("primaryFunction", OptionType(StringType),
        Some("The primary function of the droid."),
        resolve = _.value.primaryFunction),
      Field("follows", OptionType(Humanoid),
        None,
        resolve = _.value.follows)
    ))

  lazy val SecondaryDroid = ObjectType(
    "SecondaryDroid",
    "A droid who follows another droid",
    interfaces[Unit, TestData.SecondaryDroid](MonoDroid),
    fields[Unit, TestData.SecondaryDroid](
      Field("id", StringType,
        Some("The id of the droid."),
        tags = ProjectionName("_id") :: Nil,
        resolve = _.value.id),
      Field("name", OptionType(StringType),
        Some("The name of the droid."),
        resolve = ctx ⇒ Future.successful(ctx.value.name)),
      Field("friends", OptionType(ListType(OptionType(Character))),
        Some("The friends of the droid, or an empty list if they have none."),
        resolve = ctx ⇒ TestData.DeferFriends(ctx.value.friends)),
      Field("appearsIn", OptionType(ListType(OptionType(EpisodeEnum))),
        Some("Which movies they appear in."),
        resolve = _.value.appearsIn map (e ⇒ Some(e))),
      Field("primaryFunction", OptionType(StringType),
        Some("The primary function of the droid."),
        resolve = _.value.primaryFunction),
      Field("follows", OptionType(Droid),
        None,
        resolve = _.value.follows)
    ))

  val ID = Argument("id", StringType, description = "id of the character")

  val EpisodeArg = Argument("episode", OptionInputType(EpisodeEnum),
    description = "If omitted, returns the hero of the whole saga. If provided, returns the hero of that particular episode.")

  val Query = ObjectType[TestData.CharacterRepo, Unit](
    "Query", fields[TestData.CharacterRepo, Unit](
      Field("hero", Character,
        arguments = EpisodeArg :: Nil,
        resolve = (ctx) ⇒ ctx.ctx.getHero(ctx.arg(EpisodeArg))),
      Field("human", OptionType(Human),
        arguments = ID :: Nil,
        resolve = ctx ⇒ ctx.ctx.getHuman(ctx arg ID)),
      Field("droid", Droid,
        arguments = ID :: Nil,
        resolve = Projector((ctx, f)⇒ ctx.ctx.getDroid(ctx arg ID).get))
    ))

  val StarWarsSchema = Schema(Query)
}
