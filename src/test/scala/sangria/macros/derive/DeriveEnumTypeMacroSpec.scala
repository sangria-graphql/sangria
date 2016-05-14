package sangria.macros.derive

import org.scalatest.{Matchers, WordSpec}
import sangria.schema._

class DeriveEnumTypeMacroSpec extends WordSpec with Matchers {
  import DeriveMacroTestModel._

  sealed trait Difficulty

  object Difficulty {
    case object Easy extends Difficulty
    case object Medium extends Difficulty
    case object Hard extends Difficulty
  }

  sealed trait Fruit

  case object RedApple extends Fruit
  case object SuperBanana extends Fruit
  case object MegaOrange extends Fruit

  sealed abstract class ExoticFruit(val score: Int) extends Fruit

  case object Guave extends ExoticFruit(123)

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

  "Enum derivation" should {
    "support nested enum instances"  in {
      val enum = deriveEnumType[Difficulty]()
      enum.name should be ("Difficulty")
      enum.values.map(_.name).sorted should be (List(
        "Easy", "Hard", "Medium"
      ))
    }

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
