package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.util.Pos
import sangria.util.SimpleGraphQlSupport._
import sangria.validation.{ImplementationExtraFieldArgumentNotOptionalViolation, InvalidImplementationFieldArgumentTypeViolation, MissingImplementationFieldArgumentViolation, InvalidImplementationFieldTypeViolation}

class TypeFieldConstraintsSpec extends WordSpec with Matchers {

  "ObjectType" should {
    "allow unique fields" in {
      ObjectType("Test", fields[Unit, Unit](
        Field("a", StringType, resolve = _ ⇒ "foo"),
        Field("b", StringType, resolve = _ ⇒ "foo"),
        Field("c", StringType, resolve = _ ⇒ "foo")
      ))

      ObjectType("Test", () ⇒ fields[Unit, Unit](
        Field("a", StringType, resolve = _ ⇒ "foo"),
        Field("b", StringType, resolve = _ ⇒ "foo"),
        Field("c", StringType, resolve = _ ⇒ "foo")
      )).fields
    }

    "disallow non-unique fields" in {
      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test", fields[Unit, Unit](
          Field("a", StringType, resolve = _ ⇒ "foo"),
          Field("b", StringType, resolve = _ ⇒ "foo"),
          Field("a", StringType, resolve = _ ⇒ "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test", () ⇒ fields[Unit, Unit](
          Field("a", StringType, resolve = _ ⇒ "foo"),
          Field("b", StringType, resolve = _ ⇒ "foo"),
          Field("a", StringType, resolve = _ ⇒ "foo")
        )).fields
      }
    }

    "disallow invalid names" in {
      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test-object", fields[Unit, Unit](
          Field("a", StringType, resolve = _ ⇒ "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test", () ⇒ fields[Unit, Unit](
          Field("a-b-c", StringType, resolve = _ ⇒ "foo")
        )).fields
      }
    }
  }

  "InterfaceType" should {
    "allow unique fields" in {
      InterfaceType("Test", fields[Unit, Unit](
        Field("a", StringType, resolve = _ ⇒ "foo"),
        Field("b", StringType, resolve = _ ⇒ "foo"),
        Field("c", StringType, resolve = _ ⇒ "foo")
      ))

      InterfaceType("Test", () ⇒ fields[Unit, Unit](
        Field("a", StringType, resolve = _ ⇒ "foo"),
        Field("b", StringType, resolve = _ ⇒ "foo"),
        Field("c", StringType, resolve = _ ⇒ "foo")
      )).fields
    }

    "disallow non-unique fields" in {
      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test", fields[Unit, Unit](
          Field("a", StringType, resolve = _ ⇒ "foo"),
          Field("b", StringType, resolve = _ ⇒ "foo"),
          Field("a", StringType, resolve = _ ⇒ "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test", () ⇒ fields[Unit, Unit](
          Field("a", StringType, resolve = _ ⇒ "foo"),
          Field("b", StringType, resolve = _ ⇒ "foo"),
          Field("a", StringType, resolve = _ ⇒ "foo")
        )).fields
      }
    }

    "disallow invalid names" in {
      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test-int", fields[Unit, Unit](
          Field("a", StringType, resolve = _ ⇒ "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test", fields[Unit, Unit](
          Field("a-b-c", StringType, resolve = _ ⇒ "foo")
        )).fields
      }
    }
  }

  "InputObjectType" should {
    "allow unique fields" in {
      InputObjectType("Test", List(
        InputField("a", StringType),
        InputField("b", StringType),
        InputField("c", StringType)
      ))

      InputObjectType("Test", () ⇒ List(
        InputField("a", StringType),
        InputField("b", StringType),
        InputField("c", StringType)
      )).fields
    }

    "disallow non-unique fields" in {
      an [IllegalArgumentException] should be thrownBy {
        InputObjectType("Test", List(
          InputField("a", StringType),
          InputField("b", StringType),
          InputField("a", StringType)
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        InputObjectType("Test", () ⇒ List(
          InputField("a", StringType),
          InputField("b", StringType),
          InputField("a", StringType)
        )).fields
      }
    }

    "disallow invalid names" in {
      an [IllegalArgumentException] should be thrownBy {
        InputObjectType("Test-ab", List(
          InputField("a", StringType),
          InputField("b", StringType),
          InputField("a", StringType)
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        InputObjectType("Test", List(
          InputField("a-b", StringType)
        ))
      }
    }
  }

  case class A(b: Option[B], name: String)
  case class B(a: A, size: Int)

  val AType: ObjectType[Unit, A] = ObjectType("A", fields[Unit, A](
    Field("name", StringType, resolve = _.value.name),
    Field("b", OptionType(BType), resolve = _.value.b)))

  lazy val BType: ObjectType[Unit, B] = ObjectType("B", fields[Unit, B](
    Field("size", IntType, resolve = _.value.size),
    Field("a", AType, resolve = _.value.a)))

  "Schema" should {
    "provide a helpful error message if circular references are detected" in {
      val QueryType = ObjectType("Query", fields[Unit, Unit](
        Field("a", AType, resolve = _ ⇒ A(Some(B(A(None, "bar"), 1)), "foo"))
      ))

      val error = intercept[IllegalStateException](Schema(QueryType))

      error.getMessage should include("A `null` value was provided instead of type for a field 'a' of 'B' type.")
    }

    "ensure that implemented fields have correct type" in {
      val FruitType = InterfaceType("Fruit", fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1)
      ))

      val SomeOtherInterfaceType = InterfaceType("SomeOtherInterfaceType", fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1)
      ))

      val AppleType = ObjectType("Apple", interfaces[Unit, Unit](FruitType), fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1),
        Field("color", StringType, resolve = _ ⇒ "red")
      ))

      val BasketType = InterfaceType("Basket", fields[Unit, Unit](
        Field("fruit", FruitType, resolve = _ ⇒ ())
      ))

      val AppleBasketType = ObjectType("AppleBasket", interfaces[Unit, Unit](BasketType), fields[Unit, Unit](
        Field("fruit", SomeOtherInterfaceType, resolve = _ ⇒ ())
      ))

      val QueryType = ObjectType("Query", fields[Unit, Unit](
        Field("basket", BasketType, resolve = _ ⇒ ())
      ))


      val error = intercept[SchemaValidationException](
        Schema(QueryType, additionalTypes = AppleType :: AppleBasketType :: Nil))

      error.violations should contain(
        InvalidImplementationFieldTypeViolation("Basket", "AppleBasket", "fruit", "Fruit!", "SomeOtherInterfaceType!"))
    }

    "ensure that all interface field arguments are present in the implementation" in {
      val FruitType = InterfaceType("Fruit", fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1),
        Field("slice", IntType,
          arguments = Argument("parts", IntType) :: Nil,
          resolve = _.args.arg[Int]("parts"))
      ))

      val AppleType = ObjectType("Apple", interfaces[Unit, Unit](FruitType), fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1),
        Field("slice", IntType,
          arguments = Argument("pieces", IntType) :: Nil,
          resolve = _.args.arg[Int]("pieces"))
      ))

      val QueryType = ObjectType("Query", fields[Unit, Unit](
        Field("fruit", FruitType, resolve = _ ⇒ ())
      ))

      val error = intercept[SchemaValidationException](
        Schema(QueryType, additionalTypes = AppleType :: Nil))

      error.violations should contain(
        MissingImplementationFieldArgumentViolation("Fruit", "Apple", "slice", "parts"))
    }

    "ensure that all interface field argument types are the same in the implementation" in {
      val FruitType = InterfaceType("Fruit", fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1),
        Field("slice", IntType,
          arguments = Argument("parts", IntType) :: Nil,
          resolve = _.args.arg[Int]("parts"))
      ))

      val AppleType = ObjectType("Apple", interfaces[Unit, Unit](FruitType), fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1),
        Field("slice", IntType,
          arguments = Argument("parts", StringType) :: Nil,
          resolve = _.args.arg[String]("parts").toInt)
      ))

      val QueryType = ObjectType("Query", fields[Unit, Unit](
        Field("fruit", FruitType, resolve = _ ⇒ ())
      ))

      val error = intercept[SchemaValidationException](
        Schema(QueryType, additionalTypes = AppleType :: Nil))

      error.violations should contain(
        InvalidImplementationFieldArgumentTypeViolation("Fruit", "Apple", "slice", "parts", "Int!", "String!"))
    }

    "ensure that all implementation extra field arguments are optional" in {
      val FruitType = InterfaceType("Fruit", fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1),
        Field("slice", IntType,
          arguments = Argument("parts", IntType) :: Nil,
          resolve = _.args.arg[Int]("parts"))
      ))

      val AppleType = ObjectType("Apple", interfaces[Unit, Unit](FruitType), fields[Unit, Unit](
        Field("size", IntType, resolve = _ ⇒ 1),
        Field("slice", IntType,
          arguments = Argument("parts", IntType) :: Argument("careful", BooleanType) :: Nil,
          resolve = _.args.arg[Int]("parts"))
      ))

      val QueryType = ObjectType("Query", fields[Unit, Unit](
        Field("fruit", FruitType, resolve = _ ⇒ ())
      ))

      val error = intercept[SchemaValidationException](
        Schema(QueryType, additionalTypes = AppleType :: Nil))

      error.violations should contain(
        ImplementationExtraFieldArgumentNotOptionalViolation("Fruit", "Apple", "slice", "careful", "Boolean!"))
    }

    "allow covariant return types" in {
      trait Fruit {
        def size: Int
      }

      case class Apple(size: Int, color: String) extends Fruit

      trait Basket {
        def fruit: Fruit
      }

      case class AppleBasket(fruit: Apple) extends Basket

      val FruitType = InterfaceType("Fruit", fields[Unit, Fruit](
        Field("size", IntType, resolve = _.value.size),
        Field("slice", IntType,
          arguments = Argument("parts", IntType) :: Nil,
          resolve = _.args.arg[Int]("parts"))
      ))

      val AppleType = ObjectType("Apple", interfaces[Unit, Apple](FruitType), fields[Unit, Apple](
        Field("size", IntType, resolve = _.value.size),
        Field("slice", IntType,
          arguments = Argument("parts", IntType) :: Argument("careful", OptionInputType(BooleanType)) :: Nil,
          resolve = _.args.arg[Int]("parts")),
        Field("color", StringType, resolve = _.value.color)
      ))

      val BasketType = InterfaceType("Basket", fields[Unit, Basket](
        Field("fruit", FruitType, resolve = _.value.fruit)
      ))

      val AppleBasketType = ObjectType("AppleBasket", interfaces[Unit, AppleBasket](BasketType), fields[Unit, AppleBasket](
        Field("fruit", AppleType, resolve = _.value.fruit)
      ))

      val QueryType = ObjectType("Query", fields[Unit, Basket](
        Field("basket", BasketType, resolve = _.value)
      ))

      val schema = Schema(QueryType, additionalTypes = AppleType :: AppleBasketType :: Nil)

      check(
        schema,
        AppleBasket(Apple(11, "red")),
        """
         {
          basket {
            fruit {
              size
              slice(parts: 5)
            }
          }
         }
       """,
        Map("data" -> Map(
          "basket" -> Map(
            "fruit" -> Map(
              "size" -> 11,
              "slice" -> 5)))))

      checkContainsErrors(
        schema,
        AppleBasket(Apple(11, "red")),
        """
         {
          basket {
            fruit {
              color
            }
          }
         }
        """,
        null,
        List("Cannot query field 'color' on type 'Fruit'. Did you mean to use an inline fragment on 'Apple'?" → List(Pos(5, 15))))
    }
  }

}
