package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.schema._
import sangria.macros._
import sangria.util.{DebugUtil, FutureResultSupport, GraphQlSupport}
import scala.concurrent.ExecutionContext.Implicits.global

class UnionInterfaceSpec extends WordSpec with Matchers with FutureResultSupport with GraphQlSupport {
  trait Named {
    def name: Option[String]
  }

  case class Dog(name: Option[String], barks: Option[Boolean]) extends Named
  case class Cat(name: Option[String], meows: Option[Boolean]) extends Named
  case class Person(name: Option[String], pets: Option[List[Option[Any]]], friends: Option[List[Option[Named]]]) extends Named

  val NamedType = InterfaceType("Named", fields[Unit, Named](
    Field("name", OptionType(StringType), resolve = _.value.name)))

  val DogType = ObjectType("Dog", interfaces[Unit, Dog](NamedType), fields[Unit, Dog](
    Field("barks", OptionType(BooleanType), resolve = _.value.barks)))

  val CatType = ObjectType("Cat", interfaces[Unit, Cat](NamedType), fields[Unit, Cat](
    Field("meows", OptionType(BooleanType), resolve = _.value.meows)))

  val PetType = UnionType[Unit]("Pet", types = DogType :: CatType :: Nil)

  val PersonType = ObjectType("Person", interfaces[Unit, Person](NamedType), fields[Unit, Person](
    Field("pets", OptionType(ListType(OptionType(PetType))), resolve = _.value.pets),
    Field("favouritePet", PetType, resolve = _.value.pets.flatMap(_.headOption.flatMap(identity)).get),
    Field("favouritePetList", ListType(PetType), resolve = _.value.pets.getOrElse(Nil).flatMap(x ⇒ x).toSeq),
    Field("favouritePetOpt", OptionType(PetType), resolve = _.value.pets.flatMap(_.headOption.flatMap(identity))),
    Field("friends", OptionType(ListType(OptionType(NamedType))), resolve = _.value.friends)))
  
  val TestSchema = Schema(PersonType)

  val garfield = Cat(Some("Garfield"), Some(false))
  val odie = Dog(Some("Odie"), Some(true))
  val liz = Person(Some("Liz"), None, None)
  val bob = Person(Some("Bob"), Some(List(Some(garfield), Some(odie))), Some(List(Some(liz), Some(odie))))

  val schema = Schema(PersonType)

  "Execute: Union and intersection types" should {
    "introspect on union and intersection types" in check(
      (),
      """
        {
          Named: __type(name: "Named") {
            kind
            name
            fields { name }
            interfaces { name }
            possibleTypes { name }
            enumValues { name }
            inputFields { name }
          }
          Pet: __type(name: "Pet") {
            kind
            name
            fields { name }
            interfaces { name }
            possibleTypes { name }
            enumValues { name }
            inputFields { name }
          }
        }
      """,
      Map(
        "data" → Map(
          "Named" → Map(
            "kind" → "INTERFACE",
            "name" → "Named",
            "fields" → List(
              Map("name" → "name")
            ),
            "interfaces" → null,
            "possibleTypes" → List(
              Map("name" → "Cat"),
              Map("name" → "Dog"),
              Map("name" → "Person")
            ),
            "enumValues" → null,
            "inputFields" → null
          ),
          "Pet" → Map(
            "kind" → "UNION",
            "name" → "Pet",
            "fields" → null,
            "interfaces" → null,
            "possibleTypes" → List(
              Map("name" → "Dog"),
              Map("name" → "Cat")
            ),
            "enumValues" → null,
            "inputFields" → null
          )
        )
      )
    )

    "executes using union types" in check(
      bob,
      """
       {
         __typename
         name
         favouritePet {name}
         favouritePetOpt {name}
         pets {
           __typename
           name
           barks
           meows
         }
       }
      """,
      Map(
        "data" → Map(
          "__typename" → "Person",
          "name" → "Bob",
          "favouritePet" → Map("name" → "Garfield"),
          "favouritePetOpt" → Map("name" → "Garfield"),
          "pets" → List(
            Map("__typename" → "Cat", "name" → "Garfield", "meows" → false),
            Map("__typename" → "Dog", "name" → "Odie", "barks" → true)
          )
        )
      ) ,
      validateQuery = false
    )

    "executes union types with inline fragments" in check(
      bob,
      """
        {
          __typename
          name
          pets {
            __typename
            ... on Dog {
              name
              barks
            }
            ... on Cat {
              name
              meows
            }
          }
        }
      """,
      Map(
        "data" → Map(
          "__typename" → "Person",
          "name" → "Bob",
          "pets" → List(
            Map("__typename" → "Cat", "name" → "Garfield", "meows" → false),
            Map("__typename" → "Dog", "name" → "Odie", "barks" → true)
          )
        )
      )
    )

    "executes using interface types" in check(
      bob,
      """
        {
          __typename
          name
          friends {
            __typename
            name
            barks
            meows
          }
        }
      """,
      Map(
        "data" → Map(
          "__typename" → "Person",
          "name" → "Bob",
          "friends" → List(
            Map("__typename" → "Person", "name" → "Liz"),
            Map("__typename" → "Dog", "name" → "Odie", "barks" → true)
          )
        )
      ),
      validateQuery = false
    )

    "executes interface types with inline fragments" in check(
      bob,
      """
        {
          __typename
          name
          friends {
            __typename
            name
            ... on Dog {
              barks
            }
            ... on Cat {
              meows
            }
          }
        }
      """,
      Map(
        "data" → Map(
          "__typename" → "Person",
          "name" → "Bob",
          "friends" → List(
            Map("__typename" → "Person", "name" → "Liz"),
            Map("__typename" → "Dog", "name" → "Odie", "barks" → true)
          )
        )
      )
    )

    "allows fragment conditions to be abstract types" in check(
      bob,
      """
        {
          __typename
          name
          pets { ...PetFields }
          friends { ...FriendFields }
        }

        fragment PetFields on Pet {
          __typename
          ... on Dog {
            name
            barks
          }
          ... on Cat {
            name
            meows
          }
        }

        fragment FriendFields on Named {
          __typename
          name
          ... on Dog {
            barks
          }
          ... on Cat {
            meows
          }
        }
      """,
      Map(
        "data" → Map(
          "__typename" → "Person",
          "name" → "Bob",
          "pets" → List(
            Map("__typename" → "Cat", "name" → "Garfield", "meows" → false),
            Map("__typename" → "Dog", "name" → "Odie", "barks" → true)
          ),
          "friends" → List(
            Map("__typename" → "Person", "name" → "Liz"),
            Map("__typename" → "Dog", "name" → "Odie", "barks" → true)
          )
        )
      )
    )

    "caching should respect output object polymorphism" in {
      trait FooBar{
        def baz: Baz
      }

      case class Foo(baz: Baz) extends FooBar
      case class Bar(baz: Baz) extends FooBar
      case class Baz(quz: Seq[Quz])
      case class Quz(id: String, i: Int)

      val QuzType = ObjectType("Quz", fields[Unit, Quz](
        Field("id", StringType, resolve = _.value.id),
        Field("i", IntType, resolve = _.value.i)))

      val BazType = ObjectType("Baz", fields[Unit, Baz](
        Field("quz", OptionType(ListType(OptionType(QuzType))),
          arguments = Argument("id", OptionInputType(ListInputType(StringType))) :: Nil,
          resolve = c ⇒ {
            c.argOpt[Seq[String]]("id")
              .map(queried ⇒ c.value.quz.filter(quz ⇒ queried.contains(quz.id)))
              .getOrElse(c.value.quz)
              .map(Some(_))
          })))

      val FooBarType = InterfaceType("FooBar", fields[Unit, FooBar](
        Field("baz", OptionType(BazType), resolve = _.value.baz)))

      val FooType = ObjectType("Foo", interfaces[Unit, Foo](FooBarType), fields[Unit, Foo]())
      val BarType = ObjectType("Bar", interfaces[Unit, Bar](FooBarType), fields[Unit, Bar]())
      val FooBarBazType = UnionType("FooBarBaz", types = FooType :: BarType :: BazType :: Nil)

      val QueryType = ObjectType("Query", fields[Unit, List[Any]](
        Field("foo", OptionType(ListType(OptionType(FooBarBazType))), resolve = _.value map (Some(_)))))

      val schema = Schema(QueryType)

      val query =
        graphql"""
          {
            foo {
              __typename
              ... on Foo {
                baz{
                  quz(id: ["one"]){ id }
                }
              }
              ... on Bar {
                baz {
                  quz(id: ["two"]){ id i }
                }
              }
            }
          }
        """

      val data = List(
        Foo(Baz(Seq(Quz("one", 1), Quz("three", 3), Quz("five", 5)))),
        Baz(Seq(Quz("100", 100))),
        Bar(Baz(Seq(Quz("two", 2), Quz("four", 4), Quz("six", 6)))))

      Executor.execute(schema, query, root = data).await should be (
        Map(
          "data" → Map(
            "foo" → Vector(
              Map(
                "__typename" → "Foo",
                "baz" → Map(
                  "quz" → Vector(
                    Map("id" → "one")))),
              Map("__typename" → "Baz"),
              Map(
                "__typename" → "Bar",
                "baz" → Map(
                  "quz" → Vector(
                    Map(
                      "id" → "two",
                      "i" → 2))))))))
    }
  }
}