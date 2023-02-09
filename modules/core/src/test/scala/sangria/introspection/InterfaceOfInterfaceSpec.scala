package sangria.introspection

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.schema._
import sangria.util.{FutureResultSupport, GraphQlSupport}

class InterfaceOfInterfaceSpec
    extends AnyWordSpec
    with Matchers
    with FutureResultSupport
    with GraphQlSupport {
  trait Named {
    def name: Option[String]
  }

  case class Dog(name: Option[String], barks: Option[Boolean]) extends Named

  val NamedType = InterfaceType(
    "Named",
    fields[Unit, Named](Field("name", OptionType(StringType), resolve = _.value.name)))

  val DogType = ObjectType(
    "Dog",
    interfaces[Unit, Dog](NamedType),
    fields[Unit, Dog](Field("barks", OptionType(BooleanType), resolve = _.value.barks)))

  val schema = Schema(DogType)

  "Interfaces of interfaces introspection" should {
    "introspect on interface of an interface should be empty array" in check(
      (),
      """
        {
          Named: __type(name: "Named") {
            kind
            name
            fields { name }
            interfaces { name }
          }
        }
      """,
      Map(
        "data" -> Map(
          "Named" -> Map(
            "kind" -> "INTERFACE",
            "name" -> "Named",
            "fields" -> List(
              Map("name" -> "name")
            ),
            "interfaces" -> List.empty
          )
        )
      )
    )
  }
}
