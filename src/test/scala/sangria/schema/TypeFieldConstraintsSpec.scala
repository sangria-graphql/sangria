package sangria.schema

import org.scalatest.{Matchers, WordSpec}

class TypeFieldConstraintsSpec extends WordSpec with Matchers {

  "ObjectType" should {
    "allow unique fields" in {
      ObjectType("Test", fields[Unit, Unit](
        Field("a", StringType, resolve = _ => "foo"),
        Field("b", StringType, resolve = _ => "foo"),
        Field("c", StringType, resolve = _ => "foo")
      ))

      ObjectType("Test", () => fields[Unit, Unit](
        Field("a", StringType, resolve = _ => "foo"),
        Field("b", StringType, resolve = _ => "foo"),
        Field("c", StringType, resolve = _ => "foo")
      )).fields
    }

    "disallow non-unique fields" in {
      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test", fields[Unit, Unit](
          Field("a", StringType, resolve = _ => "foo"),
          Field("b", StringType, resolve = _ => "foo"),
          Field("a", StringType, resolve = _ => "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test", () => fields[Unit, Unit](
          Field("a", StringType, resolve = _ => "foo"),
          Field("b", StringType, resolve = _ => "foo"),
          Field("a", StringType, resolve = _ => "foo")
        )).fields
      }
    }

    "disallow invalid names" in {
      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test-object", fields[Unit, Unit](
          Field("a", StringType, resolve = _ => "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        ObjectType("Test", () => fields[Unit, Unit](
          Field("a-b-c", StringType, resolve = _ => "foo")
        )).fields
      }
    }
  }

  "InterfaceType" should {
    "allow unique fields" in {
      InterfaceType("Test", fields[Unit, Unit](
        Field("a", StringType, resolve = _ => "foo"),
        Field("b", StringType, resolve = _ => "foo"),
        Field("c", StringType, resolve = _ => "foo")
      ))

      InterfaceType("Test", () => fields[Unit, Unit](
        Field("a", StringType, resolve = _ => "foo"),
        Field("b", StringType, resolve = _ => "foo"),
        Field("c", StringType, resolve = _ => "foo")
      )).fields
    }

    "disallow non-unique fields" in {
      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test", fields[Unit, Unit](
          Field("a", StringType, resolve = _ => "foo"),
          Field("b", StringType, resolve = _ => "foo"),
          Field("a", StringType, resolve = _ => "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test", () => fields[Unit, Unit](
          Field("a", StringType, resolve = _ => "foo"),
          Field("b", StringType, resolve = _ => "foo"),
          Field("a", StringType, resolve = _ => "foo")
        )).fields
      }
    }

    "disallow invalid names" in {
      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test-int", fields[Unit, Unit](
          Field("a", StringType, resolve = _ => "foo")
        ))
      }

      an [IllegalArgumentException] should be thrownBy {
        InterfaceType("Test", fields[Unit, Unit](
          Field("a-b-c", StringType, resolve = _ => "foo")
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

      InputObjectType("Test", () => List(
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
        InputObjectType("Test", () => List(
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
        Field("a", AType, resolve = _ => A(Some(B(A(None, "bar"), 1)), "foo"))
      ))

      val error = intercept[IllegalStateException](Schema(QueryType))

      error.getMessage should include("A `null` value was provided instead of type for a field 'a' of 'B' type.")
    }
  }

}
