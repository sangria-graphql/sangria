package sangria.validation

import org.scalatest.{Matchers, WordSpec}
import sangria.schema._
import sangria.macros._

class DocumentAnalyzerSpec extends WordSpec with Matchers {
  val NumberType = EnumType("Number", values = List(
    EnumValue("ONE", value = 1),
    EnumValue("TWO", value = 2, deprecationReason = Some("Some enum reason."))))

  val QueryType = ObjectType("Query", fields[Unit, Unit](
    Field("normalField", OptionType(NumberType),
      arguments = Argument("enumArg", OptionInputType(NumberType)) :: Nil,
      resolve = ctx ⇒ ctx.argOpt[Int]("enumArg")),

    Field("deprecatedField", OptionType(StringType),
      deprecationReason = Some("Some field reason."),
      resolve = _ ⇒ "foo")))

  val schema = Schema(QueryType)

  "DocumentAnalyzer" should {
    "report empty set for no deprecated usages" in {
      DocumentAnalyzer(schema, gql"""{ normalField(enumArg: ONE) }""").deprecatedUsages should have size 0
    }

    "report usage of deprecated fields" in {
      DocumentAnalyzer(schema, gql"""{ normalField, deprecatedField }""").deprecatedUsages.map(_.errorMessage) should
        contain("The field 'Query.deprecatedField' is deprecated. Some field reason.")
    }

    "report usage of deprecated enums" in {
      DocumentAnalyzer(schema, gql"""{ normalField(enumArg: TWO) }""").deprecatedUsages.map(_.errorMessage) should
        contain("The enum value 'Number.TWO' is deprecated. Some enum reason.")
    }

    "report usage of deprecated enums in variables" in {
      DocumentAnalyzer(schema, gql"""query Foo($$x: Number = TWO) { normalField }""").deprecatedUsages.map(_.errorMessage) should
        contain("The enum value 'Number.TWO' is deprecated. Some enum reason.")
    }

    "report introspectionUsage" in {
      val query =
        gql"""
          {
            __type(name: "Query") {
              name
            }

            __schema {
              queryType {
                name
              }
            }

            normalField

            __typeName

          }
        """

      DocumentAnalyzer(schema, query).introspectionUsages.map(_.errorMessage) should (
          contain("Introspection field '__Schema.queryType' is used.") and
          contain("Introspection field 'Query.__type' is used.") and
          contain("Introspection field 'Query.__schema' is used.") and
          contain("Introspection field '__Type.name' is used."))
    }
  }
}
