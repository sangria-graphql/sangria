package sangria.validation

import sangria.schema._
import sangria.macros._
import sangria.util.StringMatchers
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class DocumentAnalyzerSpec extends AnyWordSpec with Matchers with StringMatchers {
  val NumberType = EnumType(
    "Number",
    values = List(
      EnumValue("ONE", value = 1),
      EnumValue("TWO", value = 2, deprecationReason = Some("Some enum reason."))))

  val QueryType = ObjectType(
    "Query",
    fields[Unit, Unit](
      Field(
        "normalField",
        OptionType(NumberType),
        arguments = Argument("enumArg", OptionInputType(NumberType)) :: Nil,
        resolve = ctx => ctx.argOpt[Int]("enumArg")),
      Field(
        "deprecatedField",
        OptionType(StringType),
        deprecationReason = Some("Some field reason."),
        resolve = _ => "foo")
    )
  )

  val schema = Schema(QueryType)

  "DocumentAnalyzer" should {
    "report empty set for no deprecated usages" in {
      schema.analyzer(gql"""{ normalField(enumArg: ONE) }""").deprecatedUsages should have size 0
    }

    "report usage of deprecated fields" in {
      schema
        .analyzer(gql"""{ normalField, deprecatedField }""")
        .deprecatedUsages
        .map(_.errorMessage) should
        contain("The field 'Query.deprecatedField' is deprecated. Some field reason.")
    }

    "report usage of deprecated enums" in {
      schema
        .analyzer(gql"""{ normalField(enumArg: TWO) }""")
        .deprecatedUsages
        .map(_.errorMessage) should
        contain("The enum value 'Number.TWO' is deprecated. Some enum reason.")
    }

    "report usage of deprecated enums in variables" in {
      schema
        .analyzer(gql"""query Foo($$x: Number = TWO) { normalField }""")
        .deprecatedUsages
        .map(_.errorMessage) should
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

      schema.analyzer(query).introspectionUsages.map(_.errorMessage) should (contain(
        "Introspection field '__Schema.queryType' is used.")
        .and(contain("Introspection field 'Query.__type' is used."))
        .and(contain("Introspection field 'Query.__schema' is used."))
        .and(contain("Introspection field '__Type.name' is used.")))
    }

    "separates one AST into multiple, maintaining document order" in {
      val query =
        gql"""
          {
            ...Y
            ...X
          }

          query One {
            foo
            bar
            ...A
            ...X
          }

          fragment A on T {
            field
            ...B
          }

          fragment X on T {
            fieldX
          }

          query Two {
            ...A
            ...Y
            baz
          }

          fragment Y on T {
            fieldY
          }

          fragment B on T {
            something
          }
        """

      query.separateOperations.keySet should be(Set(None, Some("One"), Some("Two")))

      query.separateOperations(None).renderPretty should equal("""{
          |  ...Y
          |  ...X
          |}
          |
          |fragment X on T {
          |  fieldX
          |}
          |
          |fragment Y on T {
          |  fieldY
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))

      query.separateOperations(Some("One")).renderPretty should equal("""query One {
          |  foo
          |  bar
          |  ...A
          |  ...X
          |}
          |
          |fragment A on T {
          |  field
          |  ...B
          |}
          |
          |fragment X on T {
          |  fieldX
          |}
          |
          |fragment B on T {
          |  something
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))

      query.separateOperations(Some("Two")).renderPretty should equal("""fragment A on T {
          |  field
          |  ...B
          |}
          |
          |query Two {
          |  ...A
          |  ...Y
          |  baz
          |}
          |
          |fragment Y on T {
          |  fieldY
          |}
          |
          |fragment B on T {
          |  something
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))
    }

    "ASt separation survives circular dependencies" in {
      val query =
        gql"""
          query One {
            ...A
          }

          fragment A on T {
            ...B
          }

          fragment B on T {
            ...A
          }

          query Two {
            ...B
          }
        """

      query.separateOperations.keySet should be(Set(Some("One"), Some("Two")))

      query.separateOperations(Some("One")).renderPretty should equal("""query One {
          |  ...A
          |}
          |
          |fragment A on T {
          |  ...B
          |}
          |
          |fragment B on T {
          |  ...A
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))

      query.separateOperations(Some("Two")).renderPretty should equal("""fragment A on T {
          |  ...B
          |}
          |
          |fragment B on T {
          |  ...A
          |}
          |
          |query Two {
          |  ...B
          |}""".stripMargin)(after.being(strippedOfCarriageReturns))
    }
  }
}
