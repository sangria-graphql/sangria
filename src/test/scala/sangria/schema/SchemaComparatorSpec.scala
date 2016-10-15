package sangria.schema

import language.existentials

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.Document
import sangria.schema.SchemaChange._
import sangria.macros._
import sangria.util.DebugUtil

import scala.reflect.ClassTag

class SchemaComparatorSpec extends WordSpec with Matchers {
  "SchemaComparator" should {
    val QueryType = ObjectType("Query", fields[Unit, Unit](
      Field("field1", OptionType(StringType), resolve = _ ⇒ "foo")))

    "should detect if a type was removed or added" in {
      val type1 = ObjectType("Type1", fields[Unit, Unit](
        Field("field1", OptionType(StringType), resolve = _ ⇒ "foo")))
      val type2 = ObjectType("Type2", fields[Unit, Unit](
        Field("field1", OptionType(StringType), resolve = _ ⇒ "foo")))

      val oldSchema = Schema(QueryType, additionalTypes = type1 :: type2 :: Nil)
      val newSchema = Schema(QueryType, additionalTypes = type2 :: Nil)

      assertChanges(newSchema.compare(oldSchema),
        breakingChange[TypeRemoved]("`Type1` type was removed"))

      assertChanges(oldSchema.compare(newSchema),
        nonBreakingChange[TypeAdded]("`Type1` type was added"))

      oldSchema.compare(oldSchema) should be (Vector.empty)
    }

    "should detect if a type changed its kind" in checkChanges(
      graphql"""
        interface Type1 {field1: String}
      """,

      graphql"""
        type ObjectType {field1: String}
        union Type1 = ObjectType
      """,

      nonBreakingChange[TypeAdded]("`ObjectType` type was added"),
      breakingChange[TypeKindChanged]("`Type1` changed from an Interface type to a Union type"))

    "detect if a type description changed " in checkChanges(
      graphql"""
        # normal type
        type ObjectType {field1: String}
      """,

      graphql"""
        # Cool type
        type ObjectType {field1: String}
      """,

      nonBreakingChange[TypeDescriptionChanged]("`ObjectType` type description is changed"))

    "should detect changes in enum values" in checkChanges(
      graphql"""
        enum Foo {
          A, B, C
        }
      """,

      graphql"""
        enum Foo {
          B @deprecated(reason: "Should not be used anymore")

          # The `B`
          # value!
          C, D
        }
      """,

      breakingChange[EnumValueRemoved]("Enum value `A` was removed from enum `Foo`"),
      nonBreakingChange[EnumValueAdded]("Enum value `D` was added to enum `Foo`"),
      nonBreakingChange[EnumValueDescriptionChanged]("`Foo.C` description changed"),
      nonBreakingChange[EnumValueDeprecated]("Enum value `B` was deprecated in enum `Foo`"))

    "should detect changes in unions" in checkChanges(
      graphql"""
        type Foo {f: String}
        type Bar {descr: String}
        union Agg = Foo | Bar
      """,

      graphql"""
        type Bar {descr: String}
        type Baz {descr: String}

        # Hello
        union Agg = Bar | Baz
      """,

      nonBreakingChange[TypeAdded]("`Baz` type was added"),
      breakingChange[TypeRemoved]("`Foo` type was removed"),
      breakingChange[UnionMemberRemoved]("`Foo` type was removed from union `Agg`"),
      nonBreakingChange[UnionMemberAdded]("`Baz` type was added to union `Agg`"),
      nonBreakingChange[TypeDescriptionChanged]("`Agg` type description is changed"))

    "should detect changes in scalars" in checkChanges(
      graphql"""
        scalar Date
        scalar Locale
      """,

      graphql"""
        # This is locale
        scalar Locale

        # This is country
        scalar Country
      """,

      breakingChange[TypeRemoved]("`Date` type was removed"),
      nonBreakingChange[TypeAdded]("`Country` type was added"),
      nonBreakingChange[TypeDescriptionChanged]("`Locale` type description is changed"))

    "should detect changes in input types" in checkChanges(
      graphql"""
        input Sort {dir: Int}
        input Bar {size: Int}
      """,

      graphql"""
        # This is sort
        input Sort {dir: Int}

        # This is foo
        input Foo {size: Int}
      """,

      breakingChange[TypeRemoved]("`Bar` type was removed"),
      nonBreakingChange[TypeAdded]("`Foo` type was added"),
      nonBreakingChange[TypeDescriptionChanged]("`Sort` type description is changed"))

    "detect changes in input type fields when they are added or removed" in checkChanges(
      graphql"""
        input Filter {
          name: String!
          descr: String
        }
      """,

      graphql"""
        # search filter
        input Filter {
          # filter by name
          name: String!

          # filter by size
          size: Int
        }
      """,

      breakingChange[InputFieldRemoved]("Input field `descr` was removed from `Filter` type"),
      nonBreakingChange[InputFieldAdded]("Input field `size` was added to `Filter` type"),
      nonBreakingChange[InputFieldDescriptionChanged]("`Filter.name` description is changed"),
      nonBreakingChange[TypeDescriptionChanged]("`Filter` type description is changed"))

    "detect changes in input type fields default value changes" in checkChanges(
      graphql"""
        input Filter {
          a: [String!] = ["hello", "world"]
          size: Int
          color: Int = 5
          type: [Int] = [1, 2, 3]
        }
      """,

      graphql"""
        input Filter {
          a: [String!] = ["foo"]
          size: Int = 12
          color: String = "red"
          type: [Int!] = [1, 2, 3]
        }
      """,

      breakingChange[InputFieldTypeChanged]("""`Filter.color` input field type changed from `Int` to `String`"""),
      breakingChange[InputFieldTypeChanged]("""`Filter.type` input field type changed from `[Int]` to `[Int!]`"""),
      nonBreakingChange[InputFieldDefaultChanged]("""`Filter.a` default value changed from `["hello","world"]` to `["foo"]`"""),
      nonBreakingChange[InputFieldDefaultChanged]("`Filter.size` default value changed from none to `12`"),
      nonBreakingChange[InputFieldDefaultChanged]("""`Filter.color` default value changed from `5` to `"red"`"""))

    "detect breaking and non-breaking changes in input type fields type" in checkChanges(
      graphql"""
        input Filter {
          a: String!
          b: String
          b1: String
          c: [String]
        }
      """,

      graphql"""
        input Filter {
          a: String
          b: [String]
          b1: String!
          c: [String]!
          d: Int
          e: Int!
        }
      """,

      breakingChange[InputFieldAdded]("Input field `e` was added to `Filter` type"),
      nonBreakingChange[InputFieldAdded]("Input field `d` was added to `Filter` type"),
      breakingChange[InputFieldTypeChanged]("`Filter.b` input field type changed from `String` to `[String]`"),
      nonBreakingChange[InputFieldTypeChanged]("`Filter.a` input field type changed from `String!` to `String`"),
      breakingChange[InputFieldTypeChanged]("`Filter.c` input field type changed from `[String]` to `[String]!`"),
      breakingChange[InputFieldTypeChanged]("`Filter.b1` input field type changed from `String` to `String!`"))
  }

  def breakingChange[T : ClassTag](description: String) =
    (implicitly[ClassTag[T]].runtimeClass, description, true)

  def nonBreakingChange[T : ClassTag](description: String) =
    (implicitly[ClassTag[T]].runtimeClass, description, false)

  def checkChanges(oldDoc: Document, newDoc: Document, expectedChanges: (Class[_], String, Boolean)*) = {
    val queryType =
      graphql"""
        type Query {
          field1: String
        }
      """

    val oldSchema = Schema.buildFromAst(oldDoc merge queryType)
    val newSchema = Schema.buildFromAst(newDoc merge queryType)

    assertChanges(newSchema.compare(oldSchema), expectedChanges: _*)
  }

  def assertChanges(actualChanges: Vector[SchemaChange], expectedChanges: (Class[_], String, Boolean)*) = {
    val actualRendered = actualChanges.map(c ⇒ s"  * ${c.getClass.getSimpleName}: ${c.description}${if (c.breakingChange) " (breaking)" else ""}").mkString("\n")

    withClue(s"Actual changes:\n$actualRendered\n") {
      actualChanges should have size expectedChanges.size

      val notFound = expectedChanges.filter(expectedChange ⇒
        !actualChanges.exists(ac ⇒ expectedChange._1.isAssignableFrom(ac.getClass) && ac.description == expectedChange._2 && ac.breakingChange == expectedChange._3))

      if (notFound.nonEmpty) {
        val str = notFound.map(nf ⇒ s"  * ${nf._1.getSimpleName}: ${nf._2}${if (nf._3) " (breaking)" else ""}").mkString("\n")

        fail(s"Changes not found:\n $str")
      }
    }
  }
}
