package sangria.schema

import language.existentials

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.Document
import sangria.schema.SchemaChange._
import sangria.macros._

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

    "should detect changes in directives" in checkChanges(
      graphql"""
        directive @foo(a: String, b: Int!) on FIELD_DEFINITION | ENUM
        directive @bar on FIELD_DEFINITION
      """,

      graphql"""
        # This is foo
        directive @foo(
          # first arg
          a: String,
          c: Int) on FIELD_DEFINITION | INPUT_OBJECT

        # This is baz
        directive @baz on FIELD_DEFINITION
      """,

      breakingChange[DirectiveRemoved]("`bar` directive was removed"),
      nonBreakingChange[DirectiveAdded]("`baz` directive was added"),
      nonBreakingChange[DirectiveDescriptionChanged]("`foo` directive description is changed"),
      nonBreakingChange[DirectiveArgumentDescriptionChanged]("`foo(a)` description is changed"),
      breakingChange[DirectiveArgumentRemoved]("Argument `b` was removed from `foo` directive"),
      nonBreakingChange[DirectiveLocationAdded]("`InputObject` directive location added to `foo` directive"),
      breakingChange[DirectiveLocationRemoved]("`Enum` directive location removed from `foo` directive"),
      nonBreakingChange[DirectiveArgumentAdded]("Argument `c` was added to `foo` directive"))

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

    "detect changes in object like type fields and interfaces when they are added or removed" in checkChanges(
      graphql"""
        interface I1 {
          name: String!
        }

        interface I2 {
          descr: String
        }

        type Filter implements I1, I2 {
          name: String!
          descr: String
          foo: [Int]
        }
      """,

      graphql"""
        interface I1 {
          bar: Int
        }

        interface I3 {
          descr: String
          id: ID
        }

        type Filter implements I1, I3 {
          bar: Int
          descr: String
          id: ID
        }
      """,

      breakingChange[TypeRemoved]("`I2` type was removed"),
      nonBreakingChange[TypeAdded]("`I3` type was added"),
      breakingChange[ObjectTypeInterfaceRemoved]("`Filter` object type no longer implements `I2` interface"),
      nonBreakingChange[ObjectTypeInterfaceAdded]("`Filter` object type now implements `I3` interface"),
      breakingChange[FieldRemoved]("Field `name` was removed from `Filter` type"),
      breakingChange[FieldRemoved]("Field `foo` was removed from `Filter` type"),
      nonBreakingChange[FieldAdded]("Field `id` was added to `Filter` type"),
      nonBreakingChange[FieldAdded]("Field `bar` was added to `Filter` type"),
      nonBreakingChange[FieldAdded]("Field `bar` was added to `I1` type"),
      breakingChange[FieldRemoved]("Field `name` was removed from `I1` type"))

    "detect changes in object type arguments" in checkChanges(
      graphql"""
        type Filter {
          foo(
            a: String!
            b: String
            b1: String
            c: [String]
          ): String!
        }
      """,

      graphql"""
        type Filter {
          foo(
            # descr
            a: String = "foo"
            b: [String]
            b1: String!
            c: [String]!
            d: Int
            e: Int!
          ): String!
        }
      """,

      breakingChange[ObjectTypeArgumentTypeChanged]("`Filter.foo(b)` type changed from `String` to `[String]`"),
      nonBreakingChange[ObjectTypeArgumentAdded]("Argument `d` was added to `Filter.foo` field"),
      breakingChange[ObjectTypeArgumentTypeChanged]("`Filter.foo(b1)` type changed from `String` to `String!`"),
      nonBreakingChange[ObjectTypeArgumentTypeChanged]("`Filter.foo(a)` type changed from `String!` to `String`"),
      breakingChange[ObjectTypeArgumentTypeChanged]("`Filter.foo(c)` type changed from `[String]` to `[String]!`"),
      breakingChange[ObjectTypeArgumentAdded]("Argument `e` was added to `Filter.foo` field"),
      nonBreakingChange[ObjectTypeArgumentDefaultChanged]("`Filter.foo(a)` default value changed from none to `\"foo\"`"),
      nonBreakingChange[ObjectTypeArgumentDescriptionChanged]("`Filter.foo(a)` description is changed"))

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

    "detect changes in schema definition" in checkChangesWithoutQueryType(
      graphql"""
        type Query {
          foo: String
        }
      """,

      graphql"""
        type Foo {
          foo: String
        }

        type Mut {
          bar: Int!
        }

        type Subs {
          bar: Int!
        }

        schema {
          query: Foo
          mutation: Mut
          subscription: Subs
        }
      """,

      breakingChange[TypeRemoved]("`Query` type was removed"),
      nonBreakingChange[TypeAdded]("`Mut` type was added"),
      nonBreakingChange[TypeAdded]("`Subs` type was added"),
      nonBreakingChange[TypeAdded]("`Foo` type was added"),
      breakingChange[SchemaQueryTypeChanged]("Schema query type changed from `Query` to `Foo` type"),
      nonBreakingChange[SchemaMutationTypeChanged]("Schema mutation type changed from none to `Mut` type"),
      nonBreakingChange[SchemaSubscriptionTypeChanged]("Schema subscription type changed from none to `Subs` type"))

    "detect breaking changes in schema definition" in checkChangesWithoutQueryType(
      graphql"""
        type Query {
          foo: String
        }
        type Mut {
          bar: Int!
        }

        type Subs {
          bar: Int!
        }

        schema {
          query: Query
          mutation: Mut
          subscription: Subs
        }
      """,

      graphql"""
        type Query {
          foo: String
        }

        type Subs1 {
          bar: Int!
        }

        schema {
          query: Query
          subscription: Subs1
        }
      """,

      breakingChange[TypeRemoved]("`Mut` type was removed"),
      breakingChange[TypeRemoved]("`Subs` type was removed"),
      nonBreakingChange[TypeAdded]("`Subs1` type was added"),
      breakingChange[SchemaMutationTypeChanged]("Schema mutation type changed from `Mut` to none type"),
      breakingChange[SchemaSubscriptionTypeChanged]("Schema subscription type changed from `Subs` to `Subs1` type"))
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

  def checkChangesWithoutQueryType(oldDoc: Document, newDoc: Document, expectedChanges: (Class[_], String, Boolean)*) = {
    val oldSchema = Schema.buildFromAst(oldDoc)
    val newSchema = Schema.buildFromAst(newDoc)

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
