package sangria.schema

import language.existentials

import sangria.ast.Document
import sangria.schema.SchemaChange._
import sangria.macros._

import scala.reflect.ClassTag
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec

class SchemaComparatorSpec extends AnyWordSpec with Matchers {
  "SchemaComparator" should {
    val QueryType = ObjectType(
      "Query",
      fields[Unit, Unit](Field("field1", OptionType(StringType), resolve = _ => "foo")))

    "should detect if a type was removed or added" in {
      val type1 = ObjectType(
        "Type1",
        fields[Unit, Unit](Field("field1", OptionType(StringType), resolve = _ => "foo")))
      val type2 = ObjectType(
        "Type2",
        fields[Unit, Unit](Field("field1", OptionType(StringType), resolve = _ => "foo")))

      val oldSchema = Schema(QueryType, additionalTypes = type1 :: type2 :: Nil)
      val newSchema = Schema(QueryType, additionalTypes = type2 :: Nil)

      assertChanges(
        newSchema.compare(oldSchema),
        breakingChange[TypeRemoved]("`Type1` type was removed"))

      assertChanges(
        oldSchema.compare(newSchema),
        nonBreakingChange[TypeAdded]("`Type1` type was added"))

      oldSchema.compare(oldSchema) should be(Vector.empty)
    }

    "should detect if a type changed its kind" in checkChanges(
      graphql"""
        interface Type1 {field1: String}
        type Foo implements Type1 {
          test: String
        }
      """,
      graphql"""
        type ObjectType {field1: String}
        union Type1 = ObjectType
        type Foo {
          test: String
        }
      """,
      nonBreakingChange[TypeAdded]("`ObjectType` type was added"),
      breakingChange[TypeKindChanged]("`Type1` changed from an Interface type to a Union type"),
      breakingChange[ObjectTypeInterfaceRemoved](
        "`Foo` object type no longer implements `Type1` interface"),
      breakingChange[FieldRemoved]("Field `field1` was removed from `Foo` type")
    )

    "detect if a type description changed " in checkChanges(
      graphql"""
        "normal type"
        type ObjectType {field1: String}
      """,
      graphql"""
        "Cool type"
        type ObjectType {field1: String}
      """,
      nonBreakingChange[TypeDescriptionChanged]("`ObjectType` type description is changed")
    )

    "should detect changes in enum values" in checkChanges(
      graphql"""
        enum Foo {
          A, B, C
        }
      """,
      graphql"""
        enum Foo {
          B @deprecated(reason: "Should not be used anymore")

          "The `B`\nvalue!"
          C, D
        }
      """,
      breakingChange[EnumValueRemoved]("Enum value `A` was removed from enum `Foo`"),
      nonBreakingChange[EnumValueAdded]("Enum value `D` was added to enum `Foo`"),
      nonBreakingChange[EnumValueDescriptionChanged]("`Foo.C` description changed"),
      nonBreakingChange[EnumValueDeprecated]("Enum value `B` was deprecated in enum `Foo`"),
      nonBreakingChange[EnumValueAstDirectiveAdded](
        "Directive `@deprecated(reason:\"Should not be used anymore\")` added on an enum value `Foo.B`")
    )

    "should detect changes in unions" in checkChanges(
      graphql"""
        type Foo {f: String}
        type Bar {descr: String}
        union Agg = Foo | Bar
      """,
      graphql"""
        type Bar {descr: String}
        type Baz {descr: String}

        "Hello"
        union Agg = Bar | Baz
      """,
      nonBreakingChange[TypeAdded]("`Baz` type was added"),
      breakingChange[TypeRemoved]("`Foo` type was removed"),
      breakingChange[UnionMemberRemoved]("`Foo` type was removed from union `Agg`"),
      nonBreakingChange[UnionMemberAdded]("`Baz` type was added to union `Agg`"),
      nonBreakingChange[TypeDescriptionChanged]("`Agg` type description is changed")
    )

    "should detect changes in scalars" in checkChanges(
      graphql"""
        scalar Date
        scalar Locale
      """,
      graphql"""
        "This is locale"
        scalar Locale

        "This is country"
        scalar Country
      """,
      breakingChange[TypeRemoved]("`Date` type was removed"),
      nonBreakingChange[TypeAdded]("`Country` type was added"),
      nonBreakingChange[TypeDescriptionChanged]("`Locale` type description is changed")
    )

    "should detect changes in directives" in checkChanges(
      graphql"""
        directive @foo(a: String, b: Int!) on FIELD_DEFINITION | ENUM
        directive @bar on FIELD_DEFINITION
      """,
      graphql"""
        "This is foo"
        directive @foo(
          "first arg"
          a: String,
          c: Int) on FIELD_DEFINITION | INPUT_OBJECT

        "This is baz"
        directive @baz on FIELD_DEFINITION
      """,
      breakingChange[DirectiveRemoved]("`bar` directive was removed"),
      nonBreakingChange[DirectiveAdded]("`baz` directive was added"),
      nonBreakingChange[DirectiveDescriptionChanged]("`foo` directive description is changed"),
      nonBreakingChange[DirectiveArgumentDescriptionChanged]("`foo(a)` description is changed"),
      breakingChange[DirectiveArgumentRemoved]("Argument `b` was removed from `foo` directive"),
      nonBreakingChange[DirectiveLocationAdded](
        "`InputObject` directive location added to `foo` directive"),
      breakingChange[DirectiveLocationRemoved](
        "`Enum` directive location removed from `foo` directive"),
      nonBreakingChange[DirectiveArgumentAdded]("Argument `c` was added to `foo` directive")
    )

    "detect changes repeatable directive definition" in checkChanges(
      gql"""
        directive @a repeatable on OBJECT
        directive @b(foo: Int) on OBJECT
      """,
      gql"""
        directive @a on OBJECT
        directive @b(foo: Int) repeatable on OBJECT
      """,
      breakingChange[DirectiveRepeatableChanged]("Directive `a` was made unique per location"),
      nonBreakingChange[DirectiveRepeatableChanged](
        "Directive `b` was made repeatable per location")
    )

    "should detect changes in input types" in checkChanges(
      graphql"""
        input Sort {dir: Int}
        input Bar {size: Int}
      """,
      graphql"""
        "This is sort"
        input Sort {dir: Int}

        "This is foo"
        input Foo {size: Int}
      """,
      breakingChange[TypeRemoved]("`Bar` type was removed"),
      nonBreakingChange[TypeAdded]("`Foo` type was added"),
      nonBreakingChange[TypeDescriptionChanged]("`Sort` type description is changed")
    )

    "detect changes in input type fields when they are added or removed" in checkChanges(
      graphql"""
        input Filter {
          name: String!
          descr: String
          foo: String
        }
      """,
      graphql"""
        "search filter"
        input Filter {
          "filter by name"
          name: String!

          "filter by size"
          size: Int

          foo: String @deprecated
        }
      """,
      nonBreakingChange[TypeAdded]("`Int` type was added"),
      breakingChange[InputFieldRemoved]("Input field `descr` was removed from `Filter` type"),
      nonBreakingChange[InputFieldAdded]("Input field `size` was added to `Filter` type"),
      nonBreakingChange[InputFieldDescriptionChanged]("`Filter.name` description is changed"),
      nonBreakingChange[TypeDescriptionChanged]("`Filter` type description is changed"),
      nonBreakingChange[InputFieldDeprecated]("`Filter.foo` was deprecated"),
      nonBreakingChange[InputFieldAstDirectiveAdded](
        "Directive `@deprecated` added on an input field `Filter.foo`")
    )

    "detect changes in object like type fields and interfaces when they are added or removed" in checkChanges(
      graphql"""
        interface I1 {
          name: String!
        }

        interface I2 {
          descr: String
        }

        type Filter implements I1 & I2 {
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

        type Filter implements I1 & I3 {
          bar: Int
          descr: String
          id: ID
        }
      """,
      breakingChange[TypeRemoved]("`I2` type was removed"),
      nonBreakingChange[TypeAdded]("`ID` type was added"),
      nonBreakingChange[TypeAdded]("`I3` type was added"),
      breakingChange[ObjectTypeInterfaceRemoved](
        "`Filter` object type no longer implements `I2` interface"),
      nonBreakingChange[ObjectTypeInterfaceAdded](
        "`Filter` object type now implements `I3` interface"),
      breakingChange[FieldRemoved]("Field `name` was removed from `Filter` type"),
      breakingChange[FieldRemoved]("Field `foo` was removed from `Filter` type"),
      nonBreakingChange[FieldAdded]("Field `id` was added to `Filter` type"),
      nonBreakingChange[FieldAdded]("Field `bar` was added to `Filter` type"),
      nonBreakingChange[FieldAdded]("Field `bar` was added to `I1` type"),
      breakingChange[FieldRemoved]("Field `name` was removed from `I1` type")
    )

    "detect changes in object type arguments" in checkChanges(
      graphql"""
        type Filter {
          foo(
            a: String!
            b: String
            b1: String
            c: [String]
            c1: String
          ): String!
        }
      """,
      graphql"""
        type Filter {
          foo(
            "descr"
            a: String = "foo"
            b: [String]
            b1: String!
            c: [String]!
            c1: String @deprecated
            d: Int
            e: Int!
            f: Int! = 42
          ): String!
        }
      """,
      nonBreakingChange[TypeAdded]("`Int` type was added"),
      breakingChange[ObjectTypeArgumentTypeChanged](
        "`Filter.foo(b)` type changed from `String` to `[String]`"),
      nonBreakingChange[ObjectTypeArgumentAdded]("Argument `d` was added to `Filter.foo` field"),
      breakingChange[ObjectTypeArgumentTypeChanged](
        "`Filter.foo(b1)` type changed from `String` to `String!`"),
      nonBreakingChange[ObjectTypeArgumentTypeChanged](
        "`Filter.foo(a)` type changed from `String!` to `String`"),
      breakingChange[ObjectTypeArgumentTypeChanged](
        "`Filter.foo(c)` type changed from `[String]` to `[String]!`"),
      breakingChange[ObjectTypeArgumentAdded]("Argument `e` was added to `Filter.foo` field"),
      nonBreakingChange[ObjectTypeArgumentAdded]("Argument `f` was added to `Filter.foo` field"),
      nonBreakingChange[ObjectTypeArgumentDefaultChanged](
        "`Filter.foo(a)` default value changed from none to `\"foo\"`"),
      nonBreakingChange[ObjectTypeArgumentDescriptionChanged](
        "`Filter.foo(a)` description is changed"),
      nonBreakingChange[ObjectTypeArgumentDeprecated](
        "Argument `c1` on `Filter.foo` was deprecated"),
      nonBreakingChange[FieldArgumentAstDirectiveAdded](
        "Directive `@deprecated` added on a field argument `Filter.foo[c1]`")
    )

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
      breakingChange[InputFieldTypeChanged](
        """`Filter.color` input field type changed from `Int` to `String`"""),
      breakingChange[InputFieldTypeChanged](
        """`Filter.type` input field type changed from `[Int]` to `[Int!]`"""),
      nonBreakingChange[InputFieldDefaultChanged](
        """`Filter.a` default value changed from `["hello","world"]` to `["foo"]`"""),
      nonBreakingChange[InputFieldDefaultChanged](
        "`Filter.size` default value changed from none to `12`"),
      nonBreakingChange[InputFieldDefaultChanged](
        """`Filter.color` default value changed from `5` to `"red"`""")
    )

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
          f: Boolean! = true
        }
      """,
      nonBreakingChange[TypeAdded]("`Int` type was added"),
      breakingChange[InputFieldAdded]("Input field `e` was added to `Filter` type"),
      nonBreakingChange[InputFieldAdded]("Input field `f` was added to `Filter` type"),
      nonBreakingChange[InputFieldAdded]("Input field `d` was added to `Filter` type"),
      breakingChange[InputFieldTypeChanged](
        "`Filter.b` input field type changed from `String` to `[String]`"),
      nonBreakingChange[InputFieldTypeChanged](
        "`Filter.a` input field type changed from `String!` to `String`"),
      breakingChange[InputFieldTypeChanged](
        "`Filter.c` input field type changed from `[String]` to `[String]!`"),
      breakingChange[InputFieldTypeChanged](
        "`Filter.b1` input field type changed from `String` to `String!`")
    )

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
      nonBreakingChange[TypeAdded]("`Int` type was added"),
      nonBreakingChange[TypeAdded]("`Subs` type was added"),
      nonBreakingChange[TypeAdded]("`Foo` type was added"),
      breakingChange[SchemaQueryTypeChanged](
        "Schema query type changed from `Query` to `Foo` type"),
      nonBreakingChange[SchemaMutationTypeChanged](
        "Schema mutation type changed from none to `Mut` type"),
      nonBreakingChange[SchemaSubscriptionTypeChanged](
        "Schema subscription type changed from none to `Subs` type")
    )

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
      breakingChange[SchemaMutationTypeChanged](
        "Schema mutation type changed from `Mut` to none type"),
      breakingChange[SchemaSubscriptionTypeChanged](
        "Schema subscription type changed from `Subs` to `Subs1` type")
    )

    "detect changes in field AST directives" in checkChangesWithoutQueryType(
      gql"""
        type Query {
          foo: String @foo
        }
      """,
      gql"""
        type Query {
          foo: String @bar(ids: [1, 2])
        }
      """,
      nonBreakingChange[FieldAstDirectiveAdded](
        "Directive `@bar(ids:[1,2])` added on a field `Query.foo`"),
      nonBreakingChange[FieldAstDirectiveRemoved](
        "Directive `@foo` removed from a field `Query.foo`")
    )

    "detect changes in argument AST directives" in checkChangesWithoutQueryType(
      gql"""
        type Query {
          foo(bar: String @foo): String
        }

        directive @test(bar: String @hello foo: String) on FIELD
      """,
      gql"""
        type Query {
          foo(bar: String @bar(ids: [1, 2])): String
        }

        directive @test(bar: String @world foo: String @deprecated) on FIELD
      """,
      nonBreakingChange[FieldArgumentAstDirectiveAdded](
        "Directive `@bar(ids:[1,2])` added on a field argument `Query.foo[bar]`"),
      nonBreakingChange[FieldArgumentAstDirectiveRemoved](
        "Directive `@foo` removed from a field argument `Query.foo[bar]`"),
      nonBreakingChange[DirectiveArgumentAstDirectiveRemoved](
        "Directive `@hello` removed from a directive argument `test.bar`"),
      nonBreakingChange[DirectiveArgumentAstDirectiveAdded](
        "Directive `@world` added on a directive argument `test.bar`"),
      nonBreakingChange[DirectiveArgumentAstDirectiveAdded](
        "Directive `@deprecated` added on a directive argument `test.foo`"),
      nonBreakingChange[DirectiveArgumentDeprecated]("Directive argument `test.foo` was deprecated")
    )

    "detect changes in input field AST directives" in checkChangesWithoutQueryType(
      gql"""
        type Query {a: Int}

        input Foo {
          foo: String  = "test" @foo @baz(s: "string")
        }
      """,
      gql"""
        type Query {a: Int}

        input Foo {
          foo: String @baz(s: "string") @bar(ids: [1, 2])
        }
      """,
      nonBreakingChange[InputFieldAstDirectiveAdded](
        "Directive `@bar(ids:[1,2])` added on an input field `Foo.foo`"),
      nonBreakingChange[InputFieldAstDirectiveRemoved](
        "Directive `@foo` removed from a input field `Foo.foo`"),
      nonBreakingChange[InputFieldDefaultChanged](
        "`Foo.foo` default value changed from `\"test\"` to none")
    )

    "detect changes in enum values AST directives" in checkChangesWithoutQueryType(
      gql"""
        type Query {a: Int}

        enum Foo {
          A @foo
        }
      """,
      gql"""
        type Query {a: Int}

        enum Foo {
          A @bar(ids: [1, 2])
        }
      """,
      nonBreakingChange[EnumValueAstDirectiveAdded](
        "Directive `@bar(ids:[1,2])` added on an enum value `Foo.A`"),
      nonBreakingChange[EnumValueAstDirectiveRemoved](
        "Directive `@foo` removed from a enum value `Foo.A`")
    )

    "detect changes in schema AST directives" in checkChangesWithoutQueryType(
      gql"""
        type Query {
          foo: String
        }

        schema @foo {
          query: Query
        }
      """,
      gql"""
        type Query {
          foo: String
        }

        schema @bar(ids: [1, 2]) {
          query: Query
        }
      """,
      nonBreakingChange[SchemaAstDirectiveAdded]("Directive `@bar(ids:[1,2])` added on a schema"),
      nonBreakingChange[SchemaAstDirectiveRemoved]("Directive `@foo` removed from a schema")
    )

    "detect changes in schema description" in checkChangesWithoutQueryType(
      gql"""
        type Query {
          foo: String
        }

        schema {
          query: Query
        }
      """,
      gql"""
        type Query {
          foo: String
        }

        "new description"
        schema {
          query: Query
        }
      """,
      nonBreakingChange[SchemaDescriptionChanged]("Schema description changed")
    )

    "detect changes in type AST directives" in checkChangesWithoutQueryType(
      gql"""
        type Query implements Foo2 {
          foo: String
          a: Int
        }

        input Foo @bar(ids: [1, 2]) {
          a: Int
        }

        type Foo1 @bar(ids: [1, 2]) {
          a: Int
        }

        interface Foo2 @bar(ids: [1, 2]) {
          a: Int
        }

        union Foo3 @bar(ids: [1, 2]) = Query | Foo1

        enum Foo4 @bar(ids: [1, 2]) {
          A B C
        }

        scalar Foo5 @bar(ids: [1, 2])
      """,
      gql"""
        type Query implements Foo2 {
          foo: String
          a: Int
        }

        input Foo @bar(ids: [1]) {
          a: Int
        }

        type Foo1 @baz {
          a: Int
        }

        interface Foo2 @baz {
          a: Int
        }

        union Foo3 @bar(id: 1) = Query | Foo1

        enum Foo4 @bar(id: 1) {
          A B C
        }

        scalar Foo5 @bar(ids: [1])
      """,
      nonBreakingChange[InputObjectTypeAstDirectiveAdded](
        "Directive `@bar(ids:[1])` added on an input type `Foo`"),
      nonBreakingChange[InputObjectTypeAstDirectiveRemoved](
        "Directive `@bar(ids:[1,2])` removed from an input type `Foo`"),
      nonBreakingChange[ObjectTypeAstDirectiveAdded](
        "Directive `@baz` added on an object type `Foo1`"),
      nonBreakingChange[ObjectTypeAstDirectiveRemoved](
        "Directive `@bar(ids:[1,2])` removed from an object type `Foo1`"),
      nonBreakingChange[InterfaceTypeAstDirectiveAdded](
        "Directive `@baz` added on an interface type `Foo2`"),
      nonBreakingChange[InterfaceTypeAstDirectiveRemoved](
        "Directive `@bar(ids:[1,2])` removed from an interface type `Foo2`"),
      nonBreakingChange[UnionTypeAstDirectiveAdded](
        "Directive `@bar(id:1)` added on a union type `Foo3`"),
      nonBreakingChange[UnionTypeAstDirectiveRemoved](
        "Directive `@bar(ids:[1,2])` removed from a union type `Foo3`"),
      nonBreakingChange[EnumTypeAstDirectiveAdded](
        "Directive `@bar(id:1)` added on an enum type `Foo4`"),
      nonBreakingChange[EnumTypeAstDirectiveRemoved](
        "Directive `@bar(ids:[1,2])` removed from an enum type `Foo4`"),
      nonBreakingChange[ScalarTypeAstDirectiveAdded](
        "Directive `@bar(ids:[1])` added on a scalar type `Foo5`"),
      nonBreakingChange[ScalarTypeAstDirectiveRemoved](
        "Directive `@bar(ids:[1,2])` removed from a scalar type `Foo5`")
    )

    "detect removal of @oneOf" in checkChangesWithoutQueryType(
      gql"""
        input UserBy @oneOf {
          id: ID
          email: String
          username: String
          registrationNumber: Int
        }
        type Query {
          user(by: UserBy!): String
        }
      """,
      gql"""
        input UserBy {
          id: ID
          email: String
          username: String
          registrationNumber: Int
        }
        type Query {
          user(by: UserBy!): String
        }
      """,
      nonBreakingChange[InputObjectTypeAstDirectiveRemoved](
        "Directive `@oneOf` removed from an input type `UserBy`")
    )

    "detect add of @oneOf" in checkChangesWithoutQueryType(
      gql"""
        input UserBy {
          id: ID
          email: String
          username: String
          registrationNumber: Int
        }
        type Query {
          user(by: UserBy!): String
        }
      """,
      gql"""
        input UserBy @oneOf {
          id: ID
          email: String
          username: String
          registrationNumber: Int
        }
        type Query {
          user(by: UserBy!): String
        }
      """,
      breakingChange[InputObjectTypeAstDirectiveAdded](
        "Directive `@oneOf` added on an input type `UserBy`")
    )
  }

  private[this] def breakingChange[T: ClassTag](description: String) =
    (implicitly[ClassTag[T]].runtimeClass, description, true)

  private[this] def nonBreakingChange[T: ClassTag](description: String) =
    (implicitly[ClassTag[T]].runtimeClass, description, false)

  private[this] def checkChanges(
      oldDoc: Document,
      newDoc: Document,
      expectedChanges: (Class[_], String, Boolean)*): Unit = {
    val queryType =
      graphql"""
        type Query {
          field1: String
        }
      """

    val oldSchema = Schema.buildFromAst(oldDoc.merge(queryType))
    val newSchema = Schema.buildFromAst(newDoc.merge(queryType))

    assertChanges(newSchema.compare(oldSchema), expectedChanges: _*)
  }

  private[this] def checkChangesWithoutQueryType(
      oldDoc: Document,
      newDoc: Document,
      expectedChanges: (Class[_], String, Boolean)*): Unit = {
    val oldSchema = Schema.buildFromAst(oldDoc)
    val newSchema = Schema.buildFromAst(newDoc)

    assertChanges(newSchema.compare(oldSchema), expectedChanges: _*)
  }

  private[this] def assertChanges(
      actualChanges: Vector[SchemaChange],
      expectedChanges: (Class[_], String, Boolean)*): Unit = {
    val actualRendered = actualChanges
      .map(c =>
        s"  * ${c.getClass.getSimpleName}: ${c.description}${if (c.breakingChange) " (breaking)"
          else ""}")
      .mkString("\n")

    withClue(s"Actual changes:\n$actualRendered\n") {
      actualChanges should have size expectedChanges.size

      val notFound = expectedChanges.filter(expectedChange =>
        !actualChanges.exists(ac =>
          expectedChange._1.isAssignableFrom(
            ac.getClass) && ac.description == expectedChange._2 && ac.breakingChange == expectedChange._3))

      if (notFound.nonEmpty) {
        val str = notFound
          .map(nf => s"  * ${nf._1.getSimpleName}: ${nf._2}${if (nf._3) " (breaking)" else ""}")
          .mkString("\n")

        fail(s"Changes not found:\n $str")
      }
    }
  }
}
