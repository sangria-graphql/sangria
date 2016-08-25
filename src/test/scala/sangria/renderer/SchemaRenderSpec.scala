package sangria.renderer

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.execution.Executor
import sangria.marshalling.InputUnmarshaller
import sangria.schema._
import sangria.macros._
import sangria.util.{StringMatchers, FutureResultSupport}
import sangria.introspection.introspectionQuery
import sangria.validation.IntCoercionViolation
import spray.json.JsValue
import scala.concurrent.ExecutionContext.Implicits.global
import sangria.marshalling.sprayJson._
import sangria.marshalling.ScalaInput.scalaInput

class SchemaRenderSpec extends WordSpec with Matchers with FutureResultSupport with StringMatchers {
  def renderForTest[T: InputUnmarshaller](res: T, schema: Schema[_, _]) = "\n" + SchemaRenderer.renderSchema(res)+ "\n"
  def renderForTest(schema: Schema[Unit, Unit]) = "\n" + SchemaRenderer.renderSchema(schema) + "\n"

  def renderSingleFieldSchema(tpe: OutputType[_], args: List[Argument[_]] = Nil)(implicit render: Schema[Unit, Unit] ⇒ String) = {
    val root = ObjectType("Root", fields[Unit, Unit](
      Field("singleField", tpe.asInstanceOf[OutputType[Unit]], arguments = args, resolve = _ ⇒ ())
    ))
    val schema = Schema(root)

    render(schema)
  }

  def `default schema renderer`(implicit render: Schema[Unit, Unit] ⇒ String): Unit = {
    "Prints String Field" in {
      renderSingleFieldSchema(OptionType(StringType)) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints [String] Field" in {
      renderSingleFieldSchema(OptionType(ListType(OptionType(StringType)))) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: [String]
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String! Field" in {
      renderSingleFieldSchema(StringType) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: String!
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints [String]! Field" in {
      renderSingleFieldSchema(ListType(OptionType(StringType))) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: [String]!
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints [String!] Field" in {
      renderSingleFieldSchema(OptionType(ListType(StringType))) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField: [String!]
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Print Object Field" in {
      val foo = ObjectType("Foo", fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "foo")
      ))

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("foo", OptionType(foo), resolve = _ ⇒ ())
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Foo {
        |  str: String
        |}
        |
        |type Root {
        |  foo: Foo
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String Field With Int Arg" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", OptionInputType(IntType)) :: Nil
      ) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String Field With Int Arg With Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", OptionInputType(IntType), 2) :: Nil
      ) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int = 2): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String Field With Int! Arg" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", IntType) :: Nil
      ) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int!): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String Field With Multiple Args" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", OptionInputType(IntType)) :: Argument("argTwo", OptionInputType(StringType)) :: Nil
      ) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int, argTwo: String): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String Field With Multiple Args, First is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args =
          Argument("argOne", OptionInputType(IntType), 1) ::
          Argument("argTwo", OptionInputType(StringType)) ::
          Argument("argThree", OptionInputType(BooleanType)) ::
          Nil
      ) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int = 1, argTwo: String, argThree: Boolean): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String Field With Multiple Args, Second is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args =
          Argument("argOne", OptionInputType(IntType)) ::
          Argument("argTwo", OptionInputType(StringType), defaultValue = "foo") ::
          Argument("argThree", OptionInputType(BooleanType)) ::
          Nil
      ) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int, argTwo: String = "foo", argThree: Boolean): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String Field With Multiple Args, Last is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args =
          Argument("argOne", OptionInputType(IntType)) ::
          Argument("argTwo", OptionInputType(StringType)) ::
          Argument("argThree", OptionInputType(BooleanType), false) ::
          Nil
      ) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  singleField(argOne: Int, argTwo: String, argThree: Boolean = false): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Print Interface" in {
      val foo = InterfaceType("Foo", "My\ndescription", fields[Unit, Unit](
        Field("str", OptionType(StringType), description = Some("field\ndescription"), resolve = _ ⇒ "foo")
      ))

      val bar = ObjectType("Bar", interfaces[Unit, Unit](foo), fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "foo")))

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("bar", OptionType(bar), resolve = _ ⇒ ())
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Bar implements Foo {
        |  str: String
        |}
        |
        |# My
        |# description
        |interface Foo {
        |  # field
        |  # description
        |  str: String
        |}
        |
        |type Root {
        |  bar: Bar
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Print Multiple Interface" in {
      val foo = InterfaceType("Foo", fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "foo")
      ))

      val baz = InterfaceType("Baaz", fields[Unit, Unit](
        Field("int", OptionType(IntType), resolve = _ ⇒ 1)
      ))

      val bar = ObjectType("Bar", interfaces[Unit, Unit](foo, baz), Nil)

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("bar", OptionType(bar), resolve = _ ⇒ ())
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |interface Baaz {
        |  int: Int
        |}
        |
        |type Bar implements Foo, Baaz {
        |  str: String
        |  int: Int
        |}
        |
        |interface Foo {
        |  str: String
        |}
        |
        |type Root {
        |  bar: Bar
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Print Multiple Interface (with interface hierarchy)" in {
      val foo = InterfaceType("Foo", fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "foo")
      ))

      val baz = InterfaceType("Baaz", fields[Unit, Unit](
        Field("int", OptionType(IntType), resolve = _ ⇒ 1)
      ), interfaces[Unit, Unit](foo))

      val bar = ObjectType("Bar", interfaces[Unit, Unit](baz), Nil)

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("bar", OptionType(bar), resolve = _ ⇒ ())
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |interface Baaz {
        |  int: Int
        |  str: String
        |}
        |
        |type Bar implements Baaz, Foo {
        |  int: Int
        |  str: String
        |}
        |
        |interface Foo {
        |  str: String
        |}
        |
        |type Root {
        |  bar: Bar
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Print Unions" in {
      val foo = ObjectType("Foo", fields[Unit, Unit](
        Field("bool", OptionType(BooleanType), resolve = _ ⇒ true)
      ))

      val bar = ObjectType("Bar", fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "f")
      ))

      val singleUnion = UnionType("SingleUnion", types = foo :: Nil)
      val multipleUnion = UnionType("MultipleUnion", types = foo :: bar :: Nil)

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("single", OptionType(singleUnion), resolve = _ ⇒ ()),
        Field("multiple", OptionType(multipleUnion), resolve = _ ⇒ ())
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Bar {
        |  str: String
        |}
        |
        |type Foo {
        |  bool: Boolean
        |}
        |
        |union MultipleUnion = Foo | Bar
        |
        |type Root {
        |  single: SingleUnion
        |  multiple: MultipleUnion
        |}
        |
        |union SingleUnion = Foo
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Print Input Type" in {
      val articleType = InputObjectType("Article", "Blog article", List(
        InputField("title", StringType, description = "The most important field"),
        InputField("author", OptionInputType(StringType), description = "The author of the article", defaultValue = "Anonymous"),
        InputField("comments", ListInputType(StringType), description = "comments!")))

      val inputType = InputObjectType("InputType", "My\ndescription", List(
        InputField("int", OptionInputType(IntType), description = "My\nfield\ndescription"),
        InputField("article", OptionInputType(articleType), description = "has a default!",
          defaultValue = scalaInput(Map("title" → "Hello", "auhor" → "Bob", "comments" → List("first!", "looks good!"))))))

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("str", OptionType(StringType),
          arguments = Argument("argOne", OptionInputType(inputType)) :: Nil,
          resolve = _ ⇒ None)
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |# Blog article
        |input Article {
        |  # The most important field
        |  title: String!
        |
        |  # The author of the article
        |  author: String = "Anonymous"
        |
        |  # comments!
        |  comments: [String!]!
        |}
        |
        |# My
        |# description
        |input InputType {
        |  # My
        |  # field
        |  # description
        |  int: Int
        |
        |  # has a default!
        |  article: Article = {title:"Hello",author:"Anonymous",comments:["first!","looks good!"]}
        |}
        |
        |type Root {
        |  str(argOne: InputType): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Custom Scalar" in {
      val odd = ScalarType[Int]("Odd",
        description = Some("My\ndescription"),
        coerceOutput = valueOutput,
        coerceUserInput = {
          case i: Int if i % 2 != 0 ⇒ Right(i)
          case i: BigInt if i.isValidInt && i % 2 != BigInt(0) ⇒ Right(i.intValue)
          case _ ⇒ Left(IntCoercionViolation)
        },
        coerceInput = {
          case ast.IntValue(i, _, _) if i % 2 != 0 ⇒ Right(i)
          case ast.BigIntValue(i, _, _) if i.isValidInt && i % 2 != BigInt(0) ⇒ Right(i.intValue)
          case _ ⇒ Left(IntCoercionViolation)
        })

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("odd", OptionType(odd), resolve = _ ⇒ None)
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |# My
        |# description
        |scalar Odd
        |
        |type Root {
        |  odd: Odd
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Enum" in {
      val rgb = EnumType[Int]("RGB",
        description = Some("My\ndescription"),
        values = List(
          EnumValue("RED", description = Some("My Red\n color"), value = 1),
          EnumValue("GREEN", value = 2, deprecationReason = Some("not cool anymore")),
          EnumValue("BLUE", value = 3, deprecationReason = Some(DefaultDeprecationReason))))

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("rgb", OptionType(rgb), resolve = _ ⇒ None)
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |# My
        |# description
        |enum RGB {
        |  # My Red
        |  # color
        |  RED
        |  GREEN @deprecated(reason: "not cool anymore")
        |  BLUE @deprecated
        |}
        |
        |type Root {
        |  rgb: RGB
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Directive" in {
      val myDirective = Directive("myDirective",
        description = Some("Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec posuere ornare nulla, non bibendum nisi dictum at. Etiam consequat velit ut leo fringilla mollis. Integer ut fringilla ante. Curabitur sagittis malesuada nibh sed vestibulum. \nNunc eu metus felis. Cras tellus nibh, porta nec lorem quis, elementum egestas tellus. Etiam vitae tellus vitae dui varius lobortis."),
        arguments =
          Argument("first", OptionInputType(ListInputType(StringType)), "Some descr", scalaInput(List("foo", "bar", "baz"))) ::
          Argument("last", OptionInputType(IntType), "Another descr") ::
          Nil,
        locations = Set(DirectiveLocation.FieldDefinition, DirectiveLocation.InputFieldDefinition),
        shouldInclude = _ ⇒ true)

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("foo", OptionType(StringType), resolve = _ ⇒ None)))

      val schema = Schema(root, directives = BuiltinDirectives :+ myDirective)

      render(schema) should equal ("""
        |schema {
        |  query: Root
        |}
        |
        |type Root {
        |  foo: String
        |}
        |
        |# Lorem ipsum dolor sit amet, consectetur adipiscing elit. Donec posuere ornare nulla, non bibendum nisi dictum at. Etiam consequat velit ut leo fringilla mollis. Integer ut fringilla ante. Curabitur sagittis malesuada nibh sed vestibulum.
        |# Nunc eu metus felis. Cras tellus nibh, porta nec lorem quis, elementum egestas tellus. Etiam vitae tellus vitae dui varius lobortis.
        |directive @myDirective(first: [String!] = ["foo","bar","baz"], last: Int) on FIELD_DEFINITION | INPUT_FIELD_DEFINITION
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }
  }

  "Introspection-based Schema Renderer" should {
    behave like `default schema renderer` (schema ⇒ renderForTest(Executor.execute(schema, introspectionQuery).await, schema))

    "throw an exception if introspection results contain some errors" in {
      val root = ObjectType("Root", fields[Unit, Unit](
        Field("singleField", StringType, resolve = _ ⇒ "")
      ))

      val schema = Schema(root)

      an [IllegalArgumentException] should be thrownBy
        SchemaRenderer.renderSchema(Executor.execute(schema, graphql"{someUnknownField}").awaitAndRecoverQueryAnalysis)
    }
  }

  "Schema-based Schema Renderer" should {
    behave like `default schema renderer` (schema ⇒ renderForTest(schema))
  }

  "Introspection Schema Renderer" should {
    "Print Introspection Schema" in {
      val schema = Schema(ObjectType[Unit, Unit]("Root", Nil))
      val rendered = SchemaRenderer.renderIntrospectionSchema(Executor.execute(schema, introspectionQuery).await)

      ("\n" + rendered + "\n") should equal ("""
        |# A Directive provides a way to describe alternate runtime execution and type validation behavior in a GraphQL document.
        |#
        |# In some cases, you need to provide options to alter GraphQL’s execution behavior in ways field arguments will not suffice, such as conditionally including or skipping a field. Directives provide this by describing additional information to the executor.
        |type __Directive {
        |  name: String!
        |  description: String
        |  locations: [__DirectiveLocation!]!
        |  args: [__InputValue!]!
        |  onOperation: Boolean! @deprecated(reason: "Use `locations`.")
        |  onFragment: Boolean! @deprecated(reason: "Use `locations`.")
        |  onField: Boolean! @deprecated(reason: "Use `locations`.")
        |}
        |
        |# A Directive can be adjacent to many parts of the GraphQL language, a __DirectiveLocation describes one such possible adjacencies.
        |enum __DirectiveLocation {
        |  # Location adjacent to a query operation.
        |  QUERY
        |
        |  # Location adjacent to a mutation operation.
        |  MUTATION
        |
        |  # Location adjacent to a subscription operation.
        |  SUBSCRIPTION
        |
        |  # Location adjacent to a field.
        |  FIELD
        |
        |  # Location adjacent to a fragment definition.
        |  FRAGMENT_DEFINITION
        |
        |  # Location adjacent to a fragment spread.
        |  FRAGMENT_SPREAD
        |
        |  # Location adjacent to an inline fragment.
        |  INLINE_FRAGMENT
        |
        |  # Location adjacent to a schema definition.
        |  SCHEMA
        |
        |  # Location adjacent to a scalar definition.
        |  SCALAR
        |
        |  # Location adjacent to an object type definition.
        |  OBJECT
        |
        |  # Location adjacent to a field definition.
        |  FIELD_DEFINITION
        |
        |  # Location adjacent to an argument definition.
        |  ARGUMENT_DEFINITION
        |
        |  # Location adjacent to an interface definition.
        |  INTERFACE
        |
        |  # Location adjacent to a union definition.
        |  UNION
        |
        |  # Location adjacent to an enum definition.
        |  ENUM
        |
        |  # Location adjacent to an enum value definition.
        |  ENUM_VALUE
        |
        |  # INPUT_OBJECT
        |  INPUT_OBJECT
        |
        |  # Location adjacent to an input object field definition.
        |  INPUT_FIELD_DEFINITION
        |}
        |
        |# One possible value for a given Enum. Enum values are unique values, not a placeholder for a string or numeric value. However an Enum value is returned in a JSON response as a string.
        |type __EnumValue {
        |  name: String!
        |  description: String
        |  isDeprecated: Boolean!
        |  deprecationReason: String
        |}
        |
        |# Object and Interface types are described by a list of Fields, each of which has a name, potentially a list of arguments, and a return type.
        |type __Field {
        |  name: String!
        |  description: String
        |  args: [__InputValue!]!
        |  type: __Type!
        |  isDeprecated: Boolean!
        |  deprecationReason: String
        |}
        |
        |# Arguments provided to Fields or Directives and the input fields of an InputObject are represented as Input Values which describe their type and optionally a default value.
        |type __InputValue {
        |  name: String!
        |  description: String
        |  type: __Type!
        |
        |  # A GraphQL-formatted string representing the default value for this input value.
        |  defaultValue: String
        |}
        |
        |# A GraphQL Schema defines the capabilities of a GraphQL server. It exposes all available types and directives on the server, as well as the entry points for query, mutation, and subscription operations.
        |type __Schema {
        |  # A list of all types supported by this server.
        |  types: [__Type!]!
        |
        |  # The type that query operations will be rooted at.
        |  queryType: __Type!
        |
        |  # If this server supports mutation, the type that mutation operations will be rooted at.
        |  mutationType: __Type
        |
        |  # If this server support subscription, the type that subscription operations will be rooted at.
        |  subscriptionType: __Type
        |
        |  # A list of all directives supported by this server.
        |  directives: [__Directive!]!
        |}
        |
        |# The fundamental unit of any GraphQL Schema is the type. There are many kinds of types in GraphQL as represented by the `__TypeKind` enum.
        |#
        |# Depending on the kind of a type, certain fields describe information about that type. Scalar types provide no information beyond a name and description, while Enum types provide their values. Object and Interface types provide the fields they describe. Abstract types, Union and Interface, provide the Object types possible at runtime. List and NonNull types compose other types.
        |type __Type {
        |  kind: __TypeKind!
        |  name: String
        |  description: String
        |  fields(includeDeprecated: Boolean = false): [__Field!]
        |  interfaces: [__Type!]
        |  possibleTypes: [__Type!]
        |  enumValues(includeDeprecated: Boolean = false): [__EnumValue!]
        |  inputFields: [__InputValue!]
        |  ofType: __Type
        |}
        |
        |# An enum describing what kind of type a given `__Type` is.
        |enum __TypeKind {
        |  # Indicates this type is a scalar.
        |  SCALAR
        |
        |  # Indicates this type is an object. `fields` and `interfaces` are valid fields.
        |  OBJECT
        |
        |  # Indicates this type is an interface. `fields` and `possibleTypes` are valid fields.
        |  INTERFACE
        |
        |  # Indicates this type is a union. `possibleTypes` is a valid field.
        |  UNION
        |
        |  # Indicates this type is an enum. `enumValues` is a valid field.
        |  ENUM
        |
        |  # Indicates this type is an input object. `inputFields` is a valid field.
        |  INPUT_OBJECT
        |
        |  # Indicates this type is a list. `ofType` is a valid field.
        |  LIST
        |
        |  # Indicates this type is a non-null. `ofType` is a valid field.
        |  NON_NULL
        |}
        |
        |# Marks an element of a GraphQL schema as no longer supported.
        |directive @deprecated(reason: String = "No longer supported") on ENUM_VALUE | FIELD_DEFINITION
        |
        |# Directs the executor to include this field or fragment only when the `if` argument is true.
        |directive @include(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
        |
        |# Directs the executor to skip this field or fragment when the `if` argument is true.
        |directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }
  }
}
