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
import scala.concurrent.ExecutionContext.Implicits.global
import sangria.marshalling.sprayJson._

class SchemaRenderSpec extends WordSpec with Matchers with FutureResultSupport with StringMatchers {
  def renderForTest[T: InputUnmarshaller](res: T) = "\n" + SchemaRenderer.renderSchema(res)+ "\n"
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
        |type Root {
        |  singleField: String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints [String] Field" in {
      renderSingleFieldSchema(OptionType(ListType(OptionType(StringType)))) should equal ("""
        |type Root {
        |  singleField: [String]
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints String! Field" in {
      renderSingleFieldSchema(StringType) should equal ("""
        |type Root {
        |  singleField: String!
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints [String]! Field" in {
      renderSingleFieldSchema(ListType(OptionType(StringType))) should equal ("""
        |type Root {
        |  singleField: [String]!
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Prints [String!] Field" in {
      renderSingleFieldSchema(OptionType(ListType(StringType))) should equal ("""
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
        |type Root {
        |  singleField(argOne: Int, argTwo: String, argThree: Boolean = false): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Print Interface" in {
      val foo = InterfaceType("Foo", fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "foo")
      ))

      val bar = ObjectType("Bar", interfaces[Unit, Unit](foo), fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "foo")))

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("bar", OptionType(bar), resolve = _ ⇒ ())
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |type Bar implements Foo {
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
      val inputType = InputObjectType("InputType", List(
        InputField("int", OptionInputType(IntType))
      ))

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("str", OptionType(StringType),
          arguments = Argument("argOne", OptionInputType(inputType)) :: Nil,
          resolve = _ ⇒ None)
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |input InputType {
        |  int: Int
        |}
        |
        |type Root {
        |  str(argOne: InputType): String
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Custom Scalar" in {
      val odd = ScalarType[Int]("Odd",
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
        |scalar Odd
        |
        |type Root {
        |  odd: Odd
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "Enum" in {
      val rgb = EnumType[Int]("RGB", values = List(
        EnumValue("RED", value = 1),
        EnumValue("GREEN", value = 2),
        EnumValue("BLUE", value = 3)))

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("rgb", OptionType(rgb), resolve = _ ⇒ None)
      ))

      val schema = Schema(root)

      render(schema) should equal ("""
        |enum RGB {
        |  RED
        |  GREEN
        |  BLUE
        |}
        |
        |type Root {
        |  rgb: RGB
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }
  }

  "Introspection-based Schema Renderer" should {
    behave like `default schema renderer` (schema ⇒ renderForTest(Executor.execute(schema, introspectionQuery).await))

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
        |type __Directive {
        |  name: String!
        |  description: String
        |  locations: [__DirectiveLocation!]!
        |  args: [__InputValue!]!
        |  onOperation: Boolean!
        |  onFragment: Boolean!
        |  onField: Boolean!
        |}
        |
        |enum __DirectiveLocation {
        |  QUERY
        |  MUTATION
        |  SUBSCRIPTION
        |  FIELD
        |  FRAGMENT_DEFINITION
        |  FRAGMENT_SPREAD
        |  INLINE_FRAGMENT
        |  SCHEMA
        |  SCALAR
        |  OBJECT
        |  FIELD_DEFINITION
        |  ARGUMENT_DEFINITION
        |  INTERFACE
        |  UNION
        |  ENUM
        |  ENUM_VALUE
        |  INPUT_OBJECT
        |  INPUT_FIELD_DEFINITION
        |}
        |
        |type __EnumValue {
        |  name: String!
        |  description: String
        |  isDeprecated: Boolean!
        |  deprecationReason: String
        |}
        |
        |type __Field {
        |  name: String!
        |  description: String
        |  args: [__InputValue!]!
        |  type: __Type!
        |  isDeprecated: Boolean!
        |  deprecationReason: String
        |}
        |
        |type __InputValue {
        |  name: String!
        |  description: String
        |  type: __Type!
        |  defaultValue: String
        |}
        |
        |type __Schema {
        |  types: [__Type!]!
        |  queryType: __Type!
        |  mutationType: __Type
        |  subscriptionType: __Type
        |  directives: [__Directive!]!
        |}
        |
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
        |enum __TypeKind {
        |  SCALAR
        |  OBJECT
        |  INTERFACE
        |  UNION
        |  ENUM
        |  INPUT_OBJECT
        |  LIST
        |  NON_NULL
        |}
        |""".stripMargin) (after being strippedOfCarriageReturns)
    }
  }
}
