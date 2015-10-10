package sangria.renderer

import org.scalatest.{Matchers, WordSpec}
import sangria.ast
import sangria.execution.Executor
import sangria.integration.InputUnmarshaller
import sangria.schema._
import sangria.util.AwaitSupport
import sangria.introspection.introspectionQuery
import sangria.validation.IntCoercionViolation
import scala.concurrent.ExecutionContext.Implicits.global
import sangria.integration.sprayJson._

class SchemaRenderSpec extends WordSpec with Matchers with AwaitSupport {
  def renderForTest[T: InputUnmarshaller](res: T) = SchemaRenderer.renderSchema(res) map ("\n" + _ + "\n")

  def renderSingleFieldSchema(tpe: OutputType[_], args: List[Argument[_]] = Nil) = {
    val root = ObjectType("Root", fields[Unit, Unit](
      Field("singleField", tpe.asInstanceOf[OutputType[Unit]], arguments = args, resolve = _ ⇒ ())
    ))
    val schema = Schema(root)

    renderForTest(Executor(schema).execute(introspectionQuery).await)
  }

  "SchemaRenderer" should {
    "Prints String Field" in {
      renderSingleFieldSchema(OptionType(StringType)) should be (Some("""
        |type Root {
        |  singleField: String
        |}
        |""".stripMargin))
    }

    "Prints [String] Field" in {
      renderSingleFieldSchema(OptionType(ListType(OptionType(StringType)))) should be (Some("""
        |type Root {
        |  singleField: [String]
        |}
        |""".stripMargin))
    }

    "Prints String! Field" in {
      renderSingleFieldSchema(StringType) should be (Some("""
        |type Root {
        |  singleField: String!
        |}
        |""".stripMargin))
    }

    "Prints [String]! Field" in {
      renderSingleFieldSchema(ListType(OptionType(StringType))) should be (Some("""
        |type Root {
        |  singleField: [String]!
        |}
        |""".stripMargin))
    }

    "Prints [String!] Field" in {
      renderSingleFieldSchema(OptionType(ListType(StringType))) should be (Some("""
        |type Root {
        |  singleField: [String!]
        |}
        |""".stripMargin))
    }

    "Print Object Field" in {
      val foo = ObjectType("Foo", fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "foo")
      ))

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("foo", OptionType(foo), resolve = _ ⇒ ())
      ))

      val schema = Schema(root)

      renderForTest(Executor(schema).execute(introspectionQuery).await) should be (Some("""
        |type Foo {
        |  str: String
        |}
        |
        |type Root {
        |  foo: Foo
        |}
        |""".stripMargin))
    }

    "Prints String Field With Int Arg" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", OptionInputType(IntType)) :: Nil
      ) should be (Some("""
        |type Root {
        |  singleField(argOne: Int): String
        |}
        |""".stripMargin))
    }

    "Prints String Field With Int Arg With Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", OptionInputType(IntType), 2) :: Nil
      ) should be (Some("""
        |type Root {
        |  singleField(argOne: Int = 2): String
        |}
        |""".stripMargin))
    }

    "Prints String Field With Int! Arg" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", IntType) :: Nil
      ) should be (Some("""
        |type Root {
        |  singleField(argOne: Int!): String
        |}
        |""".stripMargin))
    }

    "Prints String Field With Multiple Args" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args = Argument("argOne", OptionInputType(IntType)) :: Argument("argTwo", OptionInputType(StringType)) :: Nil
      ) should be (Some("""
        |type Root {
        |  singleField(argOne: Int, argTwo: String): String
        |}
        |""".stripMargin))
    }

    "Prints String Field With Multiple Args, First is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args =
          Argument("argOne", OptionInputType(IntType), 1) ::
          Argument("argTwo", OptionInputType(StringType)) ::
          Argument("argThree", OptionInputType(BooleanType)) ::
          Nil
      ) should be (Some("""
        |type Root {
        |  singleField(argOne: Int = 1, argTwo: String, argThree: Boolean): String
        |}
        |""".stripMargin))
    }

    "Prints String Field With Multiple Args, Second is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args =
          Argument("argOne", OptionInputType(IntType)) ::
          Argument("argTwo", OptionInputType(StringType), defaultValue = "foo") ::
          Argument("argThree", OptionInputType(BooleanType)) ::
          Nil
      ) should be (Some("""
        |type Root {
        |  singleField(argOne: Int, argTwo: String = "foo", argThree: Boolean): String
        |}
        |""".stripMargin))
    }

    "Prints String Field With Multiple Args, Last is Default" in {
      renderSingleFieldSchema(
        tpe = OptionType(StringType),
        args =
          Argument("argOne", OptionInputType(IntType)) ::
          Argument("argTwo", OptionInputType(StringType)) ::
          Argument("argThree", OptionInputType(BooleanType), false) ::
          Nil
      ) should be (Some("""
        |type Root {
        |  singleField(argOne: Int, argTwo: String, argThree: Boolean = false): String
        |}
        |""".stripMargin))
    }

    "Print Interface" in {
      val foo = InterfaceType("Foo", fields[Unit, Unit](
        Field("str", OptionType(StringType), resolve = _ ⇒ "foo")
      ))

      val bar = ObjectType("Bar", interfaces[Unit, Unit](foo), Nil)

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("bar", OptionType(bar), resolve = _ ⇒ ())
      ))

      val schema = Schema(root)

      renderForTest(Executor(schema).execute(introspectionQuery).await) should be (Some("""
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
        |""".stripMargin))
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

      renderForTest(Executor(schema).execute(introspectionQuery).await) should be (Some("""
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
        |""".stripMargin))
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

      renderForTest(Executor(schema).execute(introspectionQuery).await) should be (Some("""
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
        |""".stripMargin))
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

      renderForTest(Executor(schema).execute(introspectionQuery).await) should be (Some("""
        |input InputType {
        |  int: Int
        |}
        |
        |type Root {
        |  str(argOne: InputType): String
        |}
        |""".stripMargin))
    }

    "Custom Scalar" in {
      val odd = ScalarType[Int]("Odd",
        coerceOutput = ast.IntValue(_),
        coerceUserInput = {
          case i: Int if i % 2 != 0 ⇒ Right(i)
          case i: BigInt if i.isValidInt && i % 2 != BigInt(0) ⇒ Right(i.intValue)
          case _ ⇒ Left(IntCoercionViolation)
        },
        coerceInput = {
          case ast.IntValue(i, _) if i % 2 != 0 ⇒ Right(i)
          case ast.BigIntValue(i, _) if i.isValidInt && i % 2 != BigInt(0) ⇒ Right(i.intValue)
          case _ ⇒ Left(IntCoercionViolation)
        })

      val root = ObjectType("Root", fields[Unit, Unit](
        Field("odd", OptionType(odd), resolve = _ ⇒ None)
      ))

      val schema = Schema(root)

      renderForTest(Executor(schema).execute(introspectionQuery).await) should be (Some("""
        |scalar Odd
        |
        |type Root {
        |  odd: Odd
        |}
        |""".stripMargin))
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

      renderForTest(Executor(schema).execute(introspectionQuery).await) should be (Some("""
        |enum RGB {
        |  RED
        |  GREEN
        |  BLUE
        |}
        |
        |type Root {
        |  rgb: RGB
        |}
        |""".stripMargin))
    }

    "Print Introspection Schema" in {
      val schema = Schema(ObjectType[Unit, Unit]("Root", Nil))

      SchemaRenderer
        .renderIntrospectionSchema(Executor(schema).execute(introspectionQuery).await)
        .map("\n" + _ + "\n") should be (Some("""
          |type __Directive {
          |  name: String!
          |  description: String
          |  args: [__InputValue!]!
          |  onOperation: Boolean!
          |  onFragment: Boolean!
          |  onField: Boolean!
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
          |""".stripMargin))
    }
  }
}
