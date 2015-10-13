package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.integration.ResultMarshaller
import sangria.parser.QueryParser
import sangria.schema._
import sangria.ast
import sangria.util.AwaitSupport
import sangria.validation.StringCoercionViolation

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class ComplexityMeasurementSpec extends WordSpec with Matchers with AwaitSupport {
  val TestScalar = ScalarType[String]("TestScalar",
    complexity = 2.5D,
    coerceOutput = s ⇒ ast.StringValue(s),
    coerceUserInput = {
      case s: String ⇒ Right(s)
      case _ ⇒ Left(StringCoercionViolation)
    },
    coerceInput = {
      case ast.StringValue(id, _) ⇒ Right(id)
      case _ ⇒ Left(StringCoercionViolation)
    })

  trait Named {
    def name: Option[String]
  }

  case class Dog(name: Option[String], barks: Option[Boolean]) extends Named
  case class Cat(name: Option[String], meows: Option[Boolean]) extends Named

  val NamedType = InterfaceType("Named", fields[Unit, Named](
    Field("name", OptionType(StringType),
      complexity = Some((_, _, _) ⇒ 10D),
      resolve = _.value.name)))

  val DogType = ObjectType("Dog", interfaces[Unit, Dog](NamedType), fields[Unit, Dog](
    Field("barks", OptionType(BooleanType),
      complexity = Some((_, _, _) ⇒ 1.2D),
      resolve = _.value.barks)))

  val CatType = ObjectType("Cat", interfaces[Unit, Cat](NamedType), fields[Unit, Cat](
    Field("meows", OptionType(BooleanType), resolve = _.value.meows)))

  val PetType = UnionType[Unit]("Pet", types = DogType :: CatType :: Nil)

  lazy val TestType: ObjectType[Unit, Unit] = ObjectType("Test", () ⇒ fields[Unit, Unit](
    Field("scalar", StringType, resolve = _ ⇒ "tests"),
    Field("scalarCustom", StringType,
      complexity = Some((_, _, c) ⇒ 3.0D + c),
      resolve = _ ⇒ "testsc"),
    Field("scalarArgs", StringType,
      arguments = Argument("foo", StringType) :: Nil,
      resolve = _ ⇒ "testsa"),
    Field("complexScalar", TestScalar, resolve = _ ⇒ "testcs"),
    Field("nestList", ListType(TestType),
      arguments = Argument("size", IntType) :: Nil,
      complexity = Some((_, args, c) ⇒ 1.1D + args.arg[Int]("size") * c),
      resolve = ctx ⇒ (1 to ctx.arg[Int]("size")) map (_ ⇒ ())),
    Field("nest", TestType, resolve = _ ⇒ ()),
    Field("named", OptionType(ListType(NamedType)),
      arguments = Argument("size", IntType) :: Nil,
      complexity = Some((_, args, c) ⇒ 4.0D + args.arg[Int]("size") * c),
      resolve = _ ⇒ List(Dog(Some("Bob"), Some(true)), Cat(Some("Apples"), Some(true)))),
    Field("pets", OptionType(ListType(PetType)),
      arguments = Argument("size", IntType) :: Nil,
      complexity = Some((_, args, c) ⇒ 3.5D + args.arg[Int]("size") * c),
      resolve = _ ⇒ List(Dog(Some("Bob"), Some(true)), Cat(Some("Apples"), Some(true))))
  ))

  val schema = Schema(TestType)

  "ComplexityMeasurement" should {
    "perform basic calculation with overridden `complexity` function" in {
      val Success(query) = QueryParser.parse("""
        {
          scalar
          nestList(size: 3) {
            complexScalar
            nest {
              cc: scalarCustom
              dd: scalarCustom
            }

            foo: nest {
              cc: scalarCustom
              dd: scalarCustom
            }
          }
        }
      """)

      var complexity = 0.0D

      val captureComplexity = (c: Double) ⇒ complexity = c

      Executor.execute(schema, query, measureComplexity = Some(captureComplexity)).await should be (
        Map("data" →
          Map(
            "scalar" → "tests",
            "nestList" → List(
              Map(
                "complexScalar" → "testcs",
                "nest" → Map("cc" → "testsc", "dd" → "testsc"),
                "foo" → Map("cc" → "testsc", "dd" → "testsc")),
              Map(
                "complexScalar" → "testcs",
                "nest" → Map("cc" → "testsc", "dd" → "testsc"),
                "foo" → Map("cc" → "testsc", "dd" → "testsc")),
              Map(
                "complexScalar" → "testcs",
                "nest" → Map("cc" → "testsc", "dd" → "testsc"),
                "foo" → Map("cc" → "testsc", "dd" → "testsc"))))))

      complexity should be (54.6D)
    }

    "not include excluded fields and fragments" in {
      val Success(query) = QueryParser.parse("""
        {
          scalarArgs(foo: "bar")
          baz: scalarArgs(foo: "baz") @skip(if: false)

          nestList(size: 3) @include(if: false){
            complexScalar
            nest {
              cc: scalarCustom
              dd: scalarCustom
            }
          }

          test: scalar @skip(if: true)

          ...fr
          ...fr
          ...fr

          nest {
            ...fr @include(if: false)
          }
        }

        fragment fr on Test {
          test1: scalar @skip(if: true)
          test2: scalar @skip(if: false)
        }
      """)

      var complexity = 0.0D

      val captureComplexity = (c: Double) ⇒ complexity = c

      Executor.execute(schema, query, measureComplexity = Some(captureComplexity)).await should be (
        Map("data" → Map(
          "scalarArgs" → "testsa",
          "baz" → "testsa",
          "test2" → "tests", "nest" → Map())))

      complexity should be (4.0D)
    }

    "estimate interface type complexity based on the most complex possible type" in {
      val Success(query) = QueryParser.parse("""
        {
          n1: named(size: 10) {
            name
          }

          n2: named(size: 3) {
            name

            ... on Cat {
              meows
            }
          }

          ...Foo
        }

        fragment Foo on Test {
          named(size: 4) {
            name

            ... on Dog {
              barks
            }

            ... on Cat {
              meows
            }
          }
        }
      """)

      var complexity = 0.0D

      val captureComplexity = (c: Double) ⇒ complexity = c

      Executor.execute(schema, query, measureComplexity = Some(captureComplexity)).await should be (
        Map("data" →
          Map(
            "n1" → List(
              Map("name" → "Bob"),
              Map("name" → "Apples")),
            "n2" → List(
              Map("name" → "Bob"),
              Map("name" → "Apples", "meows" → true)),
            "named" → List(
              Map("name" → "Bob", "barks" → true),
              Map("name" → "Apples", "meows" → true)))))

      complexity should be (189.8D)
    }

    "estimate union type complexity based on the most complex possible type" in {
      val Success(query) = QueryParser.parse("""
        {
          p1: pets(size: 10) {
            ... on Named {
              name
            }
          }

          p2: pets(size: 3) {
            ... on Cat {
              name
              meows
            }
          }

          ...Foo
        }

        fragment Foo on Test {
          pets(size: 4) {
            ... on Dog {
              name
              barks
            }

            ... on Cat {
              name
              meows
            }
          }
        }
      """)

      var complexity = 0.0D

      val captureComplexity = (c: Double) ⇒ complexity = c

      Executor.execute(schema, query, measureComplexity = Some(captureComplexity)).await should be (
        Map("data" →
          Map(
            "p1" → List(
              Map("name" → "Bob"),
              Map("name" → "Apples")),
            "p2" → List(
              Map(),
              Map("name" → "Apples", "meows" → true)),
            "pets" → List(
              Map("name" → "Bob", "barks" → true),
              Map("name" → "Apples", "meows" → true)))))

      complexity should be (188.3D)
    }

    "ensure that all possible fields are considered in the calculation" in {
      val Success(query) = QueryParser.parse("""
        {
          pets(size: 10) {
            ... on Named {
              name
            }

            ... on Dog {
              barks
            }

            ... on Cat {
              meows
            }
          }
        }
      """)

      var complexity = 0.0D

      val captureComplexity = (c: Double) ⇒ complexity = c

      Executor.execute(schema, query, measureComplexity = Some(captureComplexity)).await should be (
        Map("data" →
          Map(
            "pets" → List(
              Map("name" → "Bob", "barks" → true),
              Map("name" → "Apples", "meows" → true)))))

      complexity should be (115.5D)
    }

    "ability to reject too complex queries" in {
      val Success(query) = QueryParser.parse("""
        {
          scalar
          cs1: complexScalar
          cs2: complexScalar
          cs3: complexScalar
          cs4: complexScalar
        }
        """)

      val rejectComplexity = (c: Double) ⇒
        if (c > 14)
          throw new IllegalArgumentException(s"Too complex query: max allowed complexity is 14.0, but got $c")
        else ()

      val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
        case (m, e: IllegalArgumentException) ⇒ HandledException(e.getMessage)
      }

      Executor.execute(schema, query,
          exceptionHandler = exceptionHandler,
          measureComplexity = Some(rejectComplexity)).await should be (
        Map(
          "data" → null,
          "errors" → List(
            Map("message" → "Too complex query: max allowed complexity is 14.0, but got 15.0"))))
    }
  }
}
