package sangria.execution

import org.scalatest.{Matchers, WordSpec}
import sangria.marshalling.{CoercedScalaResultMarshaller, FromInput}
import sangria.parser.DeliveryScheme.Throw
import sangria.parser.QueryParser
import sangria.schema._
import sangria.macros._
import sangria.ast

import scala.reflect.ClassTag

class ValueCoercionHelperSpec extends WordSpec with Matchers {
  val helper = ValueCoercionHelper.default
  val marshaller = CoercedScalaResultMarshaller.default

  "ValueCoercionHelper" should {
    "converts according to input coercion rules" in {
      check(opt(BooleanType), "true", Some(Some(true)))
      check(opt(BooleanType), "false", Some(Some(false)))
      check(opt(IntType), "123", Some(Some(123)))
      check(opt(FloatType), "123", Some(Some(123)))
      check(opt(FloatType), "123.456", Some(Some(123.456)))
      check(opt(StringType), "\"abc123\"", Some(Some("abc123")))
      check(opt(IDType), "123456", Some(Some("123456")))
      check(opt(IDType), "\"123456\"", Some(Some("123456")))
    }

    "does not convert when input coercion rules reject a value" in {
      check(opt(BooleanType), "123", None)
      check(opt(IntType), "123.456", None)
      check(opt(IntType), "true", None)
      check(opt(IntType), "\"123\"", None)
      check(opt(FloatType), "\"123\"", None)
      check(opt(StringType), "123", None)
      check(opt(StringType), "true", None)
      check(opt(IDType), "123.456", None)
    }

    val testEnum = EnumType("TestColor", values = List(
      EnumValue("RED", value = 1),
      EnumValue("GREEN", value = 2),
      EnumValue("BLUE", value = 3)))

    "converts enum values according to input coercion rules" in {
      check(opt(testEnum), "RED", Some(Some(1)))
      check(opt(testEnum), "BLUE", Some(Some(3)))
      check(opt(testEnum), "null", Some(None))
      check(opt(testEnum), "3", None)
      check(opt(testEnum), "\"BLUE\"", None)
    }

    // Boolean!
    val nonNullBool = BooleanType
    // [Boolean]
    val listOfBool = OptionInputType(ListInputType(OptionInputType(BooleanType)))
    // [Boolean!]
    val listOfNonNullBool = OptionInputType(ListInputType(nonNullBool))
    // [Boolean]!
    val nonNullListOfBool = ListInputType(OptionInputType(BooleanType))
    // [Boolean!]!
    val nonNullListOfNonNullBool = ListInputType(nonNullBool)

    "coerces to null unless non-null" in {
      check(opt(BooleanType), "null", Some(None))
      check(nonNullBool, "null", None)
    }

    "coerces lists of values" in {
      check(opt(listOfBool), "true", Some(Some(List(Some(true)))))
      check(opt(listOfBool), "123", None)
      check(opt(listOfBool), "null", Some(None))
      check(opt(listOfBool), "[true, false]", Some(Some(List(Some(true), Some(false)))))
      check(opt(listOfBool), "[true, 123]", None)
      check(opt(listOfBool), "[true, null]", Some(Some(List(Some(true), None))))
      check(opt(listOfBool), "{ true: true }", None)
    }

    "coerces non-null lists of values" in {
      check(nonNullListOfBool, "true", Some(List(Some(true))))
      check(nonNullListOfBool, "123", None)
      check(nonNullListOfBool, "null", None)
      check(nonNullListOfBool, "[true, false]", Some(List(Some(true), Some(false))))
      check(nonNullListOfBool, "[true, 123]", None)
      check(nonNullListOfBool, "[true, null]", Some(List(Some(true), None)))
    }

    "coerces lists of non-null values" in {
      check(listOfNonNullBool, "true", Some(Some(List(true))))
      check(listOfNonNullBool, "123", None)
      check(listOfNonNullBool, "null", Some(None))
      check(listOfNonNullBool, "[true, false]", Some(Some(List(true, false))))
      check(listOfNonNullBool, "[true, 123]", None)
      check(listOfNonNullBool, "[true, null]", None)
    }

    "coerces non-null lists of non-null values" in {
      check(nonNullListOfNonNullBool, "true", Some(List(true)))
      check(nonNullListOfNonNullBool, "123", None)
      check(nonNullListOfNonNullBool, "null", None)
      check(nonNullListOfNonNullBool, "[true, false]", Some(List(true, false)))
      check(nonNullListOfNonNullBool, "[true, 123]", None)
      check(nonNullListOfNonNullBool, "[true, null]", None)
    }

    val testInputObj = InputObjectType("TestInput", fields = List(
      InputField("int", opt(IntType), 42),
      InputField("bool", opt(BooleanType)),
      InputField("requiredBool", BooleanType)))

    "coerces input objects according to input coercion rules" in {
      check(opt(testInputObj), "null", Some(None))
      check(opt(testInputObj), "123", None)
      check(opt(testInputObj), "[]", None)
      check(opt(testInputObj), "{ int: 123, requiredBool: false }", Some(Some(Map("int" → Some(123), "requiredBool" → false))))
      check(opt(testInputObj), "{ bool: true, requiredBool: false }", Some(Some(Map("int" → Some(42), "bool" → Some(true), "requiredBool" → false))))
      check(opt(testInputObj), "{ int: true, requiredBool: true }", None)
      check(opt(testInputObj), "{ requiredBool: null }", None)
      check(opt(testInputObj), "{ bool: true }", None)
    }

    "accepts variable values assuming already coerced" in {
      check(opt(BooleanType), "$var", None)
      check(opt(BooleanType), "$var", Some(Some(true)), "$var: Boolean" → """{"var": true}""")
      check(opt(BooleanType), "$var", Some(None), "$var: Boolean" → """{"var": null}""")
    }

    "asserts variables are provided as items in lists" in {
      check(listOfBool, "[ $foo ]", Some(Some(List(None))))
      check(listOfNonNullBool, "[ $foo ]", None)
      check(listOfNonNullBool, "[ $foo ]", Some(Some(List(true))), "$foo: Boolean!" → """{"foo": true}""")
      check(listOfNonNullBool, "$foo", Some(Some(List(true))), "$foo: [Boolean!]" → """{"foo": true}""")
      check(listOfNonNullBool, "$foo", Some(Some(List(true))), "$foo: [Boolean!]" → """{"foo": [true]}""")
    }

    "omits input object fields for unprovided variables" in {
      check(opt(testInputObj), "{ int: $foo, bool: $foo, requiredBool: true }",
        Some(Some(Map("int" → Some(42), "requiredBool" → true))))

      check(opt(testInputObj), "{ int: $foo, bool: $foo, requiredBool: true }",
        Some(Some(Map("int" → None, "bool" → None, "requiredBool" → true))),
        "$foo: Boolean" → """{"foo": null}""")

      check(opt(testInputObj), "{ requiredBool: $foo }", None)

      check(opt(testInputObj), "{ bool: $foo, requiredBool: $foo }",
        Some(Some(Map("int" → Some(42), "bool" → Some(true), "requiredBool" → true))),
        "$foo: Boolean" → """{"foo": true}""")

      check(opt(testInputObj), "$foo",
        Some(Some(Map("int" → Some(42), "requiredBool" → true))),
        "$foo: TestInput" → """{"foo": {"requiredBool": true}}""")

      check(opt(testInputObj), "$foo",
        Some(Some(Map("int" → Some(42), "bool" → None, "requiredBool" → true))),
        "$foo: TestInput" → """{"foo": {"bool": null, "requiredBool": true}}""")
    }
  }

  def coerceInputValue[T](tpe: InputType[T], value: String, vars: (String, String))(implicit fromInput: FromInput[T]) = {
    val testSchema = Schema.buildFromAst(QueryParser.parse(s"""
      input TestInput {
        int: Int = 42
        bool: Boolean
        requiredBool: Boolean!
      }

      type Query {
        foo: String
      }
    """))

    import spray.json._
    import sangria.marshalling.sprayJson._

    val valueCollector = new ValueCollector(testSchema, (if (vars._2.nonEmpty) vars._2 else "{}").parseJson, None, DeprecationTracker.empty, (), PartialFunction.empty)
    val variables = valueCollector.getVariableValues(QueryParser.parse(s"query Foo${if (vars._1.nonEmpty) "(" + vars._1 + ")" else ""} {foo}").operations(Some("Foo")).variables).get

    val parsed = QueryParser.parseInputWithVariables(value)
    val args = valueCollector.getArgumentValues(Argument("a", tpe) :: Nil, ast.Argument("a", parsed) :: Nil, variables, ignoreErrors = true).get

    args.raw.get("a")
  }

  def check[T](tpe: InputType[T], value: String, result: Any, vars: (String, String) = "" → "")(implicit fromInput: FromInput[T]) =
    coerceInputValue(tpe, value, vars) should be (result)

  def cls[T : ClassTag] = implicitly[ClassTag[T]].runtimeClass

  def opt[T](tpe: InputType[T]): InputType[Option[T]] = OptionInputType(tpe)
}
