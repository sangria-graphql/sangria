package sangria.schema

import org.scalatest.Matchers
import org.scalatest.WordSpec
import sangria.execution.AttributeCoercionError
import sangria.marshalling.sprayJson._
import sangria.util.Cache
import spray.json._

class ArgsSpec extends WordSpec with Matchers {
  val NonDefaultArgumentName = "nonDefaultArgument"
  val DefaultArgumentName = "defaultArgument"
  val OptionalArgumentName = "optionalArg"
  val NestedParentArgumentName = "nestedParentArgument"

  val nonDefaultArgument = Argument(
    name = NonDefaultArgumentName,
    argumentType = IntType,
    description = "Argument without default value"
  )

  val defaultArgument = Argument(
    name = DefaultArgumentName,
    argumentType = OptionInputType(IntType),
    defaultValue = 10,
    description = "Argument with default value"
  )

  val optionalArgument = Argument(
    name = OptionalArgumentName,
    argumentType = OptionInputType(IntType),
    description = "Optional argument"
  )

  val nestedObj  = InputObjectType[JsValue]("Body", List(
      InputField(NonDefaultArgumentName, nonDefaultArgument.argumentType),
      InputField(DefaultArgumentName, defaultArgument.argumentType, 10),
      InputField(OptionalArgumentName, optionalArgument.argumentType)
    )
  )

  val nestedParentArgument = Argument(
    name = NestedParentArgumentName,
    argumentType = nestedObj,
    description = "Nested parent argument"
  )



  "Args: Companion object" when {
    "buildArgs with map input" should {
      "build with no arguments" in {
        val args = Args(List.empty)
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (Cache.empty)
      }

      "build with defined arguments" in {
        val expectedMap = Map(NonDefaultArgumentName -> 9001)
        val args = Args(List(nonDefaultArgument), expectedMap)
        args.raw should be (expectedMap)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (Cache.empty)

        args.arg(nonDefaultArgument) should be (9001)
      }

      "not build with undefined arguments" in {
        an [AttributeCoercionError] should be thrownBy Args(List(nonDefaultArgument))
      }

      "build with optional argument and defined input" in {
        val args = Args(List(optionalArgument), Map(OptionalArgumentName -> 9001))
        args.raw should be (Map(OptionalArgumentName -> Some(9001)))
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OptionalArgumentName))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (Cache.empty)

        args.arg(optionalArgument) should be (Some(9001))
      }

      "build with optional argument and undefined input" in {
        val args = Args(List(optionalArgument))
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OptionalArgumentName))
        args.undefinedArgs should be (Set(OptionalArgumentName))
        args.defaultInfo should be (Cache.empty)

        args.arg(optionalArgument) should be (None)
      }

      "build with default values" in {
        val args = Args(List(defaultArgument))
        args.raw should be (Map(DefaultArgumentName -> Some(10)))
        args.argsWithDefault should be (Set(DefaultArgumentName))
        args.optionalArgs should be (Set(DefaultArgumentName))
        args.undefinedArgs should be (Set(DefaultArgumentName))
        args.defaultInfo should be (Cache.empty)

        args.arg(defaultArgument) should be (10)
      }

      "build with overriden default values" in {
        val args = Args(List(defaultArgument), Map(DefaultArgumentName -> 9001))
        args.raw should be (Map(DefaultArgumentName -> Some(9001)))
        args.argsWithDefault should be (Set(DefaultArgumentName))
        args.optionalArgs should be (Set(DefaultArgumentName))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (Cache.empty)

        args.arg(defaultArgument) should be (9001)
      }
    }

    "buildArgs with spray-json" should {
      "build with defined argument" in {
        val json = JsObject(NonDefaultArgumentName -> JsNumber(10))
        val args = Args(List(nonDefaultArgument), json)
        args.raw should be (Map(NonDefaultArgumentName -> 10))
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (Cache.empty)

        args.arg(nonDefaultArgument) should be (10)
      }

      "not build with undefined arguments" in {
        an [AttributeCoercionError] should be thrownBy Args(List(nonDefaultArgument), s"""{}""".parseJson)
      }

      "build with optional argument and defined input" in {
        val json = JsObject(OptionalArgumentName -> JsNumber(9001))
        val args = Args(List(optionalArgument), json)
        args.raw should be (Map(OptionalArgumentName -> Some(9001)))
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OptionalArgumentName))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (Cache.empty)

        args.arg(optionalArgument) should be (Some(9001))
      }

      "build with optional argument and undefined input" in {
        val args = Args(List(optionalArgument))
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OptionalArgumentName))
        args.undefinedArgs should be (Set(OptionalArgumentName))
        args.defaultInfo should be (Cache.empty)

        args.arg(optionalArgument) should be (None)
      }

      "build with overriden default values" in {
        val json = JsObject(DefaultArgumentName -> JsNumber(9001))
        val args = Args(List(defaultArgument), json)
        args.raw should be (Map(DefaultArgumentName -> Some(9001)))
        args.argsWithDefault should be (Set(DefaultArgumentName))
        args.optionalArgs should be (Set(DefaultArgumentName))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (Cache.empty)

        args.arg(defaultArgument) should be (9001)
      }
    }

    "buildArgs with nested json objects" should {
      "build with nested arguments" in {
        val json = JsObject(NestedParentArgumentName -> JsObject(
          NonDefaultArgumentName -> JsNumber(1),
          DefaultArgumentName -> JsNumber(2),
          OptionalArgumentName -> JsNumber(3)))

        val args = Args(List(nestedParentArgument), json)
        val fields = args.arg(nestedParentArgument).asJsObject.fields

        fields(NonDefaultArgumentName) should be (JsNumber(1))
        fields(DefaultArgumentName) should be (JsNumber(2))
        fields(OptionalArgumentName) should be (JsNumber(3))
      }

      "not build without required arguments" in {
        val json = JsObject(NestedParentArgumentName -> JsObject(
          DefaultArgumentName -> JsNumber(2),
          OptionalArgumentName -> JsNumber(3)))

        an [AttributeCoercionError] should be thrownBy Args(List(nestedParentArgument), json)
      }

      "build without default arguments" in {
        val json = JsObject(NestedParentArgumentName -> JsObject(
          NonDefaultArgumentName -> JsNumber(1),
          OptionalArgumentName -> JsNumber(3)))

        val args = Args(List(nestedParentArgument), json)
        val fields = args.arg(nestedParentArgument).asJsObject.fields

        fields(NonDefaultArgumentName) should be (JsNumber(1))
        fields(DefaultArgumentName) should be (JsNumber(10))
        fields(OptionalArgumentName) should be (JsNumber(3))
      }

      "build without optional arguments" in {
        val json = JsObject(NestedParentArgumentName -> JsObject(
          NonDefaultArgumentName -> JsNumber(1),
          DefaultArgumentName -> JsNumber(2)))

        val args = Args(List(nestedParentArgument), json)
        val fields = args.arg(nestedParentArgument).asJsObject.fields

        fields(NonDefaultArgumentName) should be (JsNumber(1))
        fields(DefaultArgumentName) should be (JsNumber(2))
        fields.get(OptionalArgumentName) should be ('empty)
      }
    }

    "buildArgs with nested map objects" should {
      "build with nested arguments" in {
        val inputMap = Map(NestedParentArgumentName -> Map(
          NonDefaultArgumentName -> 1,
          DefaultArgumentName -> 2,
          OptionalArgumentName -> 3))
        val args = Args(List(nestedParentArgument), inputMap)

        val fields = args.arg(nestedParentArgument).asJsObject.fields

        fields(NonDefaultArgumentName) should be (JsNumber(1))
        fields(DefaultArgumentName) should be (JsNumber(2))
        fields(OptionalArgumentName) should be (JsNumber(3))
      }

      "not build without required arguments" in {
        val inputMap = Map(NestedParentArgumentName -> Map(
          DefaultArgumentName -> 2,
          OptionalArgumentName -> 3))

        an [AttributeCoercionError] should be thrownBy Args(List(nestedParentArgument), inputMap)
      }

      "build without default arguments" in {
        val inputMap = Map(NestedParentArgumentName -> Map(
          NonDefaultArgumentName -> 1,
          OptionalArgumentName -> 3))
        val args = Args(List(nestedParentArgument), inputMap)

        val fields = args.arg(nestedParentArgument).asJsObject.fields

        fields(NonDefaultArgumentName) should be (JsNumber(1))
        fields(DefaultArgumentName) should be (JsNumber(10))
        fields(OptionalArgumentName) should be (JsNumber(3))
      }

      "build without optional arguments" in {
        val inputMap = Map(NestedParentArgumentName -> Map(
          NonDefaultArgumentName -> 1,
          DefaultArgumentName -> 2))
        val args = Args(List(nestedParentArgument), inputMap)

        val fields = args.arg(nestedParentArgument).asJsObject.fields

        fields(NonDefaultArgumentName) should be (JsNumber(1))
        fields(DefaultArgumentName) should be (JsNumber(2))
        fields.get(OptionalArgumentName) should be ('empty)
      }
    }
  }
}
