package sangria.schema

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.AttributeCoercionError

import scala.collection.concurrent.TrieMap
import sangria.marshalling.sprayJson._
import spray.json._

class ArgsSpec extends WordSpec with Matchers {


  val NON_DEFAULT_ARGUMENT_NAME = "nonDefaultArgument"
  val DEFAULT_ARGUMENT_NAME = "defaultArgument"
  val OPTIONAL_ARGUMENT_NAME = "optionalArg"

  val nonDefaultArgument = Argument(
    name = NON_DEFAULT_ARGUMENT_NAME,
    argumentType = IntType,
    description = "Argument without default value"
  )

  val defaultArgument = Argument(
    name = DEFAULT_ARGUMENT_NAME,
    argumentType = OptionInputType(IntType),
    defaultValue = 10,
    description = "Argument with default value"
  )

  val optionalArgument = Argument(
    name = OPTIONAL_ARGUMENT_NAME,
    argumentType = OptionInputType(IntType),
    description = "Optional argument"
  )

  "Args: Companion object" when {
    "buildArgs with map input" should {
      "build with no arguments" in {
        val args = Args(List.empty)
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)
      }

      "build with defined arguments" in {
        val expectedMap = Map(NON_DEFAULT_ARGUMENT_NAME -> 9001)
        val args = Args(List(nonDefaultArgument), expectedMap)
        args.raw should be (expectedMap)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(nonDefaultArgument) should be (9001)
      }

      "not build with undefined arguments" in {
        an [AttributeCoercionError] should be thrownBy Args(List(nonDefaultArgument))
      }

      "build with optional argument and defined input" in {
        val expectedMap = Map(OPTIONAL_ARGUMENT_NAME -> 9001)
        val args = Args(List(optionalArgument), expectedMap)
        args.raw should be (expectedMap)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OPTIONAL_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(optionalArgument) should be (Some(9001))
      }

      "build with optional argument and undefined input" in {
        val args = Args(List(optionalArgument))
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OPTIONAL_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(optionalArgument) should be (None)
      }

      "build with default values" in {
        val args = Args(List(defaultArgument))
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set(DEFAULT_ARGUMENT_NAME))
        args.optionalArgs should be (Set(DEFAULT_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap(DEFAULT_ARGUMENT_NAME -> 10))

        args.arg(defaultArgument) should be (10)
      }

      "build with overriden default values" in {
        val expectedMap = Map(DEFAULT_ARGUMENT_NAME -> 9001)
        val args = Args(List(defaultArgument), expectedMap)
        args.raw should be (expectedMap)
        args.argsWithDefault should be (Set(DEFAULT_ARGUMENT_NAME))
        args.optionalArgs should be (Set(DEFAULT_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap(DEFAULT_ARGUMENT_NAME -> 10))

        args.arg(defaultArgument) should be (9001)
      }
    }

    "buildArgs with spray-json" should {
      "build with defined argument" in {
        val args = Args(List(nonDefaultArgument), s"""{"$NON_DEFAULT_ARGUMENT_NAME": 10}""".parseJson)
        args.raw should be (Map(NON_DEFAULT_ARGUMENT_NAME -> 10))
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(nonDefaultArgument) should be (10)
      }

      "not build with undefined arguments" in {
        an [AttributeCoercionError] should be thrownBy Args(List(nonDefaultArgument), s"""{}""".parseJson)
      }

      "build with optional argument and defined input" in {
        val args = Args(List(optionalArgument), s"""{"$OPTIONAL_ARGUMENT_NAME": 9001}""".parseJson)
        args.raw should be (Map(OPTIONAL_ARGUMENT_NAME -> Some(9001)))
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OPTIONAL_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(optionalArgument) should be (Some(9001))
      }

      "build with optional argument and undefined input" in {
        val args = Args(List(optionalArgument))
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OPTIONAL_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(optionalArgument) should be (None)
      }

      "build with overriden default values" in {
        val args = Args(List(defaultArgument), s"""{"$DEFAULT_ARGUMENT_NAME": 9001}""".parseJson)
        args.raw should be (Map(DEFAULT_ARGUMENT_NAME -> Some(9001)))
        args.argsWithDefault should be (Set(DEFAULT_ARGUMENT_NAME))
        args.optionalArgs should be (Set(DEFAULT_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap(DEFAULT_ARGUMENT_NAME -> 10))

        args.arg(defaultArgument) should be (9001)
      }
    }
  }
}
