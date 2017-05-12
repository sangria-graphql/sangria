package sangria.schema

import org.scalatest.{Matchers, WordSpec}

import scala.collection.concurrent.TrieMap

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
    "buildArgs" should {
      "build with no arguments" in {
        val args = Args.buildArgs(List.empty, Map.empty)
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)
      }

      "build with defined arguments" in {
        val expectedMap = Map(NON_DEFAULT_ARGUMENT_NAME -> 9001)
        val args = Args.buildArgs(List(nonDefaultArgument), expectedMap)
        args.raw should be (expectedMap)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(nonDefaultArgument) should be (9001)
      }

      "build with undefined arguments" in {
        val args = Args.buildArgs(List(nonDefaultArgument), Map.empty)
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set.empty)
        args.undefinedArgs should be (Set(NON_DEFAULT_ARGUMENT_NAME))
        args.defaultInfo should be (TrieMap.empty)

        an [NoSuchElementException] should be thrownBy args.arg(nonDefaultArgument)
      }

      "build with optional arguments and defined input" in {
        val expectedMap = Map(OPTIONAL_ARGUMENT_NAME -> Some(9001))
        val args = Args.buildArgs(List(optionalArgument), expectedMap)
        args.raw should be (expectedMap)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OPTIONAL_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(optionalArgument) should be (Some(9001))
      }

      "build with optional argument and undefined input" in {
        val args = Args.buildArgs(List(optionalArgument), Map.empty)
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set.empty)
        args.optionalArgs should be (Set(OPTIONAL_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap.empty)

        args.arg(optionalArgument) should be (None)
      }

      "build with default values" in {
        val args = Args.buildArgs(List(defaultArgument), Map.empty)
        args.raw should be (Map.empty)
        args.argsWithDefault should be (Set(DEFAULT_ARGUMENT_NAME))
        args.optionalArgs should be (Set(DEFAULT_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap(DEFAULT_ARGUMENT_NAME -> 10))

        args.arg(defaultArgument) should be (10)
      }

      "build with overriden default values" in {
        val expectedMap = Map(DEFAULT_ARGUMENT_NAME -> Some(9001))
        val args = Args.buildArgs(List(defaultArgument), expectedMap)
        args.raw should be (expectedMap)
        args.argsWithDefault should be (Set(DEFAULT_ARGUMENT_NAME))
        args.optionalArgs should be (Set(DEFAULT_ARGUMENT_NAME))
        args.undefinedArgs should be (Set.empty)
        args.defaultInfo should be (TrieMap(DEFAULT_ARGUMENT_NAME -> 10))

        args.arg(defaultArgument) should be (9001)
      }
    }
  }
}
