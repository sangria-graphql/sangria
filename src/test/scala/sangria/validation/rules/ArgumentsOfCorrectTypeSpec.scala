package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class ArgumentsOfCorrectTypeSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new ArgumentsOfCorrectType)

  "Validate: Argument values of correct type" when {
    "Valid values" should {
      "Good int value" in expectPasses(
        """
          {
            complicatedArgs {
              intArgField(intArg: 2)
            }
          }
        """)

      "Good big int value" in expectPasses(
        """
          {
            complicatedArgs {
              bigIntArgField(bigIntArg: 78426587624578962487568746587485)
            }
          }
        """)

      "Good boolean value" in expectPasses(
        """
          {
            complicatedArgs {
              booleanArgField(booleanArg: true)
            }
          }
        """)

      "Good string value" in expectPasses(
        """
          {
            complicatedArgs {
              stringArgField(stringArg: "foo")
            }
          }
        """)

      "Good float value" in expectPasses(
        """
          {
            complicatedArgs {
              floatArgField(floatArg: 1.1)
            }
          }
        """)

      "Good big decimal value" in expectPasses(
        """
          {
            complicatedArgs {
              bigDecimalArgField(bigDecimalArg: 18246597864875687436587643875634.127647823648763287463)
            }
          }
        """)

      "Int into Float" in expectPasses(
        """
          {
            complicatedArgs {
              floatArgField(floatArg: 1)
            }
          }
        """)

      "Int into ID" in expectPasses(
        """
          {
            complicatedArgs {
              idArgField(idArg: 1)
            }
          }
        """)

      "String into ID" in expectPasses(
        """
          {
            complicatedArgs {
              idArgField(idArg: "someIdString")
            }
          }
        """)

      "Good enum value" in expectPasses(
        """
          {
            dog {
              doesKnowCommand(dogCommand: SIT)
            }
          }
        """)

      "null into nullable type (1)" in expectPasses(
        """
          {
            complicatedArgs {
              intArgField(intArg: null)
            }
          }
        """)

      "null into nullable type (2)" in expectPasses(
        """
          {
            dog(a: null, b: null, c:{ requiredField: true, intField: null }) {
              name
            }
          }
        """)

    }

    "Invalid String values" should {
      "Int into String" in expectFails(
        """
          {
            complicatedArgs {
              stringArgField(stringArg: 1)
            }
          }
        """,
        List(
          "Argument 'stringArg' expected type 'String' but got: 1." → Some(Pos(4, 41))))

      "Float into String" in expectFails(
        """
          {
            complicatedArgs {
              stringArgField(stringArg: 1.0)
            }
          }
        """,
        List(
          "Argument 'stringArg' expected type 'String' but got: 1.0." → Some(Pos(4, 41))))

      "Boolean into String" in expectFails(
        """
          {
            complicatedArgs {
              stringArgField(stringArg: true)
            }
          }
        """,
        List(
          "Argument 'stringArg' expected type 'String' but got: true." → Some(Pos(4, 41))))

      "Unquoted String into String" in expectFails(
        """
          {
            complicatedArgs {
              stringArgField(stringArg: BAR)
            }
          }
        """,
        List(
          "Argument 'stringArg' expected type 'String' but got: BAR." → Some(Pos(4, 41))))
    }

    "Invalid Int values" should {
      "String into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: "3")
            }
          }
        """,
        List(
          "Argument 'intArg' expected type 'Int' but got: \"3\"." → Some(Pos(4, 35))))

      "Big Int into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: 829384293849283498239482938)
            }
          }
        """,
        List(
          "Argument 'intArg' expected type 'Int' but got: 829384293849283498239482938." → Some(Pos(4, 35))))

      "Unquoted String into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: FOO)
            }
          }
        """,
        List(
          "Argument 'intArg' expected type 'Int' but got: FOO." → Some(Pos(4, 35))))

      "Simple Float into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: 3.0)
            }
          }
        """,
        List(
          "Argument 'intArg' expected type 'Int' but got: 3.0." → Some(Pos(4, 35))))

      "Float into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: 3.333)
            }
          }
        """,
        List(
          "Argument 'intArg' expected type 'Int' but got: 3.333." → Some(Pos(4, 35))))
    }

    "Invalid Float values" should {
      "String into Float" in expectFails(
        """
          {
            complicatedArgs {
              floatArgField(floatArg: "3.333")
            }
          }
        """,
        List(
          "Argument 'floatArg' expected type 'Float' but got: \"3.333\"." → Some(Pos(4, 39))))

      "Boolean into Float" in expectFails(
        """
          {
            complicatedArgs {
              floatArgField(floatArg: true)
            }
          }
        """,
        List(
          "Argument 'floatArg' expected type 'Float' but got: true." → Some(Pos(4, 39))))

      "Unquoted into Float" in expectFails(
        """
          {
            complicatedArgs {
              floatArgField(floatArg: FOO)
            }
          }
        """,
        List(
          "Argument 'floatArg' expected type 'Float' but got: FOO." → Some(Pos(4, 39))))
    }

    "Invalid Boolean value" should {
      "Int into Boolean" in expectFails(
        """
          {
            complicatedArgs {
              booleanArgField(booleanArg: 2)
            }
          }
        """,
        List(
          "Argument 'booleanArg' expected type 'Boolean' but got: 2." → Some(Pos(4, 43))))

      "Float into Boolean" in expectFails(
        """
          {
            complicatedArgs {
              booleanArgField(booleanArg: 2.0)
            }
          }
        """,
        List(
          "Argument 'booleanArg' expected type 'Boolean' but got: 2.0." → Some(Pos(4, 43))))

      "String into Boolean" in expectFails(
        """
          {
            complicatedArgs {
              booleanArgField(booleanArg: "true")
            }
          }
        """,
        List(
          "Argument 'booleanArg' expected type 'Boolean' but got: \"true\"." → Some(Pos(4, 43))))

      "Unquoted into Boolean" in expectFails(
        """
          {
            complicatedArgs {
              booleanArgField(booleanArg: TRUE)
            }
          }
        """,
        List(
          "Argument 'booleanArg' expected type 'Boolean' but got: TRUE." → Some(Pos(4, 43))))
    }

    "Invalid ID value" should {
      "Float into ID" in expectFails(
        """
          {
            complicatedArgs {
              idArgField(idArg: 1.0)
            }
          }
        """,
        List(
          "Argument 'idArg' expected type 'ID' but got: 1.0." → Some(Pos(4, 33))))

      "Boolean into ID" in expectFails(
        """
          {
            complicatedArgs {
              idArgField(idArg: true)
            }
          }
        """,
        List(
          "Argument 'idArg' expected type 'ID' but got: true." → Some(Pos(4, 33))))

      "Unquoted into ID" in expectFails(
        """
          {
            complicatedArgs {
              idArgField(idArg: SOMETHING)
            }
          }
        """,
        List(
          "Argument 'idArg' expected type 'ID' but got: SOMETHING." → Some(Pos(4, 33))))
    }

    "Invalid Enum value" should {
      "Int into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: 2)
            }
          }
        """,
        List(
          "Argument 'dogCommand' expected type 'DogCommand' but got: 2." → Some(Pos(4, 43))))

      "Float into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: 1.0)
            }
          }
        """,
        List(
          "Argument 'dogCommand' expected type 'DogCommand' but got: 1.0." → Some(Pos(4, 43))))

      "String into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: "SIT")
            }
          }
        """,
        List(
          "Argument 'dogCommand' expected type 'DogCommand' but got: \"SIT\"." → Some(Pos(4, 43))))

      "Boolean into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: true)
            }
          }
        """,
        List(
          "Argument 'dogCommand' expected type 'DogCommand' but got: true." → Some(Pos(4, 43))))

      "Unknown Enum Value into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: JUGGLE)
            }
          }
        """,
        List(
          "Argument 'dogCommand' expected type 'DogCommand' but got: JUGGLE." → Some(Pos(4, 43))))

      "Different case Enum Value into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: sit)
            }
          }
        """,
        List(
          "Argument 'dogCommand' expected type 'DogCommand' but got: sit." → Some(Pos(4, 43))))
    }

    "Valid List value" should {
      "Good list value" in expectPasses(
        """
          {
            complicatedArgs {
              stringListArgField(stringListArg: ["one", null, "two"])
            }
          }
        """)

      "Empty list value" in expectPasses(
        """
          {
            complicatedArgs {
              stringListArgField(stringListArg: [])
            }
          }
        """)

      "Null value" in expectPasses(
        """
          {
            complicatedArgs {
              stringListArgField(stringListArg: null)
            }
          }
        """)

      "Single value into List" in expectPasses(
        """
          {
            complicatedArgs {
              stringListArgField(stringListArg: "one")
            }
          }
        """)
    }

    "Invalid List value" should {
      "Incorrect item type" in expectFails(
        """
          {
            complicatedArgs {
              stringListArgField(stringListArg: ["one", 2])
            }
          }
        """,
        List(
          "Argument 'stringListArg' expected type '[String]' but got: [\"one\", 2]." → Some(Pos(4, 49))))

      "Single value of incorrect type" in expectFails(
        """
          {
            complicatedArgs {
              stringListArgField(stringListArg: 1)
            }
          }
        """,
        List(
          "Argument 'stringListArg' expected type '[String]' but got: 1." → Some(Pos(4, 49))))
    }

    "Valid non-nullable value" should {
      "Arg on optional arg" in expectPasses(
        """
          {
            dog {
              isHousetrained(atOtherHomes: true)
            }
          }
        """)

      "No Arg on optional arg" in expectPasses(
        """
          {
            complicatedArgs {
              multipleReqs(req1: 1, req2: 2)
            }
          }
        """)

      "Multiple args reverse order" in expectPasses(
        """
          {
            complicatedArgs {
              multipleReqs(req2: 2, req1: 1)
            }
          }
        """)

      "No args on multiple optional" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOpts
            }
          }
        """)

      "One arg on multiple optional" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOpts(opt1: 1)
            }
          }
        """)

      "Second arg on multiple optional" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOpts(opt2: 1)
            }
          }
        """)

      "Multiple reqs on mixedList" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOptAndReq(req1: 3, req2: 4)
            }
          }
        """)

      "Multiple reqs and one opt on mixedList" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOptAndReq(req1: 3, req2: 4, opt1: 5)
            }
          }
        """)

      "All reqs and opts on mixedList" in expectPasses(
        """
          {
            complicatedArgs {
              multipleOptAndReq(req1: 3, req2: 4, opt1: 5, opt2: 6)
            }
          }
        """)
    }

    "Invalid non-nullable value" should {
      "Incorrect value type" in expectFails(
        """
          {
            complicatedArgs {
              multipleReqs(req2: "two", req1: "one")
            }
          }
        """,
        List(
          "Argument 'req1' expected type 'Int!' but got: \"one\"." → Some(Pos(4, 47)),
          "Argument 'req2' expected type 'Int!' but got: \"two\"." → Some(Pos(4, 34))))

      "Incorrect value and missing argument" in expectFails(
        """
          {
            complicatedArgs {
              multipleReqs(req1: "one")
            }
          }
        """,
        List(
          "Argument 'req1' expected type 'Int!' but got: \"one\"." → Some(Pos(4, 34))))

      "Null value" in expectFails(
        """
          {
            complicatedArgs {
              multipleReqs(req1: null)
            }
          }
        """,
        List(
          "Argument 'req1' expected type 'Int!' but got: null." → Some(Pos(4, 34))))
    }

    "Valid input object value" should {
      "Optional arg, despite required field in type" in expectPasses(
        """
          {
            complicatedArgs {
              complexArgField
            }
          }
        """)

      "Partial object, only required" in expectPasses(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: { requiredField: true })
            }
          }
        """)

      "Partial object, required field can be falsey" in expectPasses(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: { requiredField: false })
            }
          }
        """)

      "Partial object, including required" in expectPasses(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: { requiredField: true, intField: 4 })
            }
          }
        """)

      "Full object" in expectPasses(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: {
                requiredField: true,
                intField: 4,
                stringField: "foo",
                booleanField: false,
                stringListField: ["one", "two"]
              })
            }
          }
        """)

      "Full object with fields in different order" in expectPasses(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: {
                stringListField: ["one", "two"],
                booleanField: false,
                requiredField: true,
                stringField: "foo",
                intField: 4,
              })
            }
          }
        """)
    }

    "Invalid input object value" should {
      "Partial object, missing required" in expectFails(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: { intField: 4 })
            }
          }
        """,
        List(
          "Argument 'complexArg' expected type 'ComplexInput' but got: {intField: 4}" → Some(Pos(4, 43))))

      "Partial object, invalid field type" in expectFailsPosList(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: {
                stringListField: ["one", 2],
                requiredField: true,
              })
            }
          }
        """,
        List(
          "Argument 'complexArg' expected type 'ComplexInput' but got: {stringListField: [\"one\", 2], requiredField: true}. Reason: [in field 'stringListField'] [at index #1] String value expected" →
              List(Pos(4, 43), Pos(5, 34))))

      "Partial object, unknown field arg" in expectFailsPosList(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: {
                requiredField: true,
                unknownField: "value"
              })
            }
          }
        """,
        List(
          "Argument 'complexArg' expected type 'ComplexInput' but got: {requiredField: true, unknownField: \"value\"}. Reason: Unknown field 'unknownField' is not defined in the input type 'ComplexInput'" →
              List(Pos(4, 43), Pos(6, 17))))
    }

    "Directive arguments" should {
      "with directives of valid types" in expectPasses(
        """
          {
            dog @include(if: true) {
              name
            }
            human @skip(if: false) {
              name
            }
          }
        """)

      "with directive with incorrect types" in expectFails(
        """
          {
            dog @include(if: "yes") {
              name @skip(if: ENUM)
            }
          }
        """,
        List(
          "Argument 'if' expected type 'Boolean!' but got: \"yes\"." → Some(Pos(3, 30)),
          "Argument 'if' expected type 'Boolean!' but got: ENUM." → Some(Pos(4, 30))))
    }
  }
}
