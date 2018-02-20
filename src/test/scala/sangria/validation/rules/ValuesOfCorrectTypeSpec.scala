package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class ValuesOfCorrectTypeSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new ValuesOfCorrectType)

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
          "Expected type 'String', found '1'." → Some(Pos(4, 41))))

      "Float into String" in expectFails(
        """
          {
            complicatedArgs {
              stringArgField(stringArg: 1.0)
            }
          }
        """,
        List(
          "Expected type 'String', found '1.0'." → Some(Pos(4, 41))))

      "Boolean into String" in expectFails(
        """
          {
            complicatedArgs {
              stringArgField(stringArg: true)
            }
          }
        """,
        List(
          "Expected type 'String', found 'true'." → Some(Pos(4, 41))))

      "Unquoted String into String" in expectFails(
        """
          {
            complicatedArgs {
              stringArgField(stringArg: BAR)
            }
          }
        """,
        List(
          "Expected type 'String', found 'BAR'." → Some(Pos(4, 41))))
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
          "Expected type 'Int', found '\"3\"'." → Some(Pos(4, 35))))

      "Big Int into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: 829384293849283498239482938)
            }
          }
        """,
        List(
          "Expected type 'Int', found '829384293849283498239482938'." → Some(Pos(4, 35))))

      "Unquoted String into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: FOO)
            }
          }
        """,
        List(
          "Expected type 'Int', found 'FOO'." → Some(Pos(4, 35))))

      "Simple Float into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: 3.0)
            }
          }
        """,
        List(
          "Expected type 'Int', found '3.0'." → Some(Pos(4, 35))))

      "Float into Int" in expectFails(
        """
          {
            complicatedArgs {
              intArgField(intArg: 3.333)
            }
          }
        """,
        List(
          "Expected type 'Int', found '3.333'." → Some(Pos(4, 35))))
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
          "Expected type 'Float', found '\"3.333\"'." → Some(Pos(4, 39))))

      "Boolean into Float" in expectFails(
        """
          {
            complicatedArgs {
              floatArgField(floatArg: true)
            }
          }
        """,
        List(
          "Expected type 'Float', found 'true'." → Some(Pos(4, 39))))

      "Unquoted into Float" in expectFails(
        """
          {
            complicatedArgs {
              floatArgField(floatArg: FOO)
            }
          }
        """,
        List(
          "Expected type 'Float', found 'FOO'." → Some(Pos(4, 39))))
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
          "Expected type 'Boolean', found '2'." → Some(Pos(4, 43))))

      "Float into Boolean" in expectFails(
        """
          {
            complicatedArgs {
              booleanArgField(booleanArg: 2.0)
            }
          }
        """,
        List(
          "Expected type 'Boolean', found '2.0'." → Some(Pos(4, 43))))

      "String into Boolean" in expectFails(
        """
          {
            complicatedArgs {
              booleanArgField(booleanArg: "true")
            }
          }
        """,
        List(
          "Expected type 'Boolean', found '\"true\"'." → Some(Pos(4, 43))))

      "Unquoted into Boolean" in expectFails(
        """
          {
            complicatedArgs {
              booleanArgField(booleanArg: TRUE)
            }
          }
        """,
        List(
          "Expected type 'Boolean', found 'TRUE'." → Some(Pos(4, 43))))
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
          "Expected type 'ID', found '1.0'." → Some(Pos(4, 33))))

      "Boolean into ID" in expectFails(
        """
          {
            complicatedArgs {
              idArgField(idArg: true)
            }
          }
        """,
        List(
          "Expected type 'ID', found 'true'." → Some(Pos(4, 33))))

      "Unquoted into ID" in expectFails(
        """
          {
            complicatedArgs {
              idArgField(idArg: SOMETHING)
            }
          }
        """,
        List(
          "Expected type 'ID', found 'SOMETHING'." → Some(Pos(4, 33))))
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
          "Expected type 'DogCommand', found '2'." → Some(Pos(4, 43))))

      "Float into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: 1.0)
            }
          }
        """,
        List(
          "Expected type 'DogCommand', found '1.0'." → Some(Pos(4, 43))))

      "String into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: "SIT")
            }
          }
        """,
        List(
          "Expected type 'DogCommand', found '\"SIT\"'." → Some(Pos(4, 43))))

      "Boolean into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: true)
            }
          }
        """,
        List(
          "Expected type 'DogCommand', found 'true'." → Some(Pos(4, 43))))

      "Unknown Enum Value into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: JUGGLE)
            }
          }
        """,
        List(
          "Expected type 'DogCommand!', found 'JUGGLE'. Enum value 'JUGGLE' is undefined in enum type 'DogCommand'. Known values are: SIT, HEEL, DOWN." → Some(Pos(4, 43))))

      "Different case Enum Value into Enum" in expectFails(
        """
          {
            dog {
              doesKnowCommand(dogCommand: sit)
            }
          }
        """,
        List("Expected type 'DogCommand!', found 'sit'. Enum value 'sit' is undefined in enum type 'DogCommand'. Known values are: SIT, HEEL, DOWN." → Some(Pos(4, 43))))
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
      "Incorrect item type" in expectFailsSimple(
        """
          {
            complicatedArgs {
              stringListArgField(stringListArg: ["one", 2])
            }
          }
        """,
        "Expected type 'String', found '2'. String value expected" → Seq(Pos(4, 57)))

      "Single value of incorrect type" in expectFailsSimple(
        """
          {
            complicatedArgs {
              stringListArgField(stringListArg: 1)
            }
          }
        """,
        "Expected type '[String]', found '1'. String value expected" → Seq(Pos(4, 49)))
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
      "Incorrect value type" in expectFailsSimple(
        """
          {
            complicatedArgs {
              multipleReqs(req2: "two", req1: "one")
            }
          }
        """,
        "Expected type 'Int!', found '\"two\"'. Int value expected" → Seq(Pos(4, 34)),
        "Expected type 'Int!', found '\"one\"'. Int value expected" → Seq(Pos(4, 47)))

      "Incorrect value and missing argument" in expectFailsSimple(
        """
          {
            complicatedArgs {
              multipleReqs(req1: "one")
            }
          }
        """,
        "Expected type 'Int!', found '\"one\"'. Int value expected" → Seq(Pos(4, 34)))

      "Null value" in expectFailsSimple(
        """
          {
            complicatedArgs {
              multipleReqs(req1: null)
            }
          }
        """,
        "Expected type 'Int!', found 'null'." → Seq(Pos(4, 34)))
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
      "Partial object, missing required" in expectFailsSimple(
        """
          {
            complicatedArgs {
              complexArgField(complexArg: { intField: 4 })
            }
          }
        """,
        "Field 'ComplexInput.requiredField' of required type 'Boolean!' was not provided." → Seq(Pos(4, 43)))

      "Partial object, invalid field type" in expectFailsSimple(
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
        "Expected type 'String', found '2'. String value expected" → Seq(Pos(5, 42)))

      "Partial object, unknown field arg" in expectFailsSimple(
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
        "Field 'unknownField' is not defined by type 'ComplexInput'; Did you mean intField or booleanField?" → Seq(Pos(6, 17)))
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

      "with directive with incorrect types" in expectFailsSimple(
        """
          {
            dog @include(if: "yes") {
              name @skip(if: ENUM)
            }
          }
        """,
        "Expected type 'Boolean!', found '\"yes\"'. Boolean value expected" → Seq(Pos(3, 30)),
        "Expected type 'Boolean!', found 'ENUM'. Boolean value expected" → Seq(Pos(4, 30)))
    }
  }

  "Variable default values" should {
    "with directives of valid types" in expectPasses(
      """
        query WithDefaultValues(
          $a: Int = 1,
          $b: String = "ok",
          $c: ComplexInput = { requiredField: true, intField: 3 }
        ) {
          dog { name }
        }
      """)

    "variables with valid default null values" in expectPasses(
      """
        query WithDefaultValues(
          $a: Int = null,
          $b: String = null,
          $c: ComplexInput = { requiredField: true, intField: null }
        ) {
          dog { name }
        }
      """)

    "variables with invalid default null values" in expectFailsSimple(
      """
        query WithDefaultValues(
          $a: Int! = null,
          $b: String! = null,
          $c: ComplexInput = { requiredField: null, intField: null }
        ) {
          dog { name }
        }
      """,
      "Expected type 'Int!', found 'null'." → Seq(Pos(3, 22)),
      "Expected type 'String!', found 'null'." → Seq(Pos(4, 25)),
      "Expected type 'Boolean!', found 'null'." → Seq(Pos(5, 47)))

    "variables with invalid default values" in expectFailsSimple(
      """
        query InvalidDefaultValues(
          $a: Int = "one",
          $b: String = 4,
          $c: ComplexInput = "notverycomplex"
        ) {
          dog { name }
        }
      """,
      "Expected type 'Int', found '\"one\"'. Int value expected" → Seq(Pos(3, 21)),
      "Expected type 'String', found '4'. String value expected" → Seq(Pos(4, 24)),
      "Expected type 'ComplexInput', found '\"notverycomplex\"'." → Seq(Pos(5, 30)))

    "variables with complex invalid default values" in expectFailsSimple(
      """
        query WithDefaultValues(
          $a: ComplexInput = { requiredField: 123, intField: "abc" }
        ) {
          dog { name }
        }
      """,
      "Expected type 'Boolean!', found '123'. Boolean value expected" → Seq(Pos(3, 47)),
      "Expected type 'Int', found '\"abc\"'. Int value expected" → Seq(Pos(3, 62)))

    "complex variables missing required field" in expectFailsSimple(
      """
        query MissingRequiredField($a: ComplexInput = {intField: 3}) {
          dog { name }
        }
      """,
      "Field 'ComplexInput.requiredField' of required type 'Boolean!' was not provided." → Seq(Pos(2, 55)))

    "list variables with invalid item" in expectFailsSimple(
      """
        query InvalidItem($a: [String] = ["one", 2]) {
          dog { name }
        }
      """,
      "Expected type 'String', found '2'. String value expected" → Seq(Pos(2, 50)))
  }
}
