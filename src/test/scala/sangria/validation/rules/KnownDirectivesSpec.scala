package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class KnownDirectivesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new KnownDirectives)

  "Validate: Known directives" when {
    "within query language" should {
      "with no directives" in expectPasses(
        """
          query Foo {
            name
            ...Frag
          }

          fragment Frag on Dog {
            name
          }
        """)

      "with known directives" in expectPasses(
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

      "with unknown directive" in expectFails(
        """
          {
            dog @unknown(directive: "value") {
              name
            }
          }
        """,
        List(
          "Unknown directive 'unknown'." → Some(Pos(3, 17))
        ))

      "with many unknown directives" in expectFails(
        """
          {
            dog @unknown(directive: "value") {
              name
            }
            human @unknown(directive: "value") {
              name
              pets @unknown(directive: "value") {
                name
              }
            }
          }
        """,
        List(
          "Unknown directive 'unknown'." → Some(Pos(3, 17)),
          "Unknown directive 'unknown'." → Some(Pos(6, 19)),
          "Unknown directive 'unknown'." → Some(Pos(8, 20))
        ))

      "with well placed directives" in expectPasses(
        """
          query Foo @onQuery {
            name @include(if: true)
            ...Frag @include(if: true)
            skippedField @skip(if: true)
            ...SkippedFrag @skip(if: true)
          }

          mutation Bar @onMutation {
            someField
          }
        """)

      "with misplaced directives" in expectFails(
        """
          query Foo @include(if: true) {
            name @onQuery
            ...Frag @onQuery
          }

          mutation Bar @onQuery {
            someField
          }
        """,
        List(
          "Directive 'include' may not be used on query operation." → Some(Pos(2, 21)),
          "Directive 'onQuery' may not be used on field." → Some(Pos(3, 18)),
          "Directive 'onQuery' may not be used on fragment spread." → Some(Pos(4, 21)),
          "Directive 'onQuery' may not be used on mutation operation." → Some(Pos(7, 24))
        ))
    }

    "within schema language" should {
      "with well placed directives" in expectPasses(
        """
          type MyObj implements MyInterface @onObject {
            myField(myArg: Int @onArgumentDefinition): String @onFieldDefinition
          }

          scalar MyScalar @onScalar

          interface MyInterface @onInterface {
            myField(myArg: Int @onArgumentDefinition): String @onFieldDefinition
          }

          union MyUnion @onUnion = MyObj | Other

          enum MyEnum @onEnum {
            MY_VALUE @onEnumValue
          }

          input MyInput @onInputObject {
            myField: Int @onInputFieldDefinition
          }

          schema @onSchema {
            query: MyQuery
          }
        """)

      "with misplaced directives" in expectFails(
        """
          type MyObj implements MyInterface @onInterface {
            myField(myArg: Int @onInputFieldDefinition): String @onInputFieldDefinition
          }

          scalar MyScalar @onEnum

          interface MyInterface @onObject {
            myField(myArg: Int @onInputFieldDefinition): String @onInputFieldDefinition
          }

          union MyUnion @onEnumValue = MyObj | Other

          enum MyEnum @onScalar {
            MY_VALUE @onUnion
          }

          input MyInput @onEnum {
            myField: Int @onArgumentDefinition
          }

          schema @onObject {
            query: MyQuery
          }
        """,
        List(
          "Directive 'onInputFieldDefinition' may not be used on argument definition." → Some(Pos(3, 32)),
          "Directive 'onInputFieldDefinition' may not be used on field definition." → Some(Pos(3, 65)),
          "Directive 'onInterface' may not be used on object type definition." → Some(Pos(2, 45)),
          "Directive 'onEnum' may not be used on scalar type definition." → Some(Pos(6, 27)),
          "Directive 'onInputFieldDefinition' may not be used on argument definition." → Some(Pos(9, 32)),
          "Directive 'onInputFieldDefinition' may not be used on field definition." → Some(Pos(9, 65)),
          "Directive 'onObject' may not be used on interface definition." → Some(Pos(8, 33)),
          "Directive 'onEnumValue' may not be used on union definition." → Some(Pos(12, 25)),
          "Directive 'onUnion' may not be used on enum value definition." → Some(Pos(15, 22)),
          "Directive 'onScalar' may not be used on enum definition." → Some(Pos(14, 23)),
          "Directive 'onArgumentDefinition' may not be used on input field definition." → Some(Pos(19, 26)),
          "Directive 'onEnum' may not be used on input object type definition." → Some(Pos(18, 25)),
          "Directive 'onObject' may not be used on schema definition." → Some(Pos(22, 18))
        ))
    }
  }
}
