package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.schema._
import sangria.util.{Pos, ValidationSupport}

class OverlappingFieldsCanBeMergedSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new OverlappingFieldsCanBeMerged)

  "Validate: Overlapping fields can be merged" should {
    "unique fields" in expectPasses(
      """
        fragment uniqueFields on Dog {
          name
          nickname
        }
      """)

    "allows inline typeless fragments" in expectPasses(
      """
        {
          a
          ... {
            a
          }
        }
      """)

    "unique fields on inline fragments without type condition" in expectPasses(
      """
        fragment uniqueFields on Dog {
          ... {
            name
            nickname
          }
          ... {
            name
            nickname
          }
        }
      """)

    "identical fields" in expectPasses(
      """
        fragment mergeIdenticalFields on Dog {
          name
          name
        }
      """)

    "identical fields with identical args" in expectPasses(
      """
        fragment mergeIdenticalFieldsWithIdenticalArgs on Dog {
          doesKnowCommand(dogCommand: SIT)
          doesKnowCommand(dogCommand: SIT)
        }
      """)

    "identical fields with identical directives" in expectPasses(
      """
        fragment mergeSameFieldsWithSameDirectives on Dog {
          name @include(if: true)
          name @include(if: true)
        }
      """)

    "different args with different aliases" in expectPasses(
      """
        fragment differentArgsWithDifferentAliases on Dog {
          knowsSit: doesKnowCommand(dogCommand: SIT)
          knowsDown: doesKnowCommand(dogCommand: DOWN)
        }
      """)

    // Note: Differing skip/include directives don't create an ambiguous return
    // value and are acceptable in conditions where differing runtime values
    // may have the same desired effect of including or skipping a field.
    "different skip/include directives accepted" in expectPasses(
      """
        fragment differentDirectivesWithDifferentAliases on Dog {
          name @include(if: true)
          name @include(if: false)
        }
      """)

    "different directives with different aliases" in expectPasses(
      """
        fragment differentDirectivesWithDifferentAliases on Dog {
          nameIfTrue: name @include(if: true)
          nameIfFalse: name @include(if: false)
        }
      """)

    "Same aliases with different field targets" in expectFailsPosList(
      """
        fragment sameAliasesWithDifferentFieldTargets on Dog {
          fido: name
          fido: nickname
        }
      """,
      List(
        "Field 'fido' conflict because 'name' and 'nickname' are different fields." → List(Pos(3, 11), Pos(4, 11))
      ))

    "Same aliases allowed on non-overlapping fields" in expectPasses(
      """
        fragment sameAliasesWithDifferentFieldTargets on Pet {
          ... on Dog {
            name
          }
          ... on Cat {
            name: nickname
          }
        }
      """)

    "Alias masking direct field access" in expectFailsPosList(
      """
        fragment aliasMaskingDirectFieldAccess on Dog {
          name: nickname
          name
        }
      """,
      List(
        "Field 'name' conflict because 'nickname' and 'name' are different fields." → List(Pos(3, 11), Pos(4, 11))
      ))

    "different args, second adds an argument" in expectFailsPosList(
      """
        fragment conflictingArgs on Dog {
          doesKnowCommand
          doesKnowCommand(dogCommand: HEEL)
        }
      """,
      List(
        "Field 'doesKnowCommand' conflict because they have differing arguments." → List(Pos(3, 11), Pos(4, 11))
      ))

    "different args, second missing an argument" in expectFailsPosList(
      """
        fragment conflictingArgs on Dog {
          doesKnowCommand(dogCommand: SIT)
          doesKnowCommand
        }
      """,
      List(
        "Field 'doesKnowCommand' conflict because they have differing arguments." → List(Pos(3, 11), Pos(4, 11))
      ))

    "conflicting args" in expectFailsPosList(
      """
        fragment conflictingArgs on Dog {
          doesKnowCommand(dogCommand: SIT)
          doesKnowCommand(dogCommand: HEEL)
        }
      """,
      List(
        "Field 'doesKnowCommand' conflict because they have differing arguments." → List(Pos(3, 11), Pos(4, 11))
      ))

    "allows different args where no conflict is possible" in expectPasses(
      """
        fragment conflictingArgs on Pet {
          ... on Dog {
            name(surname: true)
          }
          ... on Cat {
            name
          }
        }
      """)

    "encounters conflict in fragments" in expectFailsPosList(
      """
        {
          ...A
          ...B
        }
        fragment A on Type {
          x: a
        }
        fragment B on Type {
          x: b
        }
      """,
      List(
        "Field 'x' conflict because 'a' and 'b' are different fields." → List(Pos(7, 11), Pos(10, 11))
      ))

    "reports each conflict once" in expectFailsPosList(
      """
        {
          f1 {
            ...A
            ...B
          }
          f2 {
            ...B
            ...A
          }
          f3 {
            ...A
            ...B
            x: c
          }
        }
        fragment A on Type {
          x: a
        }
        fragment B on Type {
          x: b
        }
      """,
      List(
        "Field 'x' conflict because 'a' and 'b' are different fields." → List(Pos(18, 11), Pos(21, 11)),
        "Field 'x' conflict because 'c' and 'a' are different fields." → List(Pos(14, 13), Pos(18, 11)),
        "Field 'x' conflict because 'c' and 'b' are different fields." → List(Pos(14, 13), Pos(21, 11))
      ))

    "deep conflict" in expectFailsPosList(
      """
        {
          field {
            x: a
          },
          field {
            x: b
          }
        }
      """,
      List(
        "Field 'field' conflict because subfields 'x' conflict because 'a' and 'b' are different fields." → List(Pos(3, 11), Pos(4, 13), Pos(6, 11), Pos(7, 13))
      ))

    "deep conflict with multiple issues" in expectFailsPosList(
      """
        {
          field {
            x: a
            y: c
          },
          field {
            x: b
            y: d
          }
        }
      """,
      List(
        "Field 'field' conflict because subfields 'y' conflict because 'c' and 'd' are different fields and subfields 'x' conflict because 'a' and 'b' are different fields." →
          List(Pos(3, 11), Pos(5, 13), Pos(4, 13), Pos(7, 11), Pos(9, 13), Pos(8, 13))
      ))

    "very deep conflict" in expectFailsPosList(
      """
        {
          field {
            deepField {
              x: a
            }
          },
          field {
            deepField {
              x: b
            }
          }
        }
      """,
      List(
        "Field 'field' conflict because subfields 'deepField' conflict because subfields 'x' conflict because 'a' and 'b' are different fields." →
          List(Pos(3, 11), Pos(4, 13), Pos(5, 15), Pos(8, 11), Pos(9, 13), Pos(10, 15))
      ))

    "reports deep conflict to nearest common ancestor" in expectFailsPosList(
      """
        {
          field {
            deepField {
              x: a
            }
            deepField {
              x: b
            }
          },
          field {
            deepField {
              y
            }
          }
        }
      """,
      List(
        "Field 'deepField' conflict because subfields 'x' conflict because 'a' and 'b' are different fields." →
          List(Pos(4, 13), Pos(5, 15), Pos(7, 13), Pos(8, 15))
      ))

    "reports deep conflict to nearest common ancestor in fragments" in expectFailsPosList(
      """
        {
          field {
            ...F
          }
          field {
            ...F
          }
        }
        fragment F on T {
          deepField {
            deeperField {
              x: a
            }
            deeperField {
              x: b
            }
          },
          deepField {
            deeperField {
              y
            }
          }
        }
      """,
      List(
        "Field 'deeperField' conflict because subfields 'x' conflict because 'a' and 'b' are different fields." →
          List(Pos(12, 13), Pos(13, 15), Pos(15, 13), Pos(16, 15))
      ))

    "reports deep conflict in nested fragments" in expectFailsPosList(
      """
        {
          field {
            ...F
          }
          field {
            ...I
          }
        }
        fragment F on T {
          x: a
          ...G
        }
        fragment G on T {
          y: c
        }
        fragment I on T {
          y: d
          ...J
        }
        fragment J on T {
          x: b
        }
      """,
      List(
        "Field 'field' conflict because subfields 'x' conflict because 'a' and 'b' are different fields and subfields 'y' conflict because 'c' and 'd' are different fields." →
          List(Pos(3, 11), Pos(11, 11), Pos(15, 11), Pos(6, 11), Pos(22, 11), Pos(18, 11))
      ))

    "ignores unknown fragments" in expectPasses(
      """
        {
          field
          ...Unknown
          ...Known
        }

        fragment Known on T {
          field
          ...OtherUnknown
        }
      """)

    "return types must be unambiguous" should {
      lazy val SomeBox: InterfaceType[Unit, Unit] = InterfaceType("SomeBox", () ⇒ fields[Unit, Unit](
        Field("deepBox", OptionType(SomeBox), resolve = _ ⇒ None),
        Field("unrelatedField", OptionType(StringType), resolve = _ ⇒ None)
      ))

      lazy val StringBox: ObjectType[Unit, Unit] = ObjectType("StringBox", interfaces[Unit, Unit](SomeBox), () ⇒ fields[Unit, Unit](
        Field("unrelatedField", OptionType(StringType), resolve = _ ⇒ None),
        Field("deepBox", OptionType(StringBox), resolve = _ ⇒ None),
        Field("scalar", OptionType(StringType), resolve = _ ⇒ None),
        Field("listStringBox", OptionType(ListType(OptionType(StringBox))), resolve = _ ⇒ None),
        Field("stringBox", OptionType(StringBox), resolve = _ ⇒ None),
        Field("intBox", OptionType(IntBox), resolve = _ ⇒ None)
      ))

      lazy val IntBox: ObjectType[Unit, Unit] = ObjectType("IntBox", interfaces[Unit, Unit](SomeBox), () ⇒ fields[Unit, Unit](
        Field("unrelatedField", OptionType(StringType), resolve = _ ⇒ None),
        Field("deepBox", OptionType(IntBox), resolve = _ ⇒ None),
        Field("scalar", OptionType(IntType), resolve = _ ⇒ None),
        Field("listStringBox", OptionType(ListType(OptionType(StringBox))), resolve = _ ⇒ None),
        Field("stringBox", OptionType(StringBox), resolve = _ ⇒ None),
        Field("intBox", OptionType(IntBox), resolve = _ ⇒ None)
      ))

      val NonNullStringBox1 = InterfaceType("NonNullStringBox1", fields[Unit, Unit](
        Field("scalar", StringType, resolve = _ ⇒ "")
      ))

      val NonNullStringBox1Impl = ObjectType("NonNullStringBox1Impl", interfaces[Unit, Unit](SomeBox, NonNullStringBox1), fields[Unit, Unit](
        Field("scalar", StringType, resolve = _ ⇒ ""),
        Field("unrelatedField", OptionType(StringType), resolve = _ ⇒ None),
        Field("deepBox", OptionType(SomeBox), resolve = _ ⇒ None)
      ))

      val NonNullStringBox2 = InterfaceType("NonNullStringBox2", fields[Unit, Unit](
        Field("scalar", StringType, resolve = _ ⇒ "")
      ))

      val NonNullStringBox2Impl = ObjectType("NonNullStringBox2Impl", interfaces[Unit, Unit](SomeBox, NonNullStringBox2), fields[Unit, Unit](
        Field("scalar", StringType, resolve = _ ⇒ ""),
        Field("unrelatedField", OptionType(StringType), resolve = _ ⇒ None),
        Field("deepBox", OptionType(SomeBox), resolve = _ ⇒ None)
      ))

      val Connection = ObjectType("Connection", fields[Unit, Unit](
        Field("edges", OptionType(ListType(OptionType(
          ObjectType("Edge", fields[Unit, Unit](
            Field("node", OptionType(
              ObjectType("Node", fields[Unit, Unit](
                Field("id", OptionType(IDType), resolve = _ ⇒ ""),
                Field("name", OptionType(StringType), resolve = _ ⇒ "")
              ))
            ), resolve = _ ⇒ ())
          ))
        ))), resolve = _ ⇒ Nil)
      ))

      val schema = Schema(ObjectType("QueryRoot", fields[Unit, Unit](
        Field("someBox", OptionType(SomeBox), resolve = _ ⇒ ()),
        Field("connection", OptionType(Connection), resolve = _ ⇒ ())
      )), additionalTypes = IntBox :: StringBox :: NonNullStringBox1 :: NonNullStringBox1Impl :: NonNullStringBox2 :: NonNullStringBox2Impl :: Nil)

      // This is invalid since an object could potentially be both the Object
      // type IntBox and the interface type NonNullStringBox1. While that
      // condition does not exist in the current schema, the schema could
      // expand in the future to allow this. Thus it is invalid.
      "conflicting return types which potentially overlap" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ...on IntBox {
                scalar
              }
              ...on NonNullStringBox1 {
                scalar
              }
            }
          }
        """,
        List(
          "Field 'scalar' conflict because they return conflicting types 'Int' and 'String!'." →
            List(Pos(5, 17), Pos(8, 17))
        ))

      "same wrapped scalar return types" in expectValid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ...on NonNullStringBox1 {
                scalar
              }
              ...on NonNullStringBox2 {
                scalar
              }
            }
          }
        """)

      "allows non-conflicting overlaping types" in expectValid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on IntBox {
                scalar: unrelatedField
              }
              ... on StringBox {
                scalar
              }
            }
          }
        """)

      "compares deep types including list" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            connection {
              ...edgeID
              edges {
                node {
                  id: name
                }
              }
            }
          }

          fragment edgeID on Connection {
            edges {
              node {
                id
              }
            }
          }
        """,
        List(
          "Field 'edges' conflict because subfields 'node' conflict because subfields 'id' conflict because 'name' and 'id' are different fields." →
            List(Pos(5, 15), Pos(6, 17), Pos(7, 19), Pos(14, 13), Pos(15, 15), Pos(16, 17))
        ))

      "ignores unknown types" in expectValid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ...on UnknownType {
                scalar
              }
              ...on NonNullStringBox2 {
                scalar
              }
            }
          }
        """)

      // In this case `deepBox` returns `SomeBox` in the first usage, and
      // `StringBox` in the second usage. These return types are not the same!
      // however this is valid because the return *shapes* are compatible.
      "compatible return shapes on different return types" in expectValid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on SomeBox {
                deepBox {
                  unrelatedField
                }
              }
              ... on StringBox {
                deepBox {
                  unrelatedField
                }
              }
            }
          }
        """)

      "disallows differing return types despite no overlap" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on IntBox {
                scalar
              }
              ... on StringBox {
                scalar
              }
            }
          }
        """,
        List("Field 'scalar' conflict because they return conflicting types 'Int' and 'String'" → List(Pos(5, 17), Pos(8, 17))))

      "reports correctly when a non-exclusive follows an exclusive" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on IntBox {
                deepBox {
                  ...X
                }
              }
            }
            someBox {
              ... on StringBox {
                deepBox {
                  ...Y
                }
              }
            }
            memoed: someBox {
              ... on IntBox {
                deepBox {
                  ...X
                }
              }
            }
            memoed: someBox {
              ... on StringBox {
                deepBox {
                  ...Y
                }
              }
            }
            other: someBox {
              ...X
            }
            other: someBox {
              ...Y
            }
          }
          fragment X on SomeBox {
            scalar
          }
          fragment Y on SomeBox {
            scalar: unrelatedField
          }
        """,
        List("Field 'other' conflict because subfields 'scalar' conflict because 'scalar' and 'unrelatedField' are different fields." →
          List(Pos(31, 13), Pos(39, 13), Pos(34, 13), Pos(42, 13))))

      "disallows differing return type nullability despite no overlap" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on NonNullStringBox1 {
                scalar
              }
              ... on StringBox {
                scalar
              }
            }
          }
        """,
        List("Field 'scalar' conflict because they return conflicting types 'String!' and 'String'" → List(Pos(5, 17), Pos(8, 17))))

      "disallows differing return type list despite no overlap" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on IntBox {
                box: listStringBox {
                  scalar
                }
              }
              ... on StringBox {
                box: stringBox {
                  scalar
                }
              }
            }
          }
        """,
        List("Field 'box' conflict because they return conflicting types '[StringBox]' and 'StringBox'" → List(Pos(5, 17), Pos(10, 17))))

      "disallows differing return type list despite no overlap (reverse)" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on IntBox {
                box: stringBox {
                  scalar
                }
              }
              ... on StringBox {
                box: listStringBox {
                  scalar
                }
              }
            }
          }
        """,
        List("Field 'box' conflict because they return conflicting types 'StringBox' and '[StringBox]'" → List(Pos(5, 17), Pos(10, 17))))

      "disallows differing subfields" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on IntBox {
                box: stringBox {
                  val: scalar
                  val: unrelatedField
                }
              }
              ... on StringBox {
                box: stringBox {
                  val: scalar
                }
              }
            }
          }
        """,
        List(
          "Field 'val' conflict because 'scalar' and 'unrelatedField' are different fields." → List(Pos(6, 19), Pos(7, 19))))

      "disallows differing deep return types despite no overlap" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            someBox {
              ... on IntBox {
                box: stringBox {
                  scalar
                }
              }
              ... on StringBox {
                box: intBox {
                  scalar
                }
              }
            }
          }
        """,
        List("Field 'box' conflict because subfields 'scalar' conflict because they return conflicting types 'String' and 'Int'" →
          List(Pos(5, 17), Pos(6, 19), Pos(10, 17), Pos(11, 19))))
    }
  }
}
