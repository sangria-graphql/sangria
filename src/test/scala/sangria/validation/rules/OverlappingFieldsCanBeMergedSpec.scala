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
        "Field 'fido' conflict because 'name' and 'nickname' are different fields." -> List(Pos(3, 11), Pos(4, 11))
      ))

    "Alias masking direct field access" in expectFailsPosList(
      """
        fragment aliasMaskingDirectFieldAccess on Dog {
          name: nickname
          name
        }
      """,
      List(
        "Field 'name' conflict because 'nickname' and 'name' are different fields." -> List(Pos(3, 11), Pos(4, 11))
      ))

    "conflicting args" in expectFailsPosList(
      """
        fragment conflictingArgs on Dog {
          doesKnowCommand(dogCommand: SIT)
          doesKnowCommand(dogCommand: HEEL)
        }
      """,
      List(
        "Field 'doesKnowCommand' conflict because they have differing arguments." -> List(Pos(3, 11), Pos(4, 11))
      ))

    "conflicting directives" in expectFailsPosList(
      """
        fragment conflictingDirectiveArgs on Dog {
          name @include(if: true)
          name @skip(if: false)
        }
      """,
      List(
        "Field 'name' conflict because they have differing directives." -> List(Pos(3, 11), Pos(4, 11))
      ))

    "conflicting directive args" in expectFailsPosList(
      """
        fragment conflictingDirectiveArgs on Dog {
          name @include(if: true)
          name @include(if: false)
        }
      """,
      List(
        "Field 'name' conflict because they have differing directives." -> List(Pos(3, 11), Pos(4, 11))
      ))

    "conflicting args with matching directives" in expectFailsPosList(
      """
        fragment conflictingArgsWithMatchingDirectiveArgs on Dog {
          doesKnowCommand(dogCommand: SIT) @include(if: true)
          doesKnowCommand(dogCommand: HEEL) @include(if: true)
        }
      """,
      List(
        "Field 'doesKnowCommand' conflict because they have differing arguments." -> List(Pos(3, 11), Pos(4, 11))
      ))

    "conflicting directives with matching args" in expectFailsPosList(
      """
        fragment conflictingDirectiveArgsWithMatchingArgs on Dog {
          doesKnowCommand(dogCommand: SIT) @include(if: true)
          doesKnowCommand(dogCommand: SIT) @skip(if: false)
        }
      """,
      List(
        "Field 'doesKnowCommand' conflict because they have differing directives." -> List(Pos(3, 11), Pos(4, 11))
      ))

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
        "Field 'x' conflict because 'a' and 'b' are different fields." -> List(Pos(7, 11), Pos(10, 11))
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
        "Field 'x' conflict because 'a' and 'b' are different fields." -> List(Pos(18, 11), Pos(21, 11)),
        "Field 'x' conflict because 'a' and 'c' are different fields." -> List(Pos(18, 11), Pos(14, 13)),
        "Field 'x' conflict because 'b' and 'c' are different fields." -> List(Pos(21, 11), Pos(14, 13))
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
        "Field 'field' conflict because subfields 'x' conflict because 'a' and 'b' are different fields." -> List(Pos(3, 11), Pos(6, 11), Pos(4, 13), Pos(7, 13))
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
        "Field 'field' conflict because subfields 'y' conflict because 'c' and 'd' are different fields and subfields 'x' conflict because 'a' and 'b' are different fields." ->
          List(Pos(3, 11), Pos(7, 11), Pos(5, 13), Pos(9, 13), Pos(4, 13), Pos(8, 13))
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
        "Field 'field' conflict because subfields 'deepField' conflict because subfields 'x' conflict because 'a' and 'b' are different fields." ->
          List(Pos(3, 11), Pos(8, 11), Pos(4, 13), Pos(9, 13), Pos(5, 15), Pos(10, 15))
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
        "Field 'deepField' conflict because subfields 'x' conflict because 'a' and 'b' are different fields." ->
          List(Pos(4, 13), Pos(7, 13), Pos(5, 15), Pos(8, 15))
      ))

    "return types must be unambiguous" should {
      val StringBox = ObjectType("StringBox", fields[Unit, Unit](
        Field("scalar", OptionType(StringType), resolve = _ => None)
      ))

      val IntBox = ObjectType("IntBox", fields[Unit, Unit](
        Field("scalar", OptionType(IntType), resolve = _ => None)
      ))

      val NonNullStringBox1 = ObjectType("NonNullStringBox1", fields[Unit, Unit](
        Field("scalar", StringType, resolve = _ => "")
      ))

      val NonNullStringBox2 = ObjectType("NonNullStringBox2", fields[Unit, Unit](
        Field("scalar", StringType, resolve = _ => "")
      ))

      val BoxUnion = UnionType("BoxUnion", types = StringBox :: IntBox :: NonNullStringBox1 :: NonNullStringBox2 :: Nil)

      val Connection = ObjectType("NonNullStringBox2", fields[Unit, Unit](
        Field("edges", OptionType(ListType(OptionType(
          ObjectType("Edge", fields[Unit, Unit](
            Field("node", OptionType(
              ObjectType("Node", fields[Unit, Unit](
                Field("id", OptionType(IDType), resolve = _ => ""),
                Field("name", OptionType(StringType), resolve = _ => "")
              ))
            ), resolve = _ => ())
          ))
        ))), resolve = _ => Nil)
      ))

      val schema = Schema(ObjectType("QueryRoot", fields[Unit, Unit](
        Field("boxUnion", OptionType(BoxUnion), resolve = _ => ()),
        Field("connection", OptionType(Connection), resolve = _ => ())
      )))

      "conflicting scalar return types" in expectInvalid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            boxUnion {
              ...on IntBox {
                scalar
              }
              ...on StringBox {
                scalar
              }
            }
          }
        """,
        List(
          "Field 'scalar' conflict because they return differing types 'Int' and 'String'." ->
            List(Pos(5, 17), Pos(8, 17))
        ))

      "same wrapped scalar return types" in expectValid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            boxUnion {
              ...on NonNullStringBox1 {
                scalar
              }
              ...on NonNullStringBox2 {
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
          "Field 'edges' conflict because subfields 'node' conflict because subfields 'id' conflict because 'id' and 'name' are different fields." ->
            List(Pos(14, 13), Pos(5, 15), Pos(15, 15), Pos(6, 17), Pos(16, 17), Pos(7, 19))
        ))

      "ignores unknown types" in expectValid(schema, new OverlappingFieldsCanBeMerged :: Nil,
        """
          {
            boxUnion {
              ...on UnknownType {
                scalar
              }
              ...on NonNullStringBox2 {
                scalar
              }
            }
          }
        """)
    }
  }
}
