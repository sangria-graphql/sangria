package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._
import sangria.validation.rules.overlappingfields.{
  CachedCheck,
  SelectionBuilder,
  SelectionConflictViolationsBuilder,
  SelectionContainer
}

import java.util.function.Consumer

/** Overlapping fields can be merged
  *
  * A selection set is only valid if all fields (including spreading any fragments) either
  * correspond to distinct response names or can be merged without ambiguity.
  *
  * The algorithm is described in [[CachedCheck]].
  *
  * The validation rule implements the same validation as OverlappingFieldsCanBeMerged and could
  * eventually replace it. It is an alternative, more performant implementation.
  */
class OverlappingFieldsCanBeMerged extends ValidationRule {

  override def visitor(ctx: ValidationContext): AstValidatingVisitor =
    new AstValidatingVisitor {

      private val selectionBuilder: SelectionBuilder = new SelectionBuilder

      override val onEnter: ValidationVisit = visits(
        on[ast.Field] { case field =>
          val parentType = ctx.typeInfo.previousParentType
          // TypeInfo.enter already resolved the field definition of this field
          val outputType = ctx.typeInfo.fieldDef.map(_.fieldType)
          selectionBuilder.enterField(parentType, field, outputType)
          AstVisitorCommand.RightContinue
        },
        on[ast.FragmentDefinition] { case fragment =>
          selectionBuilder.enterFragmentDefinition(fragment.name)
          AstVisitorCommand.RightContinue
        },
        on[ast.FragmentSpread] { case fragmentSpread =>
          selectionBuilder.spreadFragment(fragmentSpread.name)
          AstVisitorCommand.RightContinue
        },
        on[ast.SelectionContainer] { case container =>
          selectionBuilder.enterGenericSelectionContainer()
          AstVisitorCommand.RightContinue
        }
      )

      override val onLeave: ValidationVisit = visits(
        on[ast.SelectionContainer] { case _ =>
          selectionBuilder.leaveSelectionContainer()
          AstVisitorCommand.RightContinue
        },
        on[ast.Document] { case _ =>
          val roots = selectionBuilder.build()
          val violationsBuilder = new SelectionConflictViolationsBuilder(ctx.sourceMapper)
          val check = new CachedCheck
          roots.forEach {
            new Consumer[SelectionContainer] {
              override def accept(root: SelectionContainer): Unit =
                check.checkFieldsInSetCanMerge(root.fieldSet, violationsBuilder)
            }
          }
          val violations = violationsBuilder.result()
          if (violations.isEmpty) {
            AstVisitorCommand.RightContinue
          } else {
            Left(violations)
          }
        }
      )
    }
}

case class ConflictReason(fieldName: String, reason: Either[String, Vector[ConflictReason]])
