package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._
import sangria.validation.rules.overlappingfields.{CachedCheck, SelectionBuilder, SelectionConflictViolationsBuilder}

/**
  * Overlapping fields can be merged
  *
  * A selection set is only valid if all fields (including spreading any
  * fragments) either correspond to distinct response names or can be merged
  * without ambiguity.
  *
  * The algorithm is described in [[CachedCheck]].
  */
class OverlappingFieldsCanBeMergedFast extends ValidationRule {

  override def visitor(ctx: ValidationContext): AstValidatingVisitor =
    new AstValidatingVisitor {

      private val selectionBuilder: SelectionBuilder = new SelectionBuilder

      override val onEnter: ValidationVisit = {
        case field: ast.Field =>
          val parentType = ctx.typeInfo.previousParentType
          val outputType = parentType.flatMap(ctx.typeInfo.getFieldDef(_, field)).map(_.fieldType)
          selectionBuilder.enterField(parentType, field, outputType)
          AstVisitorCommand.RightContinue

        case fragment: ast.FragmentDefinition =>
          selectionBuilder.enterFragmentDefinition(fragment.name)
          AstVisitorCommand.RightContinue

        case fragmentSpread: ast.FragmentSpread =>
          selectionBuilder.spreadFragment(fragmentSpread.name)
          AstVisitorCommand.RightContinue

        case container: ast.SelectionContainer =>
          selectionBuilder.enterGenericSelectionContainer()
          AstVisitorCommand.RightContinue
      }

      override def onLeave: ValidationVisit = {
        case _: ast.SelectionContainer =>
          selectionBuilder.leaveSelectionContainer()
          AstVisitorCommand.RightContinue

        case _: ast.Document =>
          val roots = selectionBuilder.build()
          val violationsBuilder = new SelectionConflictViolationsBuilder(ctx.sourceMapper)
          val check = new CachedCheck
          roots.foreach { root =>
            check.checkFieldsInSetCanMerge(root, violationsBuilder)
          }
          val violations = violationsBuilder.result()
          if (violations.isEmpty) {
            AstVisitorCommand.RightContinue
          } else {
            Left(violations)
          }
      }
    }
}
