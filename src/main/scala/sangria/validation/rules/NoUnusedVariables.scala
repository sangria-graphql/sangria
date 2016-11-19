package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.collection.mutable.ListBuffer
import scala.language.postfixOps

/**
 * No unused variables
 *
 * A GraphQL operation is only valid if all variables defined by an operation
 * are used, either directly or within a spread fragment.
 */
class NoUnusedVariables extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val variableDefs = ListBuffer[ast.VariableDefinition]()

    override val onEnter: ValidationVisit = {
      case _: ast.OperationDefinition ⇒
        variableDefs.clear()
        AstVisitorCommand.RightContinue

      case varDef: ast.VariableDefinition ⇒
        variableDefs += varDef
        AstVisitorCommand.RightContinue
    }

    override def onLeave: ValidationVisit = {
      case operation: ast.OperationDefinition ⇒
        val usages = ctx.getRecursiveVariableUsages(operation)
        val variableNameUsed = usages.map(_.node.name).toSet

        val errors = variableDefs.filterNot(vd ⇒ variableNameUsed.contains(vd.name)).toVector.map(vd ⇒
          UnusedVariableViolation(vd.name, operation.name, ctx.sourceMapper, vd.position.toList))

        if (errors.nonEmpty) Left(errors.distinct) else AstVisitorCommand.RightContinue
    }
  }
}