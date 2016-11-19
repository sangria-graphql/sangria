package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._
import scala.collection.mutable.{Set ⇒ MutableSet}

import scala.language.postfixOps

/**
 * No undefined variables
 *
 * A GraphQL operation is only valid if all variables encountered, both directly
 * and via fragment spreads, are defined by that operation.
 */
class NoUndefinedVariables extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val variableNameDefined   = MutableSet[String]()

    override val onEnter: ValidationVisit = {
      case _: ast.OperationDefinition ⇒
        variableNameDefined.clear()
        AstVisitorCommand.RightContinue

      case varDef: ast.VariableDefinition ⇒
        variableNameDefined += varDef.name
        AstVisitorCommand.RightContinue
    }

    override def onLeave: ValidationVisit = {
      case operation: ast.OperationDefinition ⇒
        val usages = ctx.getRecursiveVariableUsages(operation)

        val errors = usages.filterNot(vu ⇒ variableNameDefined.contains(vu.node.name)).toVector.map { vu ⇒
          operation.name match {
            case Some(opName) ⇒
              UndefinedVarByOpViolation(vu.node.name, opName, ctx.sourceMapper, vu.node.position.toList ++ operation.position.toList)
            case None ⇒
              UndefinedVarViolation(vu.node.name, ctx.sourceMapper, vu.node.position.toList ++ operation.position.toList)
          }
        }

        if (errors.nonEmpty) Left(errors.distinct) else AstVisitorCommand.RightContinue
    }
  }
}