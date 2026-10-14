package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.collection.mutable.ListBuffer

/** No unused variables
  *
  * A GraphQL operation is only valid if all variables defined by an operation are used, either
  * directly or within a spread fragment.
  */
class NoUnusedVariables extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val variableDefs = ListBuffer[ast.VariableDefinition]()

    override val onEnter: ValidationVisit = visits(
      on[ast.OperationDefinition] { case _ =>
        variableDefs.clear()
        AstVisitorCommand.RightContinue
      },
      on[ast.VariableDefinition] { case varDef =>
        variableDefs += varDef
        AstVisitorCommand.RightContinue
      }
    )

    override val onLeave: ValidationVisit = visits(
      on[ast.OperationDefinition] { case operation =>
        val usages = ctx.documentAnalyzer.getRecursiveVariableUsages(operation)
        val variableNameUsed = usages.map(_.node.name).toSet

        val errors = variableDefs
          .filterNot(vd => variableNameUsed.contains(vd.name))
          .toVector
          .map(vd =>
            UnusedVariableViolation(vd.name, operation.name, ctx.sourceMapper, vd.location.toList))

        if (errors.nonEmpty) Left(errors.distinct) else AstVisitorCommand.RightContinue
      }
    )
  }
}
