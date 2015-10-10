package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.validation._

import scala.collection.mutable.{ListBuffer, Map ⇒ MutableMap, Set ⇒ MutableSet}
import scala.language.postfixOps

/**
 * No unused variables
 *
 * A GraphQL operation is only valid if all variables defined by an operation
 * are used, either directly or within a spread fragment.
 */
class NoUnusedVariables extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val visitSpreadFragments = true

    val visitedFragmentNames = MutableSet[String]()
    val variableDefs = ListBuffer[ast.VariableDefinition]()
    val variableNameUsed = MutableSet[String]()

    override val onEnter: ValidationVisit = {
      case o: ast.OperationDefinition ⇒
        visitedFragmentNames.clear()
        variableDefs.clear()
        variableNameUsed.clear()
        Right(Continue)
      case vd: ast.VariableDefinition ⇒
        variableDefs += vd

        // Do not visit deeper, or else the defined variable name will be visited.
        Right(Skip)
      case vv: ast.VariableValue ⇒
        variableNameUsed += vv.name
        Right(Continue)
      case fs: ast.FragmentSpread if visitedFragmentNames contains fs.name ⇒
        Right(Skip)
      case fs: ast.FragmentSpread ⇒
        visitedFragmentNames += fs.name
        Right(Continue)
     }

    override def onLeave: ValidationVisit = {
      case o: ast.OperationDefinition ⇒
        val errors = variableDefs.toVector
          .filter(vd ⇒ !variableNameUsed.contains(vd.name))
          .map(vd ⇒ UnusedVariableViolation(vd.name, ctx.sourceMapper, vd.position.toList))

        if (errors.nonEmpty) Left(errors) else Right(Continue)
    }
  }
}