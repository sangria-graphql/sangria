package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.language.postfixOps

/**
 * Lone anonymous operation
 *
 * A GraphQL document is only valid if when it contains an anonymous operation
 * (the query short-hand) that it contains only that one operation definition.
 */
class LoneAnonymousOperation extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    var operationCount = 0

    override val onEnter: ValidationVisit = {
      case ast.Document(definitions, _, _, _) ⇒
        operationCount = definitions.count(_.isInstanceOf[ast.OperationDefinition])
        AstVisitorCommand.RightContinue
      case op: ast.OperationDefinition ⇒
        if (op.name.isEmpty && operationCount > 1)
          Left(Vector(AnonOperationNotAloneViolation(ctx.sourceMapper, op.position.toList)))
        else
          AstVisitorCommand.RightContinue
    }
  }
}