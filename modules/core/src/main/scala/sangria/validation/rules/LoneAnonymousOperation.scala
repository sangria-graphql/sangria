package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

/** Lone anonymous operation
  *
  * A GraphQL document is only valid if when it contains an anonymous operation (the query
  * short-hand) that it contains only that one operation definition.
  */
class LoneAnonymousOperation extends ValidationRule {
  override def visitor(ctx: ValidationContext): AstValidatingVisitor = new AstValidatingVisitor {
    var operationCount = 0

    override val onEnter: ValidationVisit = visits(
      on[ast.Document] { case ast.Document(definitions, _, _, _) =>
        operationCount = definitions.count(_.isInstanceOf[ast.OperationDefinition])
        AstVisitorCommand.RightContinue
      },
      on[ast.OperationDefinition] { case op =>
        if (op.name.isEmpty && operationCount > 1)
          Left(Vector(AnonOperationNotAloneViolation(ctx.sourceMapper, op.location.toList)))
        else
          AstVisitorCommand.RightContinue
      }
    )
  }
}
