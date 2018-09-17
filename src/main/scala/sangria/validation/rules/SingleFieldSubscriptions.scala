package sangria.validation.rules

import sangria.ast
import sangria.ast.OperationType
import sangria.validation._

/**
  * Subscriptions must only include one field.
  *
  * A GraphQL subscription is valid only if it contains a single root field.
  */
class SingleFieldSubscriptions extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case od: ast.OperationDefinition if od.operationType == OperationType.Subscription && od.selections.size > 1 ⇒
        Left(
          Vector(
            SubscriptionSingleFieldOnlyViolation(
              od.name,
              ctx.sourceMapper,
              od.selections.tail.flatMap(_.location).toList
            )
          )
        )
    }
  }
}
