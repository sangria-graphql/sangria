package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.collection.mutable.{Set ⇒ MutableSet}

/**
 * Unique operation names
 *
 * A GraphQL document is only valid if all defined operations have unique names.
 */
class UniqueOperationNames extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val knownOpNames = MutableSet[String]()

    override val onEnter: ValidationVisit = {
      case ast.OperationDefinition(_, Some(name), _, _, _, _, _, pos) ⇒
        if (knownOpNames contains name)
          Left(Vector(DuplicateOperationNameViolation(name, ctx.sourceMapper, pos.toList)))
        else {
          knownOpNames += name
          AstVisitorCommand.RightContinue
        }
    }
  }
}