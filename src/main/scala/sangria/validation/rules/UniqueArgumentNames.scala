package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.collection.mutable.{Set ⇒ MutableSet}

/**
 * Unique argument names
 *
 * A GraphQL field or directive is only valid if all supplied arguments are
 * uniquely named.
 */
class UniqueArgumentNames extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val knownArgNames = MutableSet[String]()

    override val onEnter: ValidationVisit = {
      case _: ast.Field ⇒
        knownArgNames.clear()
        AstVisitorCommand.RightContinue
      case _: ast.Directive ⇒
        knownArgNames.clear()
        AstVisitorCommand.RightContinue
      case ast.Argument(name, _, _, pos) ⇒
        if (knownArgNames contains name)
          Left(Vector(DuplicateArgNameViolation(name, ctx.sourceMapper, pos.toList)))
        else {
          knownArgNames += name
          AstVisitorCommand.RightContinue
        }
    }
  }
}