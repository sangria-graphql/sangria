package sangria.validation.rules

import org.parboiled2.Position

import scala.collection.mutable.{Map ⇒ MutableMap}

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

/**
 * Unique input field names
 *
 * A GraphQL input object value is only valid if all supplied fields are
 * uniquely named.
 */
class UniqueInputFieldNames extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val knownNameStack = ValidatorStack.empty[MutableMap[String, Option[Position]]]
    var knownNames = MutableMap[String, Option[Position]]()

    override val onEnter: ValidationVisit = {
      case ast.ObjectValue(_, _, _) ⇒
        knownNameStack.push(knownNames)
        knownNames = MutableMap[String, Option[Position]]()

        AstVisitorCommand.RightContinue

      case ast.ObjectField(name, _, _, pos) ⇒
        if (knownNames contains name)
          Left(Vector(DuplicateInputFieldViolation(name, ctx.sourceMapper, knownNames(name).toList ++ pos.toList)))
        else {
          knownNames += name → pos
          AstVisitorCommand.RightContinue
        }
    }

    override def onLeave: ValidationVisit = {
      case ast.ObjectValue(_, _, _) ⇒
        knownNames = knownNameStack.pop()

        AstVisitorCommand.RightContinue
    }
  }
}