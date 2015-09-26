package sangria.validation.rules

import org.parboiled2.Position

import scala.collection.mutable.{Map => MutableMap}

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.validation._

/**
 * Unique input field names
 *
 * A GraphQL input object value is only valid if all supplied fields are
 * uniquely named.
 */
class UniqueInputFieldNames extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val knownNames = MutableMap[String, Option[Position]]()

    override val onEnter: ValidationVisit = {
      case ast.ObjectValue(fields, _) =>
        knownNames.clear()
        Right(Continue)

      case ast.ObjectField(name, _, pos) =>
        if (knownNames contains name)
          Left(Vector(DuplicateInputFieldViolation(name, ctx.sourceMapper, knownNames(name).toList ++ pos.toList)))
        else {
          knownNames += name -> pos
          Right(Continue)
        }
    }
  }
}