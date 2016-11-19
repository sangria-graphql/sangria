package sangria.validation.rules

import org.parboiled2.Position

import scala.language.postfixOps

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.collection.mutable.{Map ⇒ MutableMap}

/**
  * Unique variable names
  *
  * A GraphQL operation is only valid if all its variables are uniquely named.
  */
class UniqueVariableNames extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val knownVariableNames = MutableMap[String, List[Position]]()

    override val onEnter: ValidationVisit = {
      case _: ast.OperationDefinition ⇒
        knownVariableNames.clear()
        AstVisitorCommand.RightContinue

      case ast.VariableDefinition(name, _, _, _, pos) ⇒
        knownVariableNames get name match {
          case Some(otherPos) ⇒
            Left(Vector(DuplicateVariableViolation(name, ctx.sourceMapper, otherPos ++ pos.toList)))
          case None ⇒
            knownVariableNames += name → pos.toList
            AstVisitorCommand.RightContinue
        }
    }
  }
}
