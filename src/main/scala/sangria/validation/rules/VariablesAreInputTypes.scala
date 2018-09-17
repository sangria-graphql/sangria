package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.QueryRenderer
import sangria.validation._

/**
  * Variables are input types
  *
  * A GraphQL operation is only valid if all the variables it defines are of
  * input types (scalar, enum, or input object).
  */
class VariablesAreInputTypes extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.VariableDefinition(name, tpe, _, _, pos) ⇒
        ctx.schema.getInputType(tpe) match {
          case Some(_) ⇒ AstVisitorCommand.RightContinue
          case None ⇒
            Left(
              Vector(NonInputTypeOnVarViolation(name, QueryRenderer.render(tpe), ctx.sourceMapper, tpe.location.toList))
            )
        }
    }
  }
}
