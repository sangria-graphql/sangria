package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema.OptionInputType
import sangria.validation.ValidationContext._
import sangria.validation._

/**
  * Variable's default value is allowed
  *
  * A GraphQL document is only valid if all variable default values are allowed
  * due to a variable not being required.
  */
class VariablesDefaultValueAllowed extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.VariableDefinition(name, _, default, _, _) ⇒
        (ctx.typeInfo.inputType, default) match {
          case (Some(it), Some(d)) if !it.isOptional ⇒
            Left(Vector(DefaultForNonNullArgViolation(
              name,
              SchemaRenderer.renderTypeName(it),
              SchemaRenderer.renderTypeName(OptionInputType(it)),
              ctx.sourceMapper,
              d.location.toList)))

          case _ ⇒ AstVisitorCommand.RightContinue
        }
    }
  }
}
