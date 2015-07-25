package sangria.validation.rules

import sangria.renderer.{SchemaRenderer, QueryRenderer}
import sangria.validation.ValidationContext.isValidLiteralValue
import sangria.validation.{BadValueViolation, ValidationContext, ValidationRule}
import sangria.ast
import sangria.ast.AstVisitorCommand._

/**
 * Argument values of correct type
 *
 * A GraphQL document is only valid if all field argument literal values are
 * of the type expected by their position.
 */
class ArgumentsOfCorrectType extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.Argument(name, value, _) =>
        ctx.typeInfo.argument.map { arg =>
          if (!isValidLiteralValue(arg.inputValueType, value))
            Left(BadValueViolation(
              arg.name,
              SchemaRenderer.renderTypeName(arg.inputValueType),
              QueryRenderer.render(value),
              ctx.sourceMapper,
              value.position))
          else
            Right(Continue)
        } getOrElse Right(Continue)
    }
  }
}
