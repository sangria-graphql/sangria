package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.validation.ValidationContext.isValidLiteralValue
import sangria.validation.{BadValueViolation, InvalidInputDocumentViolation, ValidationContext, ValidationRule}

/**
 * Input document needs to confirm to the parent input type
 */
class InputDocumentOfCorrectType extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.InputDocument(values, _, pos, sourceMapper) ⇒
        ctx.typeInfo.inputType.map { inputType ⇒
          val violations = values.flatMap { value ⇒
            isValidLiteralValue(inputType, value, sourceMapper)
              .map(violation ⇒ InvalidInputDocumentViolation(
                SchemaRenderer.renderTypeName(inputType),
                QueryRenderer.render(value),
                violation,
                sourceMapper,
                Nil))
          }

          if (violations.nonEmpty)
            Left(violations)
          else
            AstVisitorCommand.RightContinue
        } getOrElse AstVisitorCommand.RightContinue
    }
  }
}
