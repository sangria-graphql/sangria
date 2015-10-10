package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema.OptionInputType
import sangria.validation.ValidationContext._
import sangria.validation._

/**
 * Variable default values of correct type
 *
 * A GraphQL document is only valid if all variable default values are of the
 * type expected by their definition.
 */
class DefaultValuesOfCorrectType extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.VariableDefinition(name, _, default, _) ⇒
        (ctx.typeInfo.inputType, default) match {
          case (Some(it), Some(d)) if !it.isInstanceOf[OptionInputType[_]] ⇒
            Left(Vector(DefaultForNonNullArgViolation(
              name,
              SchemaRenderer.renderTypeName(it),
              SchemaRenderer.renderTypeName(OptionInputType(it)),
              ctx.sourceMapper,
              d.position.toList)))
          case (Some(it), Some(defaultValue)) if !isValidLiteralValue(it, defaultValue) ⇒
            Left(Vector(BadValueForDefaultArgViolation(
              name,
              SchemaRenderer.renderTypeName(it),
              QueryRenderer.render(defaultValue),
              ctx.sourceMapper,
              defaultValue.position.toList)))
          case _ ⇒
            Right(Continue)
        }
    }
  }
}
