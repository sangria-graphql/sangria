package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
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
      case ast.VariableDefinition(name, _, default, _, _) ⇒
        val defaultForNonNull =
          (ctx.typeInfo.inputType, default) match {
            case (Some(it), Some(d)) if !it.isInstanceOf[OptionInputType[_]] ⇒
              Vector(DefaultForNonNullArgViolation(
                name,
                SchemaRenderer.renderTypeName(it),
                SchemaRenderer.renderTypeName(OptionInputType(it)),
                ctx.sourceMapper,
                d.position.toList))
            case _ ⇒ Vector.empty
          }

        val badValue =
          (ctx.typeInfo.inputType, default) match {
            case (Some(it), Some(defaultValue)) ⇒
              val violations = isValidLiteralValue(it, defaultValue, ctx.sourceMapper)

              if (violations.nonEmpty)
                violations.map(violation ⇒
                  BadValueForDefaultArgViolation(
                    name,
                    SchemaRenderer.renderTypeName(it),
                    QueryRenderer.render(defaultValue),
                    violation,
                    ctx.sourceMapper,
                    defaultValue.position.toList))
              else
                Vector.empty
            case _ ⇒ Vector.empty
          }

        val errors = defaultForNonNull ++ badValue

        if (errors.nonEmpty) Left(errors)
        else AstVisitorCommand.RightContinue
    }
  }
}
