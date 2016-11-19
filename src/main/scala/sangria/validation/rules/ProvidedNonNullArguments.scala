package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.SchemaRenderer
import sangria.schema.OptionInputType
import sangria.validation._

import scala.language.postfixOps

/**
 * Provided required arguments
 *
 * A field or directive is only valid if all required (non-null) field arguments
 * have been provided.
 */
class ProvidedNonNullArguments extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onLeave: ValidationVisit = {
      case ast.Field(_, name, args, _, _, _, _, pos) ⇒
        ctx.typeInfo.fieldDef match {
          case None ⇒ AstVisitorCommand.RightContinue
          case Some(fieldDef) ⇒
            val astArgs = args.map(_.name).toSet

            val errors = fieldDef.arguments.toVector.collect {
              case argDef if !astArgs.contains(argDef.name) && !argDef.argumentType.isInstanceOf[OptionInputType[_]] ⇒
                MissingFieldArgViolation(name, argDef.name, SchemaRenderer.renderTypeName(argDef.argumentType), ctx.sourceMapper, pos.toList)
            }

            if (errors.nonEmpty) Left(errors) else AstVisitorCommand.RightContinue
        }

      case ast.Directive(name, args, _, pos) ⇒
        ctx.typeInfo.directive match {
          case None ⇒ AstVisitorCommand.RightContinue
          case Some(dirDef) ⇒
            val astArgs = args.map(_.name).toSet

            val errors = dirDef.arguments.toVector.collect {
              case argDef if !astArgs.contains(argDef.name) && !argDef.argumentType.isInstanceOf[OptionInputType[_]] ⇒
                MissingFieldArgViolation(name, argDef.name, SchemaRenderer.renderTypeName(argDef.argumentType), ctx.sourceMapper, pos.toList)
            }

            if (errors.nonEmpty) Left(errors) else AstVisitorCommand.RightContinue
        }
    }
  }
}