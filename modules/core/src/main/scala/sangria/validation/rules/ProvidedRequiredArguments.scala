package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.SchemaRenderer
import sangria.validation._

/** Provided required arguments
  *
  * A field or directive is only valid if all required (non-null without a default value) field
  * arguments have been provided.
  */
class ProvidedRequiredArguments extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onLeave: ValidationVisit = {
      case ast.Field(_, name, args, _, _, _, _, pos) =>
        ctx.typeInfo.fieldDef match {
          case None => AstVisitorCommand.RightContinue
          case Some(fieldDef) =>
            val astArgs = args.iterator.map(_.name).toSet

            val errors = fieldDef.arguments.iterator.collect {
              case argDef
                  if !astArgs.contains(
                    argDef.name) && !argDef.argumentType.isOptional && argDef.defaultValue.isEmpty =>
                MissingFieldArgViolation(
                  name,
                  argDef.name,
                  SchemaRenderer.renderTypeName(argDef.argumentType),
                  ctx.sourceMapper,
                  pos.toList)
            }.toVector

            if (errors.nonEmpty) Left(errors) else AstVisitorCommand.RightContinue
        }

      case ast.Directive(name, args, _, pos) =>
        ctx.typeInfo.directive match {
          case None => AstVisitorCommand.RightContinue
          case Some(dirDef) =>
            val astArgs = args.iterator.map(_.name).toSet

            val errors = dirDef.arguments.iterator.collect {
              case argDef
                  if !astArgs.contains(
                    argDef.name) && !argDef.argumentType.isOptional && argDef.defaultValue.isEmpty =>
                MissingFieldArgViolation(
                  name,
                  argDef.name,
                  SchemaRenderer.renderTypeName(argDef.argumentType),
                  ctx.sourceMapper,
                  pos.toList)
            }.toVector

            if (errors.nonEmpty) Left(errors) else AstVisitorCommand.RightContinue
        }
    }
  }
}
