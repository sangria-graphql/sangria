package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.util.StringUtil
import sangria.validation._

/**
 * Known argument names
 *
 * A GraphQL field is only valid if all supplied arguments are defined by
 * that field.
 */
class KnownArgumentNames extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.Argument(name, _, _, pos) ⇒
        ctx.typeInfo.ancestors.drop(1).head match {
          case _: ast.Field ⇒
            ctx.typeInfo.fieldDef match {
              case Some(field) if !field.arguments.exists(_.name == name) ⇒
                Left(Vector(UnknownArgViolation(
                  name,
                  field.name,
                  ctx.typeInfo.previousParentType.fold("")(SchemaRenderer.renderTypeName(_, topLevel = true)),
                  StringUtil.suggestionList(name, field.arguments map (_.name)),
                  ctx.sourceMapper,
                  pos.toList)))
              case _ ⇒
                AstVisitorCommand.RightContinue
            }

          case _: ast.Directive ⇒
            ctx.typeInfo.directive match {
              case Some(dir) if !dir.arguments.exists(_.name == name) ⇒
                Left(Vector(UnknownDirectiveArgViolation(
                  name,
                  dir.name,
                  StringUtil.suggestionList(name, dir.arguments map (_.name)),
                  ctx.sourceMapper,
                  pos.toList)))
              case _ ⇒
                AstVisitorCommand.RightContinue
            }

          case _ ⇒
            AstVisitorCommand.RightContinue
        }
    }
  }
}