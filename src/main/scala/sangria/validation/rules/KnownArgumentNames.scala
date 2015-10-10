package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.renderer.{QueryRenderer, SchemaRenderer}
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
      case ast.Argument(name, _, pos) ⇒
        ctx.typeInfo.ancestors.drop(1).head match {
          case _: ast.Field ⇒
            ctx.typeInfo.fieldDef match {
              case Some(field) if !field.arguments.exists(_.name == name) ⇒
                Left(Vector(UnknownArgViolation(
                  name,
                  field.name,
                  ctx.typeInfo.previousParentType.fold("")(SchemaRenderer.renderTypeName(_, topLevel = true)),
                  ctx.sourceMapper,
                  pos.toList)))
              case _ ⇒
                Right(Continue)
            }

          case _: ast.Directive ⇒
            ctx.typeInfo.directive match {
              case Some(dir) if !dir.arguments.exists(_.name == name) ⇒
                Left(Vector(UnknownDirectiveArgViolation(
                  name,
                  dir.name,
                  ctx.sourceMapper,
                  pos.toList)))
              case _ ⇒
                Right(Continue)
            }

          case _ ⇒
            Right(Continue)
        }
    }
  }
}