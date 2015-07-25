package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.renderer.SchemaRenderer
import sangria.schema.CompositeType
import sangria.validation.{InlineFragmentOnNonCompositeErrorViolation, FragmentOnNonCompositeErrorViolation, ValidationContext, ValidationRule}

/**
 * Fragments on composite type
 *
 * Fragments use a type condition to determine if they apply, since fragments
 * can only be spread into a composite type (object, interface, or union), the
 * type condition must also be a composite type.
 */
class FragmentsOnCompositeType extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.InlineFragment(cond, _, _, pos) =>
        ctx.typeInfo.tpe match {
          case Some(tpe) if !tpe.isInstanceOf[CompositeType[_]] =>
            Left(InlineFragmentOnNonCompositeErrorViolation(cond, ctx.sourceMapper, pos))
          case _ =>
            Right(Continue)
        }
      case ast.FragmentDefinition(name, cond, _, _, pos) =>
        println(ctx.typeInfo.tpe)
        ctx.typeInfo.tpe match {
          case Some(tpe) if !tpe.isInstanceOf[CompositeType[_]] =>
            Left(FragmentOnNonCompositeErrorViolation(name, cond, ctx.sourceMapper, pos))
          case _ =>
            Right(Continue)
        }
    }
  }
}
