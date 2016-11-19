package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
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
      case ast.InlineFragment(Some(cond), _, _, _, _, pos) ⇒
        ctx.typeInfo.tpe match {
          case Some(tpe) if !tpe.isInstanceOf[CompositeType[_]] ⇒
            Left(Vector(InlineFragmentOnNonCompositeErrorViolation(cond.name, ctx.sourceMapper, cond.position.toList)))
          case _ ⇒
            AstVisitorCommand.RightContinue
        }
      case ast.FragmentDefinition(name, cond, _, _, _, _, pos) ⇒
        ctx.typeInfo.tpe match {
          case Some(tpe) if !tpe.isInstanceOf[CompositeType[_]] ⇒
            Left(Vector(FragmentOnNonCompositeErrorViolation(name, cond.name, ctx.sourceMapper, cond.position.toList)))
          case _ ⇒
            AstVisitorCommand.RightContinue
        }
    }
  }
}
