package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.validation._

import scala.language.postfixOps

/**
 * Possible fragment spread
 *
 * A fragment spread is only valid if the type condition could ever possibly
 * be true: if there is a non-empty intersection of the possible parent types,
 * and possible types which pass the type condition.
 */
class PossibleFragmentSpreads extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case f: ast.InlineFragment ⇒
        val errors = for {
          tpe ← ctx.typeInfo.tpe
          parent ← ctx.typeInfo.previousParentType
        } yield
          if (!doTypesOverlap(ctx, tpe, parent))
            Vector(TypeIncompatibleAnonSpreadViolation(
              SchemaRenderer.renderTypeName(parent, topLevel = true),
              SchemaRenderer.renderTypeName(tpe, topLevel = true),
              ctx.sourceMapper,
              f.position.toList
            ))
        else Vector.empty

        errors match {
          case Some(errors) if errors.nonEmpty ⇒ Left(errors)
          case _ ⇒ AstVisitorCommand.RightContinue
        }
      case fs: ast.FragmentSpread ⇒
        val errors = for {
          tpe ← getFragmentType(ctx, fs.name)
          parent ← ctx.typeInfo.parentType
        } yield
          if (!doTypesOverlap(ctx, tpe, parent))
            Vector(TypeIncompatibleSpreadViolation(
              fs.name,
              SchemaRenderer.renderTypeName(parent, topLevel = true),
              SchemaRenderer.renderTypeName(tpe, topLevel = true),
              ctx.sourceMapper,
              fs.position.toList
            ))
          else Vector.empty

        errors match {
          case Some(errors) if errors.nonEmpty ⇒ Left(errors)
          case _ ⇒ AstVisitorCommand.RightContinue
        }
    }

    def getFragmentType(ctx: ValidationContext, name: String) =
      ctx.fragments.get(name) flatMap (fd ⇒ ctx.schema.getOutputType(fd.typeCondition, topLevel = true))

    def doTypesOverlap(ctx: ValidationContext, type1: Type, type2: Type) = (type1, type2) match {
      case (t1: Named, t2: Named) if t1.name == t2.name ⇒ true
      case (t1: ObjectType[_, _], t2: ObjectType[_, _]) ⇒ false
      case (t1: ObjectType[_, _], t2: AbstractType) ⇒
        ctx.schema.isPossibleType(t2.name, t1)
      case (t1: AbstractType, t2: ObjectType[_, _]) ⇒
        ctx.schema.isPossibleType(t1.name, t2)
      case (t1: AbstractType, t2: Named) ⇒
        val t1TypeNames = ctx.schema.possibleTypes(t1.name).map(_.name).toSet
        ctx.schema possibleTypes t2.name exists (t ⇒ t1TypeNames.contains(t.name))
      case _ ⇒ false
    }
  }
}