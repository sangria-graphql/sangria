package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.language.postfixOps

/**
 * Known fragment names
 *
 * A GraphQL document is only valid if all `...Fragment` fragment spreads refer
 * to fragments defined in the same document.
 */
class KnownFragmentNames extends ValidationRule {
   override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
     override val onEnter: ValidationVisit = {
       case ast.FragmentSpread(name, _, _, pos) ⇒
         ctx.fragments.get(name) match {
           case None ⇒ Left(Vector(UnknownFragmentViolation(name, ctx.sourceMapper, pos.toList)))
           case _ ⇒ AstVisitorCommand.RightContinue
         }
     }
   }
 }