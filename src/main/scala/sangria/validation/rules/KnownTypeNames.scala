package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.validation._

import scala.language.postfixOps

/**
 * Known type names
 *
 * A GraphQL document is only valid if referenced types (specifically
 * variable definitions and fragment conditions) are defined by the type schema.
 */
class KnownTypeNames extends ValidationRule {
   override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
     override val onEnter: ValidationVisit = {
       case ast.NamedType(name, pos) =>
         if (!ctx.schema.allTypes.contains(name))
           Left(UnknownTypeViolation(name, ctx.sourceMapper, pos))
         else
           Right(Continue)
     }
   }
 }