package sangria.validation.rules

import language.postfixOps

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.renderer.SchemaRenderer
import sangria.validation._

/**
 * Known directives
 *
 * A GraphQL document is only valid if all `@directives` are known by the
 * schema and legally positioned.
 */
class KnownDirectives extends ValidationRule {
   override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
     override val onEnter: ValidationVisit = {
       case ast.Directive(name, _, pos) ⇒
         ctx.schema.directivesByName.get(name) match {
           case None ⇒
             Left(Vector(UnknownDirectiveViolation(name, ctx.sourceMapper, pos.toList)))
           case Some(dir) ⇒
             (ctx.typeInfo.ancestors drop 1 head) match {
               case _: ast.OperationDefinition if !dir.onOperation ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "operation", ctx.sourceMapper, pos.toList)))
               case _: ast.Field if !dir.onField ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "field", ctx.sourceMapper, pos.toList)))
               case _: ast.FragmentDefinition if !dir.onFragment ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "fragment", ctx.sourceMapper, pos.toList)))
               case _: ast.FragmentSpread if !dir.onFragment ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "fragment", ctx.sourceMapper, pos.toList)))
               case _: ast.InlineFragment if !dir.onFragment ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "fragment", ctx.sourceMapper, pos.toList)))
               case _ ⇒
                 Right(Continue)
             }
         }
     }
   }
 }