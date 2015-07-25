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
       case ast.Directive(name, _, pos) =>
         ctx.schema.directivesByName.get(name) match {
           case None =>
             Left(UnknownDirectiveViolation(name, ctx.sourceMapper, pos))
           case Some(dir) =>
             (ctx.typeInfo.ancestors drop 1 head) match {
               case _: ast.OperationDefinition if !dir.onOperation =>
                 Left(MisplacedDirectiveViolation(name, "operation", ctx.sourceMapper, pos))
               case _: ast.Field if !dir.onField =>
                 Left(MisplacedDirectiveViolation(name, "field", ctx.sourceMapper, pos))
               case _: ast.FragmentDefinition if !dir.onFragment =>
                 Left(MisplacedDirectiveViolation(name, "fragment", ctx.sourceMapper, pos))
               case _: ast.FragmentSpread if !dir.onFragment =>
                 Left(MisplacedDirectiveViolation(name, "fragment", ctx.sourceMapper, pos))
               case _: ast.InlineFragment if !dir.onFragment =>
                 Left(MisplacedDirectiveViolation(name, "fragment", ctx.sourceMapper, pos))
               case _ =>
                 Right(Continue)
             }
         }
     }
   }
 }