package sangria.validation.rules

import sangria.ast.OperationType
import sangria.schema.DirectiveLocation

import language.postfixOps

import sangria.ast
import sangria.ast.AstVisitorCommand._
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
       case ast.Directive(name, _, _, pos) ⇒
         ctx.schema.directivesByName.get(name) match {
           case None ⇒
             Left(Vector(UnknownDirectiveViolation(name, ctx.sourceMapper, pos.toList)))
           case Some(dir) ⇒
             (ctx.typeInfo.ancestors drop 1 head) match {
               case op: ast.OperationDefinition if op.operationType == OperationType.Query && !dir.locations.contains(DirectiveLocation.Query) ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "query operation", ctx.sourceMapper, pos.toList)))
               case op: ast.OperationDefinition if op.operationType == OperationType.Mutation && !dir.locations.contains(DirectiveLocation.Mutation) ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "mutation operation", ctx.sourceMapper, pos.toList)))
               case op: ast.OperationDefinition if op.operationType == OperationType.Subscription && !dir.locations.contains(DirectiveLocation.Subscription) ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "subscription operation", ctx.sourceMapper, pos.toList)))
               case _: ast.Field if !dir.locations.contains(DirectiveLocation.Field) ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "field", ctx.sourceMapper, pos.toList)))
               case _: ast.FragmentDefinition if !dir.locations.contains(DirectiveLocation.FragmentDefinition) ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "fragment definition", ctx.sourceMapper, pos.toList)))
               case _: ast.FragmentSpread if !dir.locations.contains(DirectiveLocation.FragmentSpread) ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "fragment spread", ctx.sourceMapper, pos.toList)))
               case _: ast.InlineFragment if !dir.locations.contains(DirectiveLocation.InlineFragment) ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, "inline fragment", ctx.sourceMapper, pos.toList)))
               case _ ⇒
                 Right(Continue)
             }
         }
     }
   }
 }