package sangria.validation.rules

import language.postfixOps

import sangria.ast.{AstNode, OperationType}
import sangria.schema.DirectiveLocation

import sangria.ast
import sangria.ast.AstVisitorCommand
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
             getCorrectLocation(ctx.typeInfo.ancestors) match {
               case None ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, None, ctx.sourceMapper, pos.toList)))
               case Some((correctLocation, hint)) if !dir.locations.contains(correctLocation) ⇒
                 Left(Vector(MisplacedDirectiveViolation(name, Some(hint), ctx.sourceMapper, pos.toList)))
               case _ ⇒ AstVisitorCommand.RightContinue
             }
         }
     }

     def getCorrectLocation(ancestors: Seq[AstNode]): Option[(DirectiveLocation.Value, String)] = ancestors.drop(1).head match {
       case op: ast.OperationDefinition if op.operationType == OperationType.Query ⇒
         Some(DirectiveLocation.Query → "query operation")
       case op: ast.OperationDefinition if op.operationType == OperationType.Mutation ⇒
         Some(DirectiveLocation.Mutation → "mutation operation")
       case op: ast.OperationDefinition if op.operationType == OperationType.Subscription ⇒
          Some(DirectiveLocation.Subscription → "subscription operation")

       case _: ast.Field ⇒ Some(DirectiveLocation.Field → "field")
       case _: ast.FragmentDefinition ⇒ Some(DirectiveLocation.FragmentDefinition → "fragment definition")
       case _: ast.FragmentSpread ⇒ Some(DirectiveLocation.FragmentSpread → "fragment spread")
       case _: ast.InlineFragment ⇒ Some(DirectiveLocation.InlineFragment → "inline fragment")

       case _: ast.SchemaDefinition ⇒ Some(DirectiveLocation.Schema → "schema definition")
       case _: ast.ScalarTypeDefinition ⇒ Some(DirectiveLocation.Scalar → "scalar type definition")
       case _: ast.ObjectTypeDefinition ⇒ Some(DirectiveLocation.Object → "object type definition")
       case _: ast.FieldDefinition ⇒ Some(DirectiveLocation.FieldDefinition → "field definition")
       case _: ast.InterfaceTypeDefinition ⇒ Some(DirectiveLocation.Interface → "interface definition")
       case _: ast.UnionTypeDefinition ⇒ Some(DirectiveLocation.Union → "union definition")
       case _: ast.EnumTypeDefinition ⇒ Some(DirectiveLocation.Enum → "enum definition")
       case _: ast.EnumValueDefinition ⇒ Some(DirectiveLocation.EnumValue → "enum value definition")
       case _: ast.InputObjectTypeDefinition ⇒ Some(DirectiveLocation.InputObject → "input object type definition")
       case _: ast.InputValueDefinition ⇒
         ancestors.drop(2).head match {
           case _: ast.InputObjectTypeDefinition ⇒ Some(DirectiveLocation.InputFieldDefinition → "input field definition")
           case x ⇒ Some(DirectiveLocation.ArgumentDefinition → "argument definition")
         }

       case _ ⇒ None
     }
   }
 }