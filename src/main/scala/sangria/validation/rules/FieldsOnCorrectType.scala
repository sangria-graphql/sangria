package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema.OptionInputType
import sangria.validation.ValidationContext._
import sangria.validation._

/**
 * Fields on correct type
 *
 * A GraphQL document is only valid if all fields selected are defined by the
 * parent type, or are an allowed meta field such as __typenamme
 */
class FieldsOnCorrectType extends ValidationRule {
   override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
     override val onEnter: ValidationVisit = {
       case ast.Field(_, name, _, _, _, pos) =>
         (ctx.typeInfo.previousParentType, ctx.typeInfo.fieldDef) match {
           case (Some(parent), None) =>
             Left(UndefinedFieldViolation(
               name,
               SchemaRenderer.renderTypeName(parent, topLevel = true),
               ctx.sourceMapper,
               pos))
           case _ =>
             Right(Continue)
         }
     }
   }
 }
