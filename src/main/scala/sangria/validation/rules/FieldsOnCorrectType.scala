package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.SchemaRenderer
import sangria.schema._
import sangria.util.StringUtil
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
      case ast.Field(_, name, _, _, _, _, _, pos) ⇒
        (ctx.typeInfo.previousParentType, ctx.typeInfo.fieldDef) match {
          case (Some(parent), None) ⇒
            val suggestedTypeNames = collectSuggestedTypes(parent, name)
            val suggestedFieldNames =
              if (suggestedTypeNames.nonEmpty) Vector.empty
              else collectSuggestedFieldNames(ctx.schema, parent, name)

            Left(Vector(UndefinedFieldViolation(
              name,
              SchemaRenderer.renderTypeName(parent, topLevel = true),
              suggestedTypeNames,
              suggestedFieldNames,
              ctx.sourceMapper,
              pos.toList)))
          case _ ⇒
            AstVisitorCommand.RightContinue
        }
    }

    def collectSuggestedFieldNames(schema: Schema[_, _], tpe: CompositeType[_], fieldName: String) =
      tpe match {
        case obj: ObjectLikeType[_, _] ⇒ StringUtil.suggestionList(fieldName, obj.fields map (_.name))
        case _ ⇒ Vector.empty
      }

    /**
      * Go through all of the implementations of type, as well as the interfaces
      * that they implement. If any of those types include the provided field,
      * suggest them, sorted by how often the type is referenced,  starting
      * with Interfaces.
      */
    private def collectSuggestedTypes(tpe: CompositeType[_], fieldName: String) =
      tpe match {
        case a: AbstractType ⇒
          siblingInterfacesIncludingField(a, fieldName) ++ implementationsIncludingField(a, fieldName)
        case _ ⇒ Vector.empty
      }

    /**
      * Go through all of the implementations of type, and find other interfaces
      * that they implement. If those interfaces include `field` as a valid field,
      * return them, sorted by how often the implementations include the other
      * interface.
      */
    private def siblingInterfacesIncludingField(tpe: AbstractType, fieldName: String) =
      ctx.schema.possibleTypes(tpe.name)
        .foldLeft(Map.empty[String, Int]) {
          case (oacc, obj) ⇒ obj.interfaces.foldLeft(oacc) {
            case (iacc, i) if i.getField(ctx.schema, fieldName).isEmpty ⇒ iacc
            case (iacc, i) if iacc contains i.name ⇒ iacc.updated(i.name, iacc(i.name) + 1)
            case (iacc, i) ⇒ iacc + (i.name → 1)
          }
        }
        .toVector
        .sortBy(-_._2)
        .map(_._1)


    private def implementationsIncludingField(tpe: AbstractType, fieldName: String) =
      ctx.schema.possibleTypes(tpe.name)
        .filter(_.getField(ctx.schema, fieldName).nonEmpty)
        .map(_.name)
        .sorted
  }
}
