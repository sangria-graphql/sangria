package sangria.validation.rules

import sangria.ast
import sangria.schema
import sangria.ast.AstVisitorCommand
import sangria.validation._

/** For oneOf input objects, exactly one field should be non-null. */
class ExactlyOneOfFieldGiven extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = { case ast.ObjectValue(fields, _, pos) =>
      ctx.typeInfo.inputType match {
        case Some(inputType) =>
          inputType.namedInputType match {
            case schema.InputObjectType(name, _, _, directives, _) if directives.exists { d =>
                  d.name == schema.OneOfDirective.name
                } =>
              val nonNullFields = fields.filter { field =>
                field.value match {
                  case ast.NullValue(_, _) => false
                  case _ => true
                }
              }

              nonNullFields.size match {
                case 1 => AstVisitorCommand.RightContinue
                case _ =>
                  Left(Vector(NotExactlyOneOfField(name, ctx.sourceMapper, pos.toList)))
              }

            case _ => AstVisitorCommand.RightContinue
          }
        case None => AstVisitorCommand.RightContinue
      }
    }
  }
}
