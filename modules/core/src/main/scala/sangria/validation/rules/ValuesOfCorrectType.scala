package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.validation._
import sangria.ast.AstVisitorCommand._
import sangria.schema.{EnumType, InputObjectType, ScalarAlias, ScalarType, Type}
import sangria.util.StringUtil

/** Value literals of correct type
  *
  * A GraphQL document is only valid if all value literals are of the type expected at their
  * position.
  */
class ValuesOfCorrectType extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case v: ast.NullValue =>
        ctx.typeInfo.inputType match {
          case Some(tpe) if !tpe.isOptional => badValue(tpe, v)
          case _ => RightContinue
        }

      case v: ast.ListValue =>
        // Note: TypeInfo will traverse into a list's item type, so look to the parent input type to check if it is a list.
        ctx.typeInfo.parentInputType match {
          case Some(tpe) if !tpe.nonOptionalType.isList =>
            isValidScalar(v) match {
              case Right(_) => Right(AstVisitorCommand.Skip)
              case l @ Left(_) => l
            }
          case _ => RightContinue
        }

      case v: ast.ObjectValue =>
        ctx.typeInfo.inputType.map(_.namedType) match {
          case Some(tpe: InputObjectType[_]) =>
            val errors =
              tpe.fields.toVector.flatMap { fieldDef =>
                v.fieldsByName.get(fieldDef.name) match {
                  case None if !fieldDef.fieldType.isOptional && fieldDef.defaultValue.isEmpty =>
                    Vector(
                      RequiredFieldViolation(
                        tpe.name,
                        fieldDef.name,
                        SchemaRenderer.renderTypeName(fieldDef.fieldType),
                        ctx.sourceMapper,
                        v.location.toList))
                  case _ => Vector.empty
                }
              }

            if (errors.nonEmpty) Left(errors)
            else RightContinue

          case Some(_) =>
            isValidScalar(v) match {
              case Right(_) => Right(AstVisitorCommand.Skip)
              case l @ Left(_) => l
            }

          case _ => RightContinue
        }

      case v: ast.ObjectField =>
        (ctx.typeInfo.parentInputType.map(_.namedType), ctx.typeInfo.inputType) match {
          case (Some(tpe: InputObjectType[_]), None) =>
            val suggestions = StringUtil.suggestionList(v.name, tpe.fields.map(_.name))
            val didYouMean =
              if (suggestions.nonEmpty) Some(s"Did you mean ${StringUtil.orList(suggestions)}?")
              else None

            Left(
              Vector(
                UnknownFieldViolation(
                  tpe.name,
                  v.name,
                  didYouMean,
                  ctx.sourceMapper,
                  v.location.toList)))

          case _ => RightContinue
        }

      case v: ast.EnumValue =>
        ctx.typeInfo.inputType.map(_.namedType) match {
          case Some(tpe: EnumType[_]) =>
            tpe.coerceInput(v) match {
              case Left(violation) => badValue(tpe, v, Some(violation))
              case _ => RightContinue
            }

          case _ => isValidScalar(v)
        }

      case v: ast.IntValue => isValidScalar(v)
      case v: ast.BigIntValue => isValidScalar(v)
      case v: ast.FloatValue => isValidScalar(v)
      case v: ast.BigDecimalValue => isValidScalar(v)
      case v: ast.StringValue => isValidScalar(v)
      case v: ast.BooleanValue => isValidScalar(v)
    }

    def badValue(tpe: Type, node: ast.AstNode, violation: Option[Violation] = None) =
      Left(badValueV(SchemaRenderer.renderTypeName(tpe), node, violation))

    def badValueV(tpe: String, node: ast.AstNode, violation: Option[Violation]) =
      Vector(
        BadValueViolation(
          tpe,
          QueryRenderer.render(node),
          violation,
          ctx.sourceMapper,
          node.location.toList
        ))

    def enumTypeSuggestion(tpe: Type, node: ast.Value): Option[Violation] = tpe match {
      case enumT: EnumType[_] =>
        val name = QueryRenderer.render(node)
        val suggestions = StringUtil.suggestionList(name, enumT.values.map(_.name))

        if (suggestions.nonEmpty) Some(EnumValueCoercionViolation(name, enumT.name, suggestions))
        else None

      case _ => None
    }

    def isValidScalar(value: ast.Value) =
      ctx.typeInfo.inputType match {
        case Some(tpe) =>
          tpe.namedInputType match {
            case s: ScalarType[_] =>
              s.coerceInput(value) match {
                case Left(violation) => badValue(tpe, value, Some(violation))
                case _ => RightContinue
              }

            case s: ScalarAlias[_, _] =>
              s.aliasFor.coerceInput(value) match {
                case Left(violation) => badValue(tpe, value, Some(violation))
                case Right(v) =>
                  s.fromScalar(v) match {
                    case Left(violation) => badValue(tpe, value, Some(violation))
                    case _ => RightContinue
                  }
              }

            case t =>
              badValue(tpe, value, enumTypeSuggestion(t, value))
          }

        case _ => RightContinue
      }
  }
}
