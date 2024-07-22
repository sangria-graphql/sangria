package sangria.validation.rules

import sangria.ast
import sangria.execution
import sangria.execution.Trinary.{Defined, NullWithDefault}
import sangria.schema
import sangria.ast.AstVisitorCommand
import sangria.validation._
import sangria.marshalling.CoercedScalaResultMarshaller

/** For oneOf input objects, exactly one field should be non-null. */
class ExactlyOneOfFieldGiven extends ValidationRule {
  private val marshaller = CoercedScalaResultMarshaller.default
  private val oneOfDirectiveName = schema.OneOfDirective.name

  private def hasOneOfDirective(inputObject: schema.InputObjectType[_]) =
    inputObject.astDirectives.exists(_.name == oneOfDirectiveName)

  private def getResolvedVariableValue(
      name: String,
      inputType: schema.InputType[_],
      variableValues: Map[String, execution.VariableValue]
  ): Option[Any] = {
    val variableValue = variableValues.get(name)

    variableValue.map(_.resolve(marshaller, marshaller, inputType)) match {
      case Some(Right(Defined(resolved))) => Some(resolved)
      case Some(Right(NullWithDefault(resolved))) => Some(resolved)
      case _ => None
    }
  }

  private def visitNode(
      ctx: ValidationContext,
      inputType: Option[schema.InputType[_]],
      node: Either[ast.ObjectValue, ast.VariableValue]
  ): Either[Vector[Violation], AstVisitorCommand.Value] =
    inputType.fold(AstVisitorCommand.RightContinue) { inputType =>
      inputType.namedInputType match {
        case namedInputType: schema.InputObjectType[_] if hasOneOfDirective(namedInputType) =>
          val (allFields, nonNullFields) = node match {
            case Left(ast.ObjectValue(fields, _, _)) =>
              val nonNullFields = fields.filter { field =>
                field.value match {
                  case ast.NullValue(_, _) => false
                  case ast.VariableValue(name, _, _) =>
                    val fieldInputType = namedInputType.fieldsByName
                      .get(field.name)
                      .map(_.fieldType)

                    fieldInputType.forall { fieldInputType =>
                      getResolvedVariableValue(name, fieldInputType, ctx.variables).isDefined
                    }
                  case _ => true
                }
              }
              (fields, nonNullFields)

            case Right(ast.VariableValue(name, _, _)) =>
              val variableValue = getResolvedVariableValue(name, namedInputType, ctx.variables)

              try
                variableValue match {
                  case Some(resolved) =>
                    val variableObj = resolved.asInstanceOf[Map[String, Any]]
                    val allFields = variableObj.filter { case (key, _) =>
                      namedInputType.fieldsByName.contains(key)
                    }
                    val nonNullFields = allFields.filter { case (_, v) => v != None }
                    (allFields, nonNullFields)
                  case _ => (Vector.empty, Vector.empty)
                }
              catch {
                // could get this from asInstanceOf failing for unexpected variable type.
                // other validation will cover this problem.
                case _: Throwable => (Vector.empty, Vector.empty)
              }
          }

          (allFields.size, nonNullFields.size) match {
            case (1, 1) => AstVisitorCommand.RightContinue
            case _ =>
              val pos = node.fold(_.location, _.location)
              Left(
                Vector(
                  NotExactlyOneOfField(namedInputType.name, ctx.sourceMapper, pos.toList)
                )
              )
          }
        case _ => AstVisitorCommand.RightContinue
      }
    }

  override def visitor(ctx: ValidationContext): AstValidatingVisitor = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case node: ast.ObjectValue => visitNode(ctx, ctx.typeInfo.inputType, Left(node))
      case node: ast.VariableValue => visitNode(ctx, ctx.typeInfo.inputType, Right(node))
    }
  }
}
