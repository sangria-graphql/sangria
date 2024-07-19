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
  val marshaller = CoercedScalaResultMarshaller.default

  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    private def getResolvedVariableValue(
        name: String,
        inputType: schema.InputType[_],
        variableValues: Map[String, execution.VariableValue]
    ): Option[Any] = {
      val variableValue = ctx.variables.get(name)

      variableValue.map(_.resolve(marshaller, marshaller, inputType)) match {
        case Some(Right(Defined(resolved))) => Some(resolved)
        case Some(Right(NullWithDefault(resolved))) => Some(resolved)
        case _ => None
      }
    }

    private def hasOneOfDirective(inputObject: schema.InputObjectType[_]) =
      inputObject.astDirectives.exists(_.name == schema.OneOfDirective.name)

    private def visitNode(
        inputType: Option[schema.InputType[_]],
        node: Either[ast.ObjectValue, ast.VariableValue]
    ) =
      inputType.fold(AstVisitorCommand.RightContinue) { inputType =>
        inputType.namedInputType match {
          case namedInputType: schema.InputObjectType[_] if hasOneOfDirective(namedInputType) =>
            val pos = node match {
              case Left(node) => node.location
              case Right(node) => node.location
            }

            val nonNullFields = node match {
              case Left(ast.ObjectValue(fields, _, _)) =>
                fields.filter { field =>
                  field.value match {
                    case ast.NullValue(_, _) => false
                    case ast.VariableValue(name, _, _) =>
                      val fieldInputType = namedInputType.fieldsByName
                        .get(field.name)
                        .map(_.fieldType)

                      val variableValue = fieldInputType.flatMap { fieldInputType =>
                        getResolvedVariableValue(name, fieldInputType, ctx.variables)
                      }

                      variableValue.isDefined
                    case _ => true
                  }
                }
              case Right(ast.VariableValue(name, _, _)) =>
                val variableValue = getResolvedVariableValue(name, namedInputType, ctx.variables)

                try
                  variableValue match {
                    case Some(resolved) =>
                      val variableObj = resolved.asInstanceOf[Map[String, Any]]
                      namedInputType.fields.filter { field =>
                        variableObj.get(field.name).fold(false)(_ != None)
                      }
                    case _ => Vector.empty
                  }
                catch {
                  // could get this from asInstanceOf failing for unexpected variable type.
                  // other validation will cover this problem.
                  case _: Throwable => Vector.empty
                }
            }

            nonNullFields.size match {
              case 1 => AstVisitorCommand.RightContinue
              case _ =>
                Left(
                  Vector(
                    NotExactlyOneOfField(namedInputType.name, ctx.sourceMapper, pos.toList)
                  )
                )
            }
          case _ => AstVisitorCommand.RightContinue
        }
      }
    override val onEnter: ValidationVisit = {
      case node: ast.ObjectValue => visitNode(ctx.typeInfo.inputType, Left(node))
      case node: ast.VariableValue => visitNode(ctx.typeInfo.inputType, Right(node))
    }
  }
}
