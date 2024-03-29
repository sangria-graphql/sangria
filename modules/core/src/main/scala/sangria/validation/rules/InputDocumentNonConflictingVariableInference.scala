package sangria.validation.rules

import sangria.ast.AstLocation
import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.validation.{ValidationContext, ValidationRule, VariableInferenceViolation}

import scala.collection.mutable

/** All inferred variables within input document should not conflict in it's inferred type
  */
class InputDocumentNonConflictingVariableInference extends ValidationRule {
  override def visitor(ctx: ValidationContext): AstValidatingVisitor = new AstValidatingVisitor {
    private var inInputDocument = false
    private val usedVariables = new mutable.HashMap[String, (ast.Type, List[AstLocation])]

    override val onEnter: ValidationVisit = {
      case _: ast.InputDocument =>
        inInputDocument = true
        AstVisitorCommand.RightContinue

      case v: ast.VariableValue if inInputDocument && ctx.typeInfo.inputType.isDefined =>
        val parentType = ctx.typeInfo.inputType.get
        val parentTypeAst = SchemaRenderer.renderTypeNameAst(parentType)

        usedVariables.get(v.name) match {
          case Some((existing, otherPos)) if existing != parentTypeAst =>
            Left(
              Vector(
                VariableInferenceViolation(
                  variableName = v.name,
                  type1 = QueryRenderer.renderCompact(existing),
                  type2 = QueryRenderer.renderCompact(parentTypeAst),
                  sourceMapper = ctx.sourceMapper,
                  locations = v.location.toList ++ otherPos
                )))
          case None =>
            usedVariables(v.name) = (parentTypeAst, v.location.toList)
            AstVisitorCommand.RightContinue
          case _ => AstVisitorCommand.RightContinue
        }
    }

    override def onLeave: ValidationVisit = { case _: ast.InputDocument =>
      inInputDocument = false
      AstVisitorCommand.RightContinue
    }
  }
}
