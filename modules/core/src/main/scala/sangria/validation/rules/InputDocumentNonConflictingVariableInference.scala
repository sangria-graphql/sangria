package sangria.validation.rules

import sangria.ast.AstLocation
import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.SchemaRenderer
import sangria.validation.{ValidationContext, ValidationRule, VariableInferenceViolation}

import scala.collection.mutable

/**
 * All inferred variables within input document should not conflict in it's inferred type
 */
class InputDocumentNonConflictingVariableInference extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
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
            Left(Vector(VariableInferenceViolation(v.name, existing.renderCompact, parentTypeAst.renderCompact, ctx.sourceMapper, v.location.toList ++ otherPos)))
          case None =>
            usedVariables(v.name) = (parentTypeAst, v.location.toList)
            AstVisitorCommand.RightContinue
          case _ => AstVisitorCommand.RightContinue
        }
    }

    override def onLeave = {
      case _: ast.InputDocument =>
        inInputDocument = false
        AstVisitorCommand.RightContinue
    }
  }
}
