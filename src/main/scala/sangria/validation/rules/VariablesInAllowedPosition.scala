package sangria.validation.rules

import scala.language.postfixOps

import sangria.schema.{OptionInputType, InputType}

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.SchemaRenderer
import sangria.validation._

import scala.collection.mutable.{Map ⇒ MutableMap}

/**
 * Variables passed to field arguments conform to type
 */
class VariablesInAllowedPosition extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val varDefs = MutableMap[String, ast.VariableDefinition]()

    override val onEnter: ValidationVisit = {
      case _: ast.OperationDefinition ⇒
        varDefs.clear()
        AstVisitorCommand.RightContinue

      case varDef: ast.VariableDefinition ⇒
        varDefs(varDef.name) = varDef
        AstVisitorCommand.RightContinue
    }

    override def onLeave: ValidationVisit = {
      case operation: ast.OperationDefinition ⇒
        val usages = ctx.getRecursiveVariableUsages(operation)

        // A var type is allowed if it is the same or more strict (e.g. is
        // a subtype of) than the expected type. It can be more strict if
        // the variable type is non-null when the expected type is nullable.
        // If both are list types, the variable item type can be more strict
        // than the expected item type (contravariant).
        val errors = usages.toVector.flatMap { usage ⇒
          for {
            varDef ← varDefs.get(usage.node.name)
            tpe ← usage.tpe
            inputTpe ← ctx.schema.getInputType(varDef.tpe)
            if !TypeComparators.isSubType(ctx.schema, effectiveType(inputTpe, varDef), tpe)
          } yield BadVarPositionViolation(
            usage.node.name,
            SchemaRenderer.renderTypeName(inputTpe),
            SchemaRenderer.renderTypeName(tpe),
            ctx.sourceMapper,
            varDef.position.toList ++ usage.node.position.toList)
        }

        if (errors.nonEmpty) Left(errors.distinct) else AstVisitorCommand.RightContinue
    }

    // If a variable definition has a default value, it's effectively non-null.
    def effectiveType(varType: InputType[_], varDef: ast.VariableDefinition) =
      if (varDef.defaultValue.isDefined && varType.isInstanceOf[OptionInputType[_]])
        varType.asInstanceOf[OptionInputType[_]].ofType
      else
        varType
  }
}