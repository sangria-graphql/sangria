package sangria.validation.rules

import sangria.schema.{InputType, Schema}
import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.marshalling.ToInput
import sangria.renderer.SchemaRenderer
import sangria.validation._

import scala.collection.mutable.{Map => MutableMap}

/** Variables passed to field arguments conform to type
  */
class VariablesInAllowedPosition extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val varDefs = MutableMap[String, ast.VariableDefinition]()

    override val onEnter: ValidationVisit = {
      case _: ast.OperationDefinition =>
        varDefs.clear()
        AstVisitorCommand.RightContinue

      case varDef: ast.VariableDefinition =>
        varDefs(varDef.name) = varDef
        AstVisitorCommand.RightContinue
    }

    override def onLeave: ValidationVisit = { case operation: ast.OperationDefinition =>
      val usages = ctx.documentAnalyzer.getRecursiveVariableUsages(operation)

      // A var type is allowed if it is the same or more strict (e.g. is
      // a subtype of) than the expected type. It can be more strict if
      // the variable type is non-null when the expected type is nullable.
      // If both are list types, the variable item type can be more strict
      // than the expected item type (contravariant).
      val errors = usages.toVector.flatMap { usage =>
        for {
          varDef <- varDefs.get(usage.node.name)
          tpe <- usage.tpe
          inputTpe <- ctx.schema.getInputType(varDef.tpe)
          if !allowedVariableUsage(
            ctx.schema,
            inputTpe,
            varDef.defaultValue,
            tpe,
            usage.defaultValue)
        } yield BadVarPositionViolation(
          usage.node.name,
          SchemaRenderer.renderTypeName(inputTpe),
          SchemaRenderer.renderTypeName(tpe),
          ctx.sourceMapper,
          varDef.location.toList ++ usage.node.location.toList
        )
      }

      if (errors.nonEmpty) Left(errors.distinct) else AstVisitorCommand.RightContinue
    }

    /** Returns true if the variable is allowed in the location it was found,
      * which includes considering if default values exist for either the variable
      * or the location at which it is located.
      */
    def allowedVariableUsage(
        schema: Schema[_, _],
        varType: InputType[_],
        varDefaultValue: Option[ast.Value],
        locationType: InputType[_],
        locationDefaultValue: Option[(_, ToInput[_, _])]
    ) =
      if (!locationType.isOptional && varType.isOptional) {
        val hasNonNullVariableDefaultValue =
          varDefaultValue.exists(default => !default.isInstanceOf[ast.NullValue])
        val hasLocationDefaultValue = locationDefaultValue.isDefined

        if (!hasNonNullVariableDefaultValue && !hasLocationDefaultValue) false
        else TypeComparators.isSubType(schema, varType.nonOptionalType, locationType)
      } else TypeComparators.isSubType(schema, varType, locationType)
  }
}
