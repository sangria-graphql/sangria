package sangria.validation.rules

import scala.language.postfixOps

import sangria.schema.{Named, ListInputType, OptionInputType, InputType}

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.renderer.{SchemaRenderer, QueryRenderer}
import sangria.validation._

import scala.collection.mutable.{Set => MutableSet, Map => MutableMap}

/**
 * Variables passed to field arguments conform to type
 */
class VariablesInAllowedPosition extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val visitSpreadFragments = true

    val varDefs = MutableMap[String, ast.VariableDefinition]()
    val visitedFragmentNames = MutableSet[String]()

    override val onEnter: ValidationVisit = {
      case _: ast.OperationDefinition =>
        varDefs.clear()
        visitedFragmentNames.clear()
        Right(Continue)
      case varDef: ast.VariableDefinition =>
        varDefs(varDef.name) = varDef
        Right(Continue)
      case ast.FragmentSpread(name, _, _) if visitedFragmentNames contains name =>
        Right(Skip)
      case ast.FragmentSpread(name, _, _) =>
        visitedFragmentNames += name
        Right(Continue)
      case ast.VariableValue(name, pos) =>
        val res = for {
          varDef <- varDefs get name
          varTpe <- ctx.schema.getInputType(varDef.tpe)
          inputType <- ctx.typeInfo.inputType
        } yield if (varTypeAllowedForType(effectiveType(varTpe, varDef), inputType))
          Vector.empty
        else
          Vector(BadVarPositionViolation(
            name,
            SchemaRenderer.renderTypeName(varTpe),
            SchemaRenderer.renderTypeName(inputType),
            ctx.sourceMapper,
            pos.toList))

        res match {
          case Some(errors) if errors.nonEmpty => Left(errors)
          case _ => Right(Continue)
        }
    }

    // If a variable definition has a default value, it's effectively non-null.
    def effectiveType(varType: InputType[_], varDef: ast.VariableDefinition) =
      if (varDef.defaultValue.isDefined && varType.isInstanceOf[OptionInputType[_]])
        varType.asInstanceOf[OptionInputType[_]].ofType
      else
        varType

    // A var type is allowed if it is the same or more strict than the expected
    // type. It can be more strict if the variable type is non-null when the
    // expected type is nullable. If both are list types, the variable item type can
    // be more strict than the expected item type.
    def varTypeAllowedForType(varType: InputType[_], expectedType: InputType[_]): Boolean =
      (varType, expectedType) match {
        case (OptionInputType(ofType1), OptionInputType(ofType2)) => varTypeAllowedForType(ofType1, ofType2)
        case (vt, OptionInputType(ofType2)) => varTypeAllowedForType(vt, ofType2)
        case (ListInputType(ofType1), ListInputType(ofType2)) => varTypeAllowedForType(ofType1, ofType2)
        case (t1: Named, t2: Named) => t1 == t2
        case _ => false
      }
  }
}