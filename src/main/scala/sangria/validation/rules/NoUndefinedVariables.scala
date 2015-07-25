package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.validation._
import scala.collection.mutable.{Set => MutableSet}

import scala.language.postfixOps

/**
 * No undefined variables
 *
 * A GraphQL operation is only valid if all variables encountered, both directly
 * and via fragment spreads, are defined by that operation.
 */
class NoUndefinedVariables extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val visitSpreadFragments = true

    var operation: Option[ast.OperationDefinition] = None
    val visitedFragmentNames = MutableSet[String]()
    val definedVariableNames = MutableSet[String]()

    override val onEnter: ValidationVisit = {
      case o: ast.OperationDefinition =>
        operation = Some(o)
        visitedFragmentNames.clear()
        definedVariableNames.clear()
        Right(Continue)
      case v: ast.VariableDefinition =>
        definedVariableNames += v.name
        Right(Continue)
      case fs: ast.FragmentSpread =>
        if (visitedFragmentNames contains fs.name)
          Right(Skip)
        else {
          visitedFragmentNames += fs.name
          Right(Continue)
        }
      case ast.VariableValue(name, pos) =>
        if (!definedVariableNames.contains(name)) {
          val withinFragment = ctx.typeInfo.ancestors.exists(_.isInstanceOf[ast.FragmentDefinition])
          val opName = operation.flatMap(_.name)

          Left(Vector(
            if (withinFragment && opName.isDefined)
              UndefinedVarByOpViolation(name, opName.get, ctx.sourceMapper, pos.toList ++ operation.flatMap(_.position).toList)
            else
              UndefinedVarViolation(name, ctx.sourceMapper, pos.toList)))
        } else Right(Continue)
     }
  }
}