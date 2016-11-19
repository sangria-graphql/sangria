package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.collection.mutable.{Set ⇒ MutableSet}

/**
 * Unique fragment names
 *
 * A GraphQL document is only valid if all defined fragments have unique names.
 */
class UniqueFragmentNames extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val knownFragmentNames = MutableSet[String]()

    override val onEnter: ValidationVisit = {
      case fragDef: ast.FragmentDefinition ⇒
        if (knownFragmentNames contains fragDef.name)
          Left(Vector(DuplicateFragmentNameViolation(fragDef.name, ctx.sourceMapper, fragDef.position.toList)))
        else {
          knownFragmentNames += fragDef.name
          AstVisitorCommand.RightContinue
        }
    }
  }
}