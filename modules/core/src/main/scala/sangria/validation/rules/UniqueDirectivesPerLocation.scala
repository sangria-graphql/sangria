package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.collection.mutable.{Map => MutableMap}

/** Unique directive names per location
  *
  * A GraphQL document is only valid if all directives at a given location are uniquely named.
  */
class UniqueDirectivesPerLocation extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    private val repeatableDirectives = ctx.schema.directivesByName.map { case (n, d) =>
      (n, d.repeatable)
    }

    override val onEnter: ValidationVisit = {
      // Many different AST nodes may contain directives. Rather than listing
      // them all, just listen for entering any node, and check to see if it
      // defines any directives.
      case node: ast.WithDirectives =>
        val knownDirectives = MutableMap[String, ast.Directive]()

        val errors = node.directives.foldLeft(Vector.empty[Violation]) {
          case (es, d) if repeatableDirectives.getOrElse(d.name, true) =>
            es
          case (es, d) if knownDirectives.contains(d.name) =>
            es :+ DuplicateDirectiveViolation(
              d.name,
              ctx.sourceMapper,
              knownDirectives(d.name).location.toList ++ d.location.toList)
          case (es, d) =>
            knownDirectives(d.name) = d
            es
        }

        if (errors.nonEmpty) Left(errors)
        else AstVisitorCommand.RightContinue
    }
  }
}
