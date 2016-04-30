package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.util.StringUtil
import sangria.validation._

import scala.language.postfixOps

/**
 * Known type names
 *
 * A GraphQL document is only valid if referenced types (specifically
 * variable definitions and fragment conditions) are defined by the type schema.
 */
class KnownTypeNames extends ValidationRule {
  // todo: ignore type definitions when support is added: https://github.com/graphql/graphql-js/commit/812e09d681c2f10d4e5d09f75314e47953eeb7d4
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.NamedType(name, pos) ⇒
        if (!ctx.schema.allTypes.contains(name))
          Left(Vector(UnknownTypeViolation(
            name,
            StringUtil.suggestionList(name, ctx.schema.availableTypeNames),
            ctx.sourceMapper,
            pos.toList)))
        else
          Right(Continue)
    }
  }
}