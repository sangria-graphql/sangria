package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.util.StringUtil
import sangria.validation._

/**
 * Known type names
 *
 * A GraphQL document is only valid if referenced types (specifically
 * variable definitions and fragment conditions) are defined by the type schema.
 */
class KnownTypeNames extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case _: ast.ObjectTypeDefinition | _: ast.InterfaceTypeDefinition | _: ast.UnionTypeDefinition | _: ast.InputObjectTypeDefinition | _: ast.TypeSystemExtensionDefinition | _: ast.SchemaDefinition =>
        // When validating SDL, at the moment schema does not know about these types.
        // All type names are validated in the schema materializer as new schema is constructed.
        AstVisitorCommand.RightSkip
      case ast.NamedType(name, pos) =>
        if (!ctx.schema.allTypes.contains(name))
          Left(Vector(UnknownTypeViolation(
            name,
            StringUtil.suggestionList(name, ctx.schema.availableTypeNames),
            ctx.sourceMapper,
            pos.toList)))
        else
          AstVisitorCommand.RightContinue
    }
  }
}
