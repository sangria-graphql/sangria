package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.util.StringUtil
import sangria.validation._
import sangria.validation.rules.KnownTypeNames.SuggestionFunction

/** Known type names
  *
  * A GraphQL document is only valid if referenced types (specifically variable definitions and
  * fragment conditions) are defined by the type schema.
  */
class KnownTypeNames(suggestion: SuggestionFunction = SuggestionFunction.Default)
    extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    // When validating SDL, at the moment schema does not know about these types.
    // All type names are validated in the schema materializer as new schema is constructed.
    private val skipDefinition: ValidationVisit = { case _ => AstVisitorCommand.RightSkip }

    override val onEnter: ValidationVisit = visits(
      on[ast.ObjectTypeDefinition](skipDefinition),
      on[ast.InterfaceTypeDefinition](skipDefinition),
      on[ast.UnionTypeDefinition](skipDefinition),
      on[ast.InputObjectTypeDefinition](skipDefinition),
      on[ast.TypeSystemExtensionDefinition](skipDefinition),
      on[ast.SchemaDefinition](skipDefinition),
      on[ast.NamedType] { case ast.NamedType(name, pos) =>
        if (!ctx.schema.allTypes.contains(name))
          Left(
            Vector(
              UnknownTypeViolation(
                name,
                suggestion(name, ctx.schema.availableTypeNames),
                ctx.sourceMapper,
                pos.toList)))
        else
          AstVisitorCommand.RightContinue
      }
    )
  }
}

object KnownTypeNames {
  trait SuggestionFunction {
    def apply(input: String, availableTypeNames: Vector[String]): Seq[String]
  }
  object SuggestionFunction {
    object Default extends SuggestionFunction {
      override def apply(input: String, availableTypeNames: Vector[String]): Seq[String] =
        StringUtil.suggestionList(input, availableTypeNames)
    }
    object Disabled extends SuggestionFunction {
      override def apply(input: String, availableTypeNames: Vector[String]): Seq[String] = Nil
    }
  }
}
