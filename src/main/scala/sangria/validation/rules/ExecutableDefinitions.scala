package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

/**
  * Executable definitions
  *
  * A GraphQL document is only valid for execution if all definitions are either
  * operation or fragment definitions.
  */
class ExecutableDefinitions extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    override val onEnter: ValidationVisit = {
      case ast.Document(definitions, _, _, _) ⇒
        val errors =
          definitions.collect {
            case d if !d.isInstanceOf[ast.OperationDefinition] && !d.isInstanceOf[ast.FragmentDefinition] ⇒
              NonExecutableDefinitionViolation(definitionName(d), d, ctx.sourceMapper, d.position.toList)
          }

        if (errors.nonEmpty) Left(errors)
        else AstVisitorCommand.RightContinue
    }
  }
  
  def definitionName(definition: ast.Definition): Option[String] = definition match {
    case d: ast.FragmentDefinition ⇒ Some(d.name)
    case d: ast.OperationDefinition ⇒ d.name
    case d: ast.TypeDefinition ⇒ Some(d.name)
    case d: ast.DirectiveDefinition ⇒ Some(d.name)
    case d: ast.SchemaDefinition ⇒ None
    case d: ast.TypeExtensionDefinition ⇒ Some(d.name)
  }
}