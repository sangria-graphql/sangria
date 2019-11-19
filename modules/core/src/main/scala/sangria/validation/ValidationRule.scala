package sangria.validation

import sangria.ast.{AstNode, AstVisitorCommand}

trait ValidationRule {
  type ValidationVisit = PartialFunction[AstNode, Either[Vector[Violation], AstVisitorCommand.Value]]

  trait AstValidatingVisitor {
    def onEnter: ValidationVisit = PartialFunction.empty
    def onLeave: ValidationVisit = PartialFunction.empty
  }

  def visitor(ctx: ValidationContext): AstValidatingVisitor
}

