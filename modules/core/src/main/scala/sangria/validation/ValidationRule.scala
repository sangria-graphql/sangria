package sangria.validation

import sangria.ast.{AstNode, AstVisitorCommand}

trait ValidationRule {
  type ValidationVisit =
    PartialFunction[AstNode, Either[Vector[Violation], AstVisitorCommand.Value]]

  trait AstValidatingVisitor {
    def onEnter: ValidationVisit = PartialFunction.empty
    def onLeave: ValidationVisit = PartialFunction.empty

    /** The classes of the nodes that `onEnter` can match (subclasses included), or `None` if it can
      * match any node. Declaring them lets the validator skip the visitor for other nodes. The
      * declaration must not be narrower than what `onEnter` matches, and must be the same for all
      * the visitors of a class (the validator caches it per class).
      */
    def onEnterNodes: Option[Vector[Class[_]]] = None

    /** Same as `onEnterNodes`, for `onLeave`. */
    def onLeaveNodes: Option[Vector[Class[_]]] = None
  }

  def visitor(ctx: ValidationContext): AstValidatingVisitor
}
