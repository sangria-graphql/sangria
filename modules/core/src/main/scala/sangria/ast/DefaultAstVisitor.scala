package sangria.ast

import sangria.visitor.VisitorCommand

case class DefaultAstVisitor(
    override val onEnter: PartialFunction[AstNode, VisitorCommand] = PartialFunction.empty,
    override val onLeave: PartialFunction[AstNode, VisitorCommand] = PartialFunction.empty
) extends AstVisitor
