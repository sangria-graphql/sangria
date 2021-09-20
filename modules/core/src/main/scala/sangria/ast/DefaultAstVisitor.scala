package sangria.ast

import sangria.visitor.VisitorCommand

case class DefaultAstVisitor(
    override val onEnter: PartialFunction[AstNode, VisitorCommand] = { case _ =>
      VisitorCommand.Continue
    },
    override val onLeave: PartialFunction[AstNode, VisitorCommand] = { case _ =>
      VisitorCommand.Continue
    }
) extends AstVisitor
