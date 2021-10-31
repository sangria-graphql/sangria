package sangria.schema

import sangria.ast.{AstLocation, AstNode, Comment}
import sangria.visitor.{Visit, VisitAnyField, VisitorCommand, visit}

object AstNodeTransformer {
  def withoutAstLocations[T <: AstNode](node: T, stripComments: Boolean = false): T = {
    val enterComment = (_: Comment) =>
      if (stripComments) VisitorCommand.Delete else VisitorCommand.Continue

    visit[AstNode](
      node,
      Visit[Comment](enterComment),
      VisitAnyField[AstNode, Option[AstLocation]]((_, _) => VisitorCommand.Transform(None)))
      .asInstanceOf[T]
  }
}
