package sangria.schema

import sangria.ast.AstNode

// backported from 3.0
object AstNodeTransformer {
  def withoutAstLocations[T <: AstNode](node: T, stripComments: Boolean = false): T =
    AstNode.withoutAstLocations(node, stripComments)
}
