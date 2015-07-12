package sangria.execution

import sangria.ast.Document
import sangria.schema.Schema

object Executor {
  object NilUserContext

  def execute[Ctx](
       schema: Schema[Ctx, _],
       ast: Document,
       marshaller: ResultMarshaller = new SimpleResultMarshaller,
       userContext: Ctx = NilUserContext): marshaller.Node = {
    marshaller.booleanNode(true)
  }
}
