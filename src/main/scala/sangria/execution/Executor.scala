package sangria.execution

import sangria.ast.Document
import sangria.schema.Schema

object Executor {
  def execute[Ctx, Root](
       schema: Schema[Ctx, Root],
       ast: Document,
       root: Root = (),
       userContext: Ctx = (),
       operationName: Option[String] = None,
       variables: Map[String, Any] = Map.empty,
       marshaller: ResultMarshaller = new SimpleResultMarshaller): ExecutionResult[marshaller.Node] = {
    ExecutionResult(marshaller.booleanNode(true), Nil, marshaller.booleanNode(true))
  }
}

case class ExecutionResult[T](data: T, errors: List[T], result: T)
