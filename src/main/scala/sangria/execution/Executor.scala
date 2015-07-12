package sangria.execution

import sangria.DeliveryScheme
import sangria.ast.{OperationDefinition, Document}
import sangria.schema.Schema

import scala.util.{Success, Failure, Try}

object Executor {
  def execute[Ctx, Root](
       schema: Schema[Ctx, Root],
       queryAst: Document,
       root: Root = (),
       userContext: Ctx = (),
       operationName: Option[String] = None,
       variables: Map[String, Any] = Map.empty,
       marshaller: ResultMarshaller = new SimpleResultMarshaller)(implicit scheme: DeliveryScheme[ExecutionResult[marshaller.Node]]): scheme.Result  = {

    val foo = for {
      operation <- getOperation(queryAst, operationName)
    } yield ExecutionResult(marshaller.booleanNode(true), Nil, marshaller.booleanNode(true))

    foo match {
      case Success(res) => scheme success res
      case Failure(error) => scheme failure error
    }
  }

  def getOperation(document: Document, operationName: Option[String]): Try[OperationDefinition] = {
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(ExecutionError("Must provide operation name if query contains multiple operations"))
    else {
      val operation = operationName flatMap (opName => document.operations get Some(opName)) orElse document.operations.values.headOption

      operation map (Success(_)) getOrElse Failure(ExecutionError(s"Unknown operation name: ${operationName.get}"))
    }
  }

  def getVariableValues(document: Document, operationName: Option[String]): Try[OperationDefinition] = {
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(ExecutionError("Must provide operation name if query contains multiple operations"))
    else {
      val operation = operationName flatMap (opName => document.operations get Some(opName)) orElse document.operations.values.headOption

      operation map (Success(_)) getOrElse Failure(ExecutionError(s"Unknown operation name: ${operationName.get}"))
    }
  }
}

case class ExecutionResult[T](data: T, errors: List[T], result: T)
