package sangria.execution

import org.parboiled2.Position
import sangria.DeliveryScheme
import sangria.ast
import sangria.parser.SourceMapper
import sangria.renderer.{SchemaRenderer, QueryRenderer}
import sangria.schema._
import sangria.validation._

import scala.util.{Success, Failure, Try}

case class Executor[Ctx, Root](
    schema: Schema[Ctx, Root],
    root: Root = (),
    userContext: Ctx = (),
    deferredResolver: DeferredResolver = NilDeferredResolver)(implicit marshaller: ResultMarshaller, sangriaScheduler: SangriaScheduler) {

  def execute[Input](
       queryAst: ast.Document,
       operationName: Option[String] = None,
       arguments: Option[Input] = None)(implicit um: InputUnmarshaller[Input], scheme: DeliveryScheme[ExecutionResult[marshaller.Node]]): scheme.Result = {
    val valueExecutor = new ValueExecutor[Input](schema, arguments getOrElse um.emptyNode, queryAst.sourceMapper)(um)

    val foo = for {
      operation <- getOperation(queryAst, operationName)
      variables <- valueExecutor.getVariableValues(operation.variables)
    } yield ExecutionResult(marshaller.booleanNode(true), Nil, marshaller.booleanNode(true))

    foo match {
      case Success(res) => scheme success res
      case Failure(error) => scheme failure error
    }
  }

  def getOperation(document: ast.Document, operationName: Option[String]): Try[ast.OperationDefinition] =
    if (document.operations.size != 1 && operationName.isEmpty)
      Failure(new ExecutionError("Must provide operation name if query contains multiple operations"))
    else {
      val operation = operationName flatMap (opName => document.operations get Some(opName)) orElse document.operations.values.headOption

      operation map (Success(_)) getOrElse Failure(new ExecutionError(s"Unknown operation name: ${operationName.get}"))
    }
}

case class ExecutionResult[T](data: T, errors: List[T], result: T)
