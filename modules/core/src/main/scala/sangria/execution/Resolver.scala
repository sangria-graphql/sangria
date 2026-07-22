package sangria.execution

import sangria.ast
import sangria.ast.SourceMapper
import sangria.execution.deferred.{Deferred, DeferredResolver}
import sangria.marshalling.{ResultMarshaller, ScalarValueInfo}
import sangria.schema._

import scala.concurrent.ExecutionContext

private[execution] trait ResolverBuilder {
  def build[Ctx](
      marshaller: ResultMarshaller,
      middlewareCtx: MiddlewareQueryContext[Ctx, _, _],
      schema: Schema[Ctx, _],
      valueCollector: ValueCollector[Ctx, _],
      variables: Map[String, VariableValue],
      fieldCollector: FieldCollector[Ctx, _],
      userContext: Ctx,
      exceptionHandler: ExceptionHandler,
      deferredResolver: DeferredResolver[Ctx],
      sourceMapper: Option[SourceMapper],
      deprecationTracker: Option[DeprecationTracker],
      middleware: List[ApplyedMiddleware[Ctx, Middleware[Ctx]]],
      beforeFieldMiddlewares: List[ApplyedMiddleware[Ctx, MiddlewareBeforeField[Ctx]]],
      maxQueryDepth: Option[Int],
      deferredResolverState: Any,
      preserveOriginalErrors: Boolean,
      validationTiming: TimeMeasurement,
      queryReducerTiming: TimeMeasurement,
      queryAst: ast.Document
  )(implicit
      executionContext: ExecutionContext
  ): Resolver[Ctx]
}

private[execution] trait Resolver[Ctx] {
  val marshaller: ResultMarshaller
  def resolveFieldsPar(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node]

  def resolveFieldsSeq(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node]

  def resolveFieldsSubs(tpe: ObjectType[Ctx, _], value: Any, fields: CollectedFields)(
      scheme: ExecutionScheme): scheme.Result[Ctx, marshaller.Node]
}

case class MappedCtxUpdate[Ctx, Val, NewVal](
    ctxFn: Val => Ctx,
    mapFn: Val => NewVal,
    onError: Throwable => Unit)

object Resolver {
  val DefaultComplexity = 1.0d

  def marshalEnumValue(
      value: String,
      marshaller: ResultMarshaller,
      typeName: String): marshaller.Node =
    marshaller.enumNode(value, typeName)

  def marshalScalarValue(
      value: Any,
      marshaller: ResultMarshaller,
      typeName: String,
      scalarInfo: Set[ScalarValueInfo]): marshaller.Node =
    value match {
      case astValue: ast.Value => marshalAstValue(astValue, marshaller, typeName, scalarInfo)
      case null => marshaller.nullNode
      case _ => marshaller.scalarNode(value, typeName, scalarInfo)
    }

  private def marshalAstValue(
      value: ast.Value,
      marshaller: ResultMarshaller,
      typeName: String,
      scalarInfo: Set[ScalarValueInfo]): marshaller.Node = value match {
    case ast.StringValue(str, _, _, _, _) => marshaller.scalarNode(str, typeName, scalarInfo)
    case ast.IntValue(i, _, _) => marshaller.scalarNode(i, typeName, scalarInfo)
    case ast.BigIntValue(i, _, _) => marshaller.scalarNode(i, typeName, scalarInfo)
    case ast.FloatValue(f, _, _) => marshaller.scalarNode(f, typeName, scalarInfo)
    case ast.BigDecimalValue(f, _, _) => marshaller.scalarNode(f, typeName, scalarInfo)
    case ast.BooleanValue(b, _, _) => marshaller.scalarNode(b, typeName, scalarInfo)
    case ast.NullValue(_, _) => marshaller.nullNode
    case ast.EnumValue(enumT, _, _) => marshaller.enumNode(enumT, typeName)
    case ast.ListValue(values, _, _) =>
      marshaller.arrayNode(values.map(marshalAstValue(_, marshaller, typeName, scalarInfo)))
    case ast.ObjectValue(values, _, _) =>
      marshaller.mapNode(
        values.map(v => v.name -> marshalAstValue(v.value, marshaller, typeName, scalarInfo)))
    case ast.VariableValue(name, _, _) => marshaller.enumNode(name, typeName)
  }
}

trait DeferredWithInfo {
  def deferred: Deferred[Any]
  def complexity: Double
  def field: Field[_, _]
  def astFields: Vector[ast.Field]
  def args: Args
}
