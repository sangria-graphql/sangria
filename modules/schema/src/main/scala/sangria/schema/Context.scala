package sangria.schema

import sangria.ast.SourceMapper
import sangria.execution.{DeprecationTracker, ExecutionPath, MiddlewareAttachment}
import sangria.{ast, introspection}
import sangria.marshalling.ResultMarshaller

import scala.reflect.ClassTag

/** The context of a field during schema resolution.
  *
  * When a GraphQL request is executed by a Sangria server, each [[Field field]] in the request is
  * resolved to determine the data that should be returned. An instance of this class provides the
  * context for a particular field's resolution.
  *
  * @param value
  *   The object to which the field belongs.
  * @param ctx
  *   The context object that was passed to Sangria's execution method.
  * @tparam Ctx
  *   Type of the context object that was passed to Sangria's execution method.
  * @tparam Val
  *   Type of the object to which the field belongs.
  */
case class Context[Ctx, Val](
    value: Val,
    ctx: Ctx,
    args: Args,
    schema: Schema[Ctx, Val],
    field: Field[Ctx, Val],
    parentType: ObjectType[Ctx, Any],
    marshaller: ResultMarshaller,
    query: ast.Document,
    sourceMapper: Option[SourceMapper],
    deprecationTracker: DeprecationTracker,
    astFields: Vector[ast.Field],
    path: ExecutionPath,
    deferredResolverState: Any,
    middlewareAttachments: Vector[MiddlewareAttachment] = Vector.empty
) extends WithInputTypeRendering[Ctx]
    with WithArguments {
  def isIntrospection: Boolean = introspection.isIntrospection(parentType, field)

  def attachment[T <: MiddlewareAttachment: ClassTag]: Option[T] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass

    middlewareAttachments.collectFirst {
      case a if clazz.isAssignableFrom(a.getClass) => a.asInstanceOf[T]
    }
  }

  def attachments[T <: MiddlewareAttachment: ClassTag]: Vector[T] = {
    val clazz = implicitly[ClassTag[T]].runtimeClass

    middlewareAttachments.collect {
      case a if clazz.isAssignableFrom(a.getClass) => a.asInstanceOf[T]
    }
  }
}
