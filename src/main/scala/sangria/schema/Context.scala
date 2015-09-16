package sangria.schema

import sangria.execution.{Resolver, ResultMarshaller}

import language.implicitConversions

import sangria.ast

import scala.concurrent.{ExecutionContext, Future}

sealed trait Action[+Ctx, +Val] {
  def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): Action[Ctx, NewVal]
}
sealed trait LeafAction[+Ctx, +Val] extends Action[Ctx, Val] {
  def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): LeafAction[Ctx, NewVal]
}

object Action {
  implicit def futureAction[Ctx, Val](value: Future[Val]): LeafAction[Ctx, Val] = FutureValue(value)
  implicit def deferredAction[Ctx, Val](value: Deferred[Val]): LeafAction[Ctx, Val] = DeferredValue(value)
  implicit def deferredFutureAction[Ctx, Val, D <: Deferred[Val]](value: Future[D])(implicit ev: D <:< Deferred[Val]): LeafAction[Ctx, Val] = DeferredFutureValue(value)
  implicit def defaultAction[Ctx, Val](value: Val): LeafAction[Ctx, Val] = Value(value)
}

case class Value[Ctx, Val](value: Val) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): Value[Ctx, NewVal] =
    Value(fn(value))
}

case class FutureValue[Ctx, Val](value: Future[Val]) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): FutureValue[Ctx, NewVal] =
    FutureValue(value map fn)
}

case class DeferredValue[Ctx, Val](value: Deferred[Val]) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): DeferredValue[Ctx, NewVal] =
    DeferredValue(MappingDeferred(value, fn))
}

case class DeferredFutureValue[Ctx, Val](value: Future[Deferred[Val]]) extends LeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): DeferredFutureValue[Ctx, NewVal] =
    DeferredFutureValue(value map (MappingDeferred(_, fn)))
}

class UpdateCtx[Ctx, Val](val action: LeafAction[Ctx, Val], val nextCtx: Val => Ctx) extends Action[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): MappedUpdateCtx[Ctx, Val, NewVal] =
    new MappedUpdateCtx[Ctx, Val, NewVal](action, nextCtx, fn)
}

class MappedUpdateCtx[Ctx, Val, NewVal](val action: LeafAction[Ctx, Val], val nextCtx: Val => Ctx, val mapFn: Val => NewVal) extends Action[Ctx, NewVal] {
  override def map[NewNewVal](fn: NewVal => NewNewVal)(implicit ec: ExecutionContext): MappedUpdateCtx[Ctx, Val, NewNewVal] =
    new MappedUpdateCtx[Ctx, Val, NewNewVal](action, nextCtx, v => fn(mapFn(v)))
}

object UpdateCtx {
  def apply[Ctx, Val](action: LeafAction[Ctx, Val])(newCtx: Val => Ctx): UpdateCtx[Ctx, Val] = new UpdateCtx(action, newCtx)
}

abstract class NoProjection[Ctx, Val, Res] extends (Context[Ctx, Val] => Action[Ctx, Res])

object NoProjection {
  def apply[Ctx, Val, Res](fn: Context[Ctx, Val] => Action[Ctx, Res]) =
    new NoProjection[Ctx, Val, Res] {
      override def apply(ctx: Context[Ctx, Val]) = fn(ctx)
    }
}

abstract class Projection[Ctx, Val, Res](val projectedName: String, val fn: Context[Ctx, Val] => Action[Ctx, Res]) extends (Context[Ctx, Val] => Action[Ctx, Res])

object Projection {
  def apply[Ctx, Val, Res](projectedName: String, fn: Context[Ctx, Val] => Action[Ctx, Res]) =
    new Projection[Ctx, Val, Res](projectedName, fn) {
      override def apply(ctx: Context[Ctx, Val]) = fn(ctx)
    }
}

trait Projector[Ctx, Val, Res] extends (Context[Ctx, Val] => Action[Ctx, Res]) {
  val maxLevel: Int = Integer.MAX_VALUE
  def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]): Action[Ctx, Res]
}

object Projector {
  def apply[Ctx, Val, Res](fn: (Context[Ctx, Val], Vector[ProjectedName]) => Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]) = fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = throw new IllegalStateException("Default apply should not be called on projector!")
    }

  def apply[Ctx, Val, Res](levels: Int, fn: (Context[Ctx, Val], Vector[ProjectedName]) => Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      override val maxLevel = levels
      def apply(ctx: Context[Ctx, Val], projected: Vector[ProjectedName]) = fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = throw new IllegalStateException("Default apply should not be called on projector!")
    }
}

case class ProjectedName(name: String, children: Vector[ProjectedName] = Vector.empty) {
  lazy val asVector = {
    def loop(name: ProjectedName): Vector[Vector[String]] =
      Vector(name.name) +: (name.children flatMap loop map (name.name +: _))

    loop(this)
  }
}

trait Deferred[+T]

case class MappingDeferred[A, +B](deferred: Deferred[A], mapFn: A => B) extends Deferred[B]

trait WithArguments {
  def args: Map[String, Any]
  def arg[T](arg: Argument[T]) = args(arg.name).asInstanceOf[T]
  def arg[T](name: String) = args(name).asInstanceOf[T]
  def argOpt[T](arg: Argument[T]) = args.get(arg.name).asInstanceOf[Option[T]]
  def argOpt[T](name: String) = args.get(name).asInstanceOf[Option[T]]
}

trait WithInputTypeRendering {
  def marshaller: ResultMarshaller

  def renderInputValueCompact[T](value: T, tpe: InputType[T], m: ResultMarshaller = marshaller): String =
    m.renderCompact(renderInputValue(value, tpe, m))

  def renderInputValuePretty[T](value: T, tpe: InputType[T], m: ResultMarshaller = marshaller): String =
    m.renderPretty(renderInputValue(value, tpe, m))

  def renderInputValue[T](value: T, tpe: InputType[T], m: ResultMarshaller = marshaller): m.Node = {
    def loop(t: InputType[_], v: Any): m.Node = t match {
      case _ if value == null => m.nullNode
      case s: ScalarType[Any @unchecked] => Resolver.marshalValue(s.coerceOutput(v), m)
      case e: EnumType[Any @unchecked] => Resolver.marshalValue(e.coerceOutput(v), m)
      case io: InputObjectType[_] =>
        val mapValue = v.asInstanceOf[Map[String, Any]]

        io.fields.foldLeft(m.emptyMapNode) {
          case (acc, field) if mapValue contains field.name =>
            m.addMapNodeElem(acc, field.name, loop(field.fieldType, mapValue(field.name)))
          case (acc, _) => acc
        }
      case l: ListInputType[_] =>
        val listValue = v.asInstanceOf[Seq[Any]]

        listValue.foldLeft(m.emptyArrayNode) {
          case (acc, value) => m.addArrayNodeElem(acc, loop(l.ofType, value))
        }
      case o: OptionInputType[_] => v match {
        case Some(optVal) => loop(o.ofType, optVal)
        case None => m.nullNode
        case other => loop(o.ofType, other)
      }
    }

    loop(tpe, value)
  }
}

case class Context[Ctx, Val](
  value: Val,
  ctx: Ctx,
  args: Map[String, Any],
  schema: Schema[Ctx, Val],
  field: Field[Ctx, Val],
  parentType: ObjectType[Ctx, Any],
  marshaller: ResultMarshaller,
  astFields: List[ast.Field]) extends WithArguments with WithInputTypeRendering

case class DirectiveContext(selection: ast.WithDirectives, directive: Directive, args: Map[String, Any]) extends WithArguments

trait DeferredResolver {
  def resolve(deferred: List[Deferred[Any]]): List[Future[Any]]
}

object DeferredResolver {
  val empty = new DeferredResolver {
    override def resolve(deferred: List[Deferred[Any]]) = deferred map (_ => Future.failed(UnsupportedDeferError))
  }
}

case object UnsupportedDeferError extends Exception
