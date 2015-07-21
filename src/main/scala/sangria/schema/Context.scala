package sangria.schema

import language.implicitConversions

import sangria.ast

import scala.concurrent.Future
import scala.util.{Failure, Try}

sealed trait Action[+Ctx, +T]

object Action {
  implicit def futureAction[Ctx, Val](value: Future[Val]): Action[Ctx, Val] = FutureValue(value)
  implicit def deferredAction[Ctx, Val](value: Deferred[Val]): Action[Ctx, Val] = DeferredValue(value)
  implicit def deferredFutureAction[Ctx, Val](value: Future[Deferred[Val]]): Action[Ctx, Val] = DeferredFutureValue(value)
  implicit def defaultAction[Ctx, Val](value: Val): Action[Ctx, Val] = Value(value)
}

case class Value[Ctx, Val](value: Val) extends Action[Ctx, Val]
case class FutureValue[Ctx, Val](value: Future[Val]) extends Action[Ctx, Val]
case class DeferredValue[Ctx, Val](value: Deferred[Val]) extends Action[Ctx, Val]
case class DeferredFutureValue[Ctx, Val](value: Future[Deferred[Val]]) extends Action[Ctx, Val]
class UpdateCtx[Ctx, Val](action: Action[Ctx, Val], neCtx: Val => Ctx) extends Action[Ctx, Val]

object UpdateCtx {
  def apply[Ctx, Val](action: Action[Ctx, Val])(newCtx: Val => Ctx): UpdateCtx[Ctx, Val] = new UpdateCtx(action, newCtx)
}

abstract class Projection[Ctx, Val, Res](projectedName: Option[String]) extends (Context[Ctx, Val] => Action[Ctx, Res])

object Projection {
  def apply[Ctx, Val, Res](fn: Context[Ctx, Val] => Action[Ctx, Res]) =
    new Projection[Ctx, Val, Res](None) {
      override def apply(ctx: Context[Ctx, Val]) = fn(ctx)
    }

  def apply[Ctx, Val, Res](projectedName: String, fn: Context[Ctx, Val] => Action[Ctx, Res]) =
    new Projection[Ctx, Val, Res](Some(projectedName)) {
      override def apply(ctx: Context[Ctx, Val]) = fn(ctx)
    }
}

trait Projector[Ctx, Val, Res] extends (Context[Ctx, Val] => Action[Ctx, Res])

object Projector {
  def apply[Ctx, Val, Res](fn: (Context[Ctx, Val], ProjectedName) => Action[Ctx, Res]) =
    new Projector[Ctx, Val, Res] {
      def apply(ctx: Context[Ctx, Val], projected: ProjectedName) = fn(ctx, projected)
      override def apply(ctx: Context[Ctx, Val]) = ??? // it should not be called
    }
}

case class ProjectedName(name: String, children: List[ProjectedName] = Nil)

trait Deferred[+T]

trait WithArguments {
  def args: Map[String, Any]
  def arg[T](arg: Argument[T]) = args(arg.name).asInstanceOf[T]
  def argOpt[T](arg: Argument[T]) = args.get(arg.name).asInstanceOf[T]
}

case class Context[Ctx, Val](
      value: Val,
      ctx: Ctx,
      args: Map[String, Any],
      schema: Schema[Ctx, Val],
      field: Field[Ctx, Val],
      astFields: List[ast.Field]) extends WithArguments

case class DirectiveContext(selection: ast.WithDirectives, directive: Directive, args: Map[String, Any]) extends WithArguments

trait DeferredResolver {
  def resolve(deferred: List[Deferred[Any]]): Future[List[Any]]
}

object NilDeferredResolver extends DeferredResolver {
  override def resolve(deferred: List[Deferred[Any]]) = Future.failed(UnsupportedDeferError)
}

case object UnsupportedDeferError extends Exception
