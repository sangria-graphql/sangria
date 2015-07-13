package sangria.schema

import language.implicitConversions

import sangria.ast

import scala.concurrent.Future
import scala.util.{Failure, Try}

sealed trait Op[+Ctx, +T]

object Op {
  implicit def defaultFutureOp[Ctx, T](value: => Future[T]): Op[Ctx, T] = FutureValue(() => value)
  implicit def defaultDeferredOp[Ctx, T](value: => Deferred[T]): Op[Ctx, T] = DeferredValue(() => value)
  implicit def defaultOp[Ctx, T](value: => T): Op[Ctx, T] = Value(() => value)
}

case class Value[Ctx, Val](value: () => Val) extends Op[Ctx, Val]
case class FutureValue[Ctx, Val](value: () => Future[Val]) extends Op[Ctx, Val]
case class DeferredValue[Ctx, T](value: () => Deferred[T]) extends Op[Ctx, T]
case class UpdateCtx[Ctx, Val](newCtx: Ctx, op: Op[Ctx, Val]) extends Op[Ctx, Val]

// TODO
//case class FieldMagnet[Ctx, Val]...
//case class FieldLookup[Ctx, Val]...

trait Deferred[+T]

case class Context[Ctx, Val](
      value: Val,
      ctx: Ctx,
      args: Map[String, Any],
      schema: Schema[Ctx, Val],
      field: Field[Ctx, Val],
      fieldAst: ast.Field) {
  def arg[T](arg: Argument[T]) = args(arg.name).asInstanceOf[T]
  def argOpt[T](arg: Argument[T]) = args.get(arg.name).asInstanceOf[T]
}

trait DeferredResolver {
  def resolve(deferred: List[Deferred[_]]): List[Try[Any]]
}

object NilDeferredResolver extends DeferredResolver {
  override def resolve(deferred: List[Deferred[_]]) = deferred map (_ => Failure(UnsupportedDeferError))
}

case object UnsupportedDeferError extends Exception
