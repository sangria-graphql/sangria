package sangria.schema

import sangria.execution.deferred.Deferred
import sangria.streaming.SubscriptionStream

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Try}
import scala.util.control.NonFatal

trait Action[+Ctx, +Val] {
  def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): Action[Ctx, NewVal]
}
trait LeafAction[+Ctx, +Val] extends Action[Ctx, Val] {
  def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): LeafAction[Ctx, NewVal]
}
sealed trait StandardLeafAction[+Ctx, +Val] extends LeafAction[Ctx, Val]
sealed trait ReduceAction[+Ctx, +Val] extends Action[Ctx, Val] {
  def map[NewVal](fn: Val => NewVal)(implicit ec: ExecutionContext): LeafAction[Ctx, NewVal]
}

object ReduceAction {
  implicit def futureAction[Ctx, Val](value: Future[Val]): ReduceAction[Ctx, Val] = FutureValue(
    value)
  implicit def tryAction[Ctx, Val](value: Try[Val]): ReduceAction[Ctx, Val] = TryValue(value)
  implicit def defaultAction[Ctx, Val](value: Val): ReduceAction[Ctx, Val] = Value(value)
}

object Action extends LowPrioActions {
  def sequence[Ctx, Val](actions: Seq[LeafAction[Ctx, Val]]): SequenceLeafAction[Ctx, Val] =
    SequenceLeafAction[Ctx, Val](actions)

  def apply[Ctx, Val](a: Action[Ctx, Val]): Action[Ctx, Val] = a

  implicit def deferredAction[Ctx, Val](value: Deferred[Val]): LeafAction[Ctx, Val] = DeferredValue(
    value)
  implicit def tryAction[Ctx, Val](value: Try[Val]): LeafAction[Ctx, Val] = TryValue(value)
}

trait LowPrioActions extends LowestPrioActions {
  implicit def deferredFutureAction[Ctx, Val](value: Future[Deferred[Val]]): LeafAction[Ctx, Val] =
    DeferredFutureValue(value)
}

trait LowestPrioActions {
  implicit def futureAction[Ctx, Val](value: Future[Val]): LeafAction[Ctx, Val] = FutureValue(value)
  implicit def defaultAction[Ctx, Val](value: Val): LeafAction[Ctx, Val] = Value(value)
}

object LeafAction {
  def sequence[Ctx, Val](actions: Seq[LeafAction[Ctx, Val]]): SequenceLeafAction[Ctx, Val] =
    SequenceLeafAction[Ctx, Val](actions)

  def apply[Ctx, Val](a: LeafAction[Ctx, Val]): LeafAction[Ctx, Val] = a
}

case class Value[Ctx, Val](value: Val)
    extends StandardLeafAction[Ctx, Val]
    with ReduceAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): LeafAction[Ctx, NewVal] =
    try Value(fn(value))
    catch {
      case NonFatal(e) => TryValue(Failure(e))
    }
}

case class TryValue[Ctx, Val](value: Try[Val])
    extends StandardLeafAction[Ctx, Val]
    with ReduceAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): TryValue[Ctx, NewVal] =
    TryValue(value.map(fn))
}

case class PartialValue[Ctx, Val](value: Val, errors: Vector[Throwable])
    extends StandardLeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): LeafAction[Ctx, NewVal] =
    try PartialValue(fn(value), errors)
    catch {
      case NonFatal(e) => TryValue(Failure(e))
    }
}

case class FutureValue[Ctx, Val](value: Future[Val])
    extends StandardLeafAction[Ctx, Val]
    with ReduceAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): FutureValue[Ctx, NewVal] =
    FutureValue(value.map(fn))
}

case class PartialFutureValue[Ctx, Val](value: Future[PartialValue[Ctx, Val]])
    extends StandardLeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): PartialFutureValue[Ctx, NewVal] =
    PartialFutureValue(value.map(_.map(fn) match {
      case v: PartialValue[Ctx, NewVal] => v
      case TryValue(Failure(e)) => throw e
      case v => throw new IllegalStateException(s"Unexpected result from `PartialValue.map`: $v")
    }))
}

case class DeferredValue[Ctx, Val](value: Deferred[Val]) extends StandardLeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): DeferredValue[Ctx, NewVal] =
    DeferredValue(MappingDeferred(value, (v: Val) => (fn(v), Vector.empty)))

  def mapWithErrors[NewVal](fn: Val => (NewVal, Vector[Throwable])): DeferredValue[Ctx, NewVal] =
    DeferredValue(MappingDeferred(value, fn))
}

case class DeferredFutureValue[Ctx, Val](value: Future[Deferred[Val]])
    extends StandardLeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): DeferredFutureValue[Ctx, NewVal] =
    DeferredFutureValue(value.map(MappingDeferred(_, (v: Val) => (fn(v), Vector.empty))))

  def mapWithErrors[NewVal](fn: Val => (NewVal, Vector[Throwable]))(implicit
      ec: ExecutionContext): DeferredFutureValue[Ctx, NewVal] =
    DeferredFutureValue(value.map(MappingDeferred(_, fn)))
}

case class SequenceLeafAction[Ctx, Val](value: Seq[LeafAction[Ctx, Val]])
    extends StandardLeafAction[Ctx, Seq[Val]] {
  override def map[NewVal](fn: Seq[Val] => NewVal)(implicit
      ec: ExecutionContext): MappedSequenceLeafAction[Ctx, Val, NewVal] =
    new MappedSequenceLeafAction[Ctx, Val, NewVal](this, fn)
}

class MappedSequenceLeafAction[Ctx, Val, NewVal](
    val action: SequenceLeafAction[Ctx, Val],
    val mapFn: Seq[Val] => NewVal)
    extends StandardLeafAction[Ctx, NewVal] {
  override def map[NewNewVal](fn: NewVal => NewNewVal)(implicit
      ec: ExecutionContext): MappedSequenceLeafAction[Ctx, Val, NewNewVal] =
    new MappedSequenceLeafAction[Ctx, Val, NewNewVal](action, v => fn(mapFn(v)))
}

class UpdateCtx[Ctx, Val](val action: LeafAction[Ctx, Val], val nextCtx: Val => Ctx)
    extends Action[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): MappedUpdateCtx[Ctx, Val, NewVal] =
    new MappedUpdateCtx[Ctx, Val, NewVal](action, nextCtx, fn)
}

class MappedUpdateCtx[Ctx, Val, NewVal](
    val action: LeafAction[Ctx, Val],
    val nextCtx: Val => Ctx,
    val mapFn: Val => NewVal)
    extends Action[Ctx, NewVal] {
  override def map[NewNewVal](fn: NewVal => NewNewVal)(implicit
      ec: ExecutionContext): MappedUpdateCtx[Ctx, Val, NewNewVal] =
    new MappedUpdateCtx[Ctx, Val, NewNewVal](action, nextCtx, v => fn(mapFn(v)))
}

object UpdateCtx {
  def apply[Ctx, Val](action: LeafAction[Ctx, Val])(newCtx: Val => Ctx): UpdateCtx[Ctx, Val] =
    new UpdateCtx(action, newCtx)
}

private[sangria] case class SubscriptionValue[Ctx, Val, S[_]](
    source: S[Any],
    stream: SubscriptionStream[S])
    extends StandardLeafAction[Ctx, Val] {
  override def map[NewVal](fn: Val => NewVal)(implicit
      ec: ExecutionContext): SubscriptionValue[Ctx, NewVal, S] =
    throw new IllegalStateException(
      "`map` is not supported subscription actions. Action is only intended for internal use.")
}
