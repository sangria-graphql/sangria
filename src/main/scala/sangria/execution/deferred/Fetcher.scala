package sangria.execution.deferred

import scala.collection.mutable.{Set ⇒ MutableSet}
import scala.concurrent.Future
import scala.util.Try

class Fetcher[Ctx, Id, Res](val idFn: Res ⇒ Id, val fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], val maxBatchSize: Option[Int], val cache: Option[() ⇒ FetcherCache]) {
  def get(id: Id) = FetcherDeferredOne(this, id)
  def getOpt(id: Id) = FetcherDeferredOpt(this, id)
  def getSeq(ids: Seq[Id]) = FetcherDeferredSeq(this, ids)
  def getSeqOpt(ids: Seq[Id]) = FetcherDeferredSeqOpt(this, ids)

  def ids(deferred: Vector[Deferred[Any]]): Vector[Id] = {
    val allIds =  MutableSet[Id]()

    deferred foreach {
      case FetcherDeferredOne(s, id) if s eq this ⇒ allIds += id.asInstanceOf[Id]
      case FetcherDeferredOpt(s, id) if s eq this ⇒ allIds += id.asInstanceOf[Id]
      case FetcherDeferredSeq(s, ids) if s eq this ⇒ allIds ++= ids.asInstanceOf[Seq[Id]]
      case FetcherDeferredSeqOpt(s, ids) if s eq this ⇒ allIds ++= ids.asInstanceOf[Seq[Id]]
      case _ ⇒ // skip
    }

    allIds.toVector
  }
}

object Fetcher {
  def apply[Ctx, Id, Res](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]])(implicit id: HasId[Res, Id]): Fetcher[Ctx, Id, Res] =
    new Fetcher[Ctx, Id, Res](i ⇒ id.id(i), fetch, None, None)

  def apply[Ctx, Id, Res](maxBatchSize: Int, fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]])(implicit id: HasId[Res, Id]): Fetcher[Ctx, Id, Res] =
    new Fetcher[Ctx, Id, Res](i ⇒ id.id(i), fetch, Some(maxBatchSize), None)

  def sync[Ctx, Id, Res](fetch: (Ctx, Seq[Id]) ⇒ Seq[Res])(implicit id: HasId[Res, Id]): Fetcher[Ctx, Id, Res] =
    new Fetcher[Ctx, Id, Res](i ⇒ id.id(i), (ctx: Ctx, ids: Seq[Id]) ⇒ Future.fromTry(Try(fetch(ctx, ids))), None, None)

  def sync[Ctx, Id, Res](maxBatchSize: Int, fetch: (Ctx, Seq[Id]) ⇒ Seq[Res])(implicit id: HasId[Res, Id]): Fetcher[Ctx, Id, Res] =
    new Fetcher[Ctx, Id, Res](i ⇒ id.id(i), (ctx: Ctx, ids: Seq[Id]) ⇒ Future.fromTry(Try(fetch(ctx, ids))), Some(maxBatchSize), None)

  def caching[Ctx, Id, Res](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]])(implicit id: HasId[Res, Id]): Fetcher[Ctx, Id, Res] =
    new Fetcher[Ctx, Id, Res](i ⇒ id.id(i), fetch, None, Some(() ⇒ FetcherCache.simple))

  def caching[Ctx, Id, Res](maxBatchSize: Int, fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]])(implicit id: HasId[Res, Id]): Fetcher[Ctx, Id, Res] =
    new Fetcher[Ctx, Id, Res](i ⇒ id.id(i), fetch, Some(maxBatchSize), Some(() ⇒ FetcherCache.simple))

  def caching[Ctx, Id, Res](cache: FetcherCache, fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]])(implicit id: HasId[Res, Id]): Fetcher[Ctx, Id, Res] =
    new Fetcher[Ctx, Id, Res](i ⇒ id.id(i), fetch, None, Some(() ⇒ cache))

  def caching[Ctx, Id, Res](cache: FetcherCache, maxBatchSize: Int, fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]])(implicit id: HasId[Res, Id]): Fetcher[Ctx, Id, Res] =
    new Fetcher[Ctx, Id, Res](i ⇒ id.id(i), fetch, Some(maxBatchSize), Some(() ⇒ cache))
}

case class FetcherDeferredOne[Ctx, Id, T](source: Fetcher[Ctx, Id, T], id: Id) extends DeferredOne[Id, T]
case class FetcherDeferredOpt[Ctx, Id, T](source: Fetcher[Ctx, Id, T], id: Id) extends DeferredOne[Id, T]
case class FetcherDeferredSeq[Ctx, Id, T](source: Fetcher[Ctx, Id, T], ids: Seq[Id]) extends DeferredSeq[Id, T]
case class FetcherDeferredSeqOpt[Ctx, Id, T](source: Fetcher[Ctx, Id, T], ids: Seq[Id]) extends DeferredSeq[Id, T]

