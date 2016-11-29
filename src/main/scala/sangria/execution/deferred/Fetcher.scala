package sangria.execution.deferred

import scala.collection.mutable.{Set ⇒ MutableSet, Map ⇒ MutableMap}
import scala.concurrent.Future

class Fetcher[Ctx, Res, Id](
  val idFn: Res ⇒ Id,
  val fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]],
  val fetchRel: (Ctx, RelationIds[Res]) ⇒ Future[Seq[Res]],
  val config: FetcherConfig
) {
  def defer(id: Id) = FetcherDeferredOne(this, id)
  def deferOpt(id: Id) = FetcherDeferredOpt(this, id)
  def deferSeq(ids: Seq[Id]) = FetcherDeferredSeq(this, ids)
  def deferSeqOpt(ids: Seq[Id]) = FetcherDeferredSeqOpt(this, ids)

  def deferRel[RelId](rel: Relation[Res, RelId], relId: RelId) = FetcherDeferredRel(this, rel, relId)
  def deferRelOpt[RelId](rel: Relation[Res, RelId], relId: RelId) = FetcherDeferredRelOpt(this, rel, relId)
  def deferRelSeq[RelId](rel: Relation[Res, RelId], relId: RelId) = FetcherDeferredRelSeq(this, rel, relId)
  def deferRelSeqMany[RelId](rel: Relation[Res, RelId], relIds: Seq[RelId]) = FetcherDeferredRelSeqMany(this, rel, relIds)

  def clearCache(deferredResolverState: Any) =
    findCache(deferredResolverState)(_.clear())

  def clearCachedId(deferredResolverState: Any, id: Id) =
    findCache(deferredResolverState)(_.clearId(id))

  def clearCachedRel(deferredResolverState: Any, rel: Relation[Res, _]) =
    findCache(deferredResolverState)(_.clearRel(rel))

  def clearCachedRelId[RelId](deferredResolverState: Any, rel: Relation[Res, RelId], relId: RelId) =
    findCache(deferredResolverState)(_.clearRelId(rel, relId))

  private def findCache(deferredResolverState: Any)(op: FetcherCache ⇒ Unit): Unit =
    deferredResolverState match {
      case map: Map[AnyRef, FetcherCache] @unchecked ⇒ map.get(this) match {
        case Some(cache) ⇒ op(cache)
        case None ⇒ // just ignore
      }
      case _ ⇒ // just ignore
    }

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

  def relIds(deferred: Vector[Deferred[Any]]): Map[Relation[Any, Any], Vector[Any]] = {
    val allIds =  MutableMap[Relation[Any, Any], MutableSet[Any]]()

    def addToSet(rel: Relation[Any, Any], id: Any) =
      allIds.get(rel) match {
        case Some(set) ⇒ set += id
        case None ⇒
          val set = MutableSet[Any]()
          set += id
          allIds(rel) = set
      }

    deferred foreach {
      case FetcherDeferredRel(s, rel, relId) if s eq this ⇒ addToSet(rel, relId)
      case FetcherDeferredRelOpt(s, rel, relId) if s eq this ⇒ addToSet(rel, relId)
      case FetcherDeferredRelSeq(s, rel, relId) if s eq this ⇒ addToSet(rel, relId)
      case FetcherDeferredRelSeqMany(s, rel, relIds) if s eq this ⇒ relIds.foreach(addToSet(rel, _))
      case _ ⇒ // skip
    }

    allIds.map{case (k, v) ⇒ k → v.toVector}.toMap
  }

  def isRel(deferred: Deferred[Any]) = deferred match {
    case FetcherDeferredRel(_, _, _) |
         FetcherDeferredRelOpt(_, _, _) |
         FetcherDeferredRelSeq(_, _, _) |
         FetcherDeferredRelSeqMany(_, _, _) ⇒ true
    case _ ⇒ false
  }
}

object Fetcher {
  private def relationUnsupported[Ctx, Id, Res]: (Ctx, RelationIds[Res]) ⇒ Future[Seq[Res]] =
    (_, _) ⇒ throw new RelationNotSupportedError

  def apply[Ctx, Res, Id](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], config: FetcherConfig = FetcherConfig.empty)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, Id] =
    new Fetcher[Ctx, Res, Id](i ⇒ id.id(i), fetch, relationUnsupported, config)

  def rel[Ctx, Res, Id](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], fetchRel: (Ctx, RelationIds[Res]) ⇒ Future[Seq[Res]], config: FetcherConfig = FetcherConfig.empty)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, Id] =
    new Fetcher[Ctx, Res, Id](i ⇒ id.id(i), fetch, fetchRel, config)

  def caching[Ctx, Res, Id](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], config: FetcherConfig = FetcherConfig.caching)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, Id] =
    new Fetcher[Ctx, Res, Id](i ⇒ id.id(i), fetch, relationUnsupported, config)

  def relCaching[Ctx, Res, Id](fetch: (Ctx, Seq[Id]) ⇒ Future[Seq[Res]], fetchRel: (Ctx, RelationIds[Res]) ⇒ Future[Seq[Res]], config: FetcherConfig = FetcherConfig.caching)(implicit id: HasId[Res, Id]): Fetcher[Ctx, Res, Id] =
    new Fetcher[Ctx, Res, Id](i ⇒ id.id(i), fetch, fetchRel, config)
}

case class FetcherConfig(cacheConfig: Option[() ⇒ FetcherCache] = None, maxBatchSizeConfig: Option[Int] = None) {
  def caching = copy(cacheConfig = Some(() ⇒ FetcherCache.simple))
  def caching(cache: FetcherCache) = copy(cacheConfig = Some(() ⇒ cache))

  def maxBatchSize(size: Int) = copy(maxBatchSizeConfig = Some(size))
}

object FetcherConfig {
  val empty = FetcherConfig()

  def caching = empty.caching
  def caching(cache: FetcherCache) = empty.caching(cache)

  def maxBatchSize(size: Int) = empty.maxBatchSize(size)
}

trait DeferredOne[+T, Id] extends Deferred[T] {
  def id: Id
}

trait DeferredOpt[+T, Id] extends Deferred[Option[T]] {
  def id: Id
}

trait DeferredSeq[+T, Id] extends Deferred[Seq[T]] {
  def ids: Seq[Id]
}

trait DeferredRel[T, RelId] extends Deferred[T] {
  def rel: Relation[T, RelId]
  def relId: RelId
}

trait DeferredRelOpt[T, RelId] extends Deferred[Option[T]] {
  def rel: Relation[T, RelId]
  def relId: RelId
}

trait DeferredRelSeq[T, RelId] extends Deferred[Seq[T]] {
  def rel: Relation[T, RelId]
  def relId: RelId
}

trait DeferredRelSeqMany[T, RelId] extends Deferred[Seq[T]] {
  def rel: Relation[T, RelId]
  def relIds: Seq[RelId]
}

case class FetcherDeferredOne[Ctx, T, Id](source: Fetcher[Ctx, T, Id], id: Id) extends DeferredOne[T, Id]
case class FetcherDeferredOpt[Ctx, T, Id](source: Fetcher[Ctx, T, Id], id: Id) extends DeferredOpt[T, Id]
case class FetcherDeferredSeq[Ctx, T, Id](source: Fetcher[Ctx, T, Id], ids: Seq[Id]) extends DeferredSeq[T, Id]
case class FetcherDeferredSeqOpt[Ctx, T, Id](source: Fetcher[Ctx, T, Id], ids: Seq[Id]) extends DeferredSeq[T, Id]

case class FetcherDeferredRel[Ctx, RelId, T, Id](source: Fetcher[Ctx, T, Id], rel: Relation[T, RelId], relId: RelId) extends DeferredRel[T, RelId]
case class FetcherDeferredRelOpt[Ctx, RelId, T, Id](source: Fetcher[Ctx, T, Id], rel: Relation[T, RelId], relId: RelId) extends DeferredRelOpt[T, RelId]
case class FetcherDeferredRelSeq[Ctx, RelId, T, Id](source: Fetcher[Ctx, T, Id], rel: Relation[T, RelId], relId: RelId) extends DeferredRelSeq[T, RelId]
case class FetcherDeferredRelSeqMany[Ctx, RelId, T, Id](source: Fetcher[Ctx, T, Id], rel: Relation[T, RelId], relIds: Seq[RelId]) extends DeferredRelSeqMany[T, RelId]

trait Relation[T, RelId] {
  def relIds(value: T): Seq[RelId]
}

object Relation {
  def apply[T, RelId](name: String, idFn: T ⇒ Seq[RelId]): Relation[T, RelId] =
    SimpleRelation[T, RelId](name)(idFn)
}

abstract class AbstractRelation[T, RelId](idFn: T ⇒ Seq[RelId]) extends Relation[T, RelId] {
  def relIds(value: T) = idFn(value)
}

case class SimpleRelation[T, RelId](name: String)(idFn: T ⇒ Seq[RelId]) extends AbstractRelation[T, RelId](idFn)

case class RelationIds[Res](rawIds: Map[Relation[Res, _], Seq[_]]) {
  def apply[RelId](relation: Relation[Res, RelId]): Seq[RelId] =
    get[RelId](relation).getOrElse(Vector.empty)

  def get[RelId](relation: Relation[Res, RelId]): Option[Seq[RelId]] =
    rawIds.get(relation).asInstanceOf[Option[Seq[RelId]]]
}

class RelationNotSupportedError extends Exception(s"Relations are not supported by Fetcher.")