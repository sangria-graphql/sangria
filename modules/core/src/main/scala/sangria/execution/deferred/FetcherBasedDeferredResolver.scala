package sangria.execution.deferred

import sangria.execution.DeferredWithInfo

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.VectorBuilder
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.collection.mutable.{Map => MutableMap, Set => MutableSet}

class FetcherBasedDeferredResolver[-Ctx](
    fetchers: Vector[Fetcher[Ctx, _, _, _]],
    fallback: Option[DeferredResolver[Ctx]])
    extends DeferredResolver[Ctx] {
  private val fetchersMap: Map[AnyRef, Fetcher[Ctx, _, _, _]] @uncheckedVariance =
    fetchers.map(f => f -> f).toMap

  override def groupDeferred[T <: DeferredWithInfo](deferred: Vector[T]) =
    fallback match {
      case Some(f) => f.groupDeferred(deferred)
      case None => super.groupDeferred(deferred)
    }

  override val includeDeferredFromField =
    fallback.flatMap(_.includeDeferredFromField).orElse(super.includeDeferredFromField)

  override def initialQueryState =
    fetchers.flatMap(f => f.config.cacheConfig.map(cacheFn => (f: AnyRef) -> cacheFn())).toMap

  def resolve(deferred: Vector[Deferred[Any]], ctx: Ctx, queryState: Any)(implicit
      ec: ExecutionContext) = {
    val fetcherCaches = queryState.asInstanceOf[Map[AnyRef, FetcherCache]]

    val grouped = deferred.groupBy {
      case FetcherDeferredOne(s, _) => fetchersMap.get(s)
      case FetcherDeferredOpt(s, _) => fetchersMap.get(s)
      case FetcherDeferredOptOpt(s, _) => fetchersMap.get(s)
      case FetcherDeferredSeq(s, _) => fetchersMap.get(s)
      case FetcherDeferredSeqOpt(s, _) => fetchersMap.get(s)
      case FetcherDeferredSeqOptExplicit(s, _) => fetchersMap.get(s)
      case FetcherDeferredRel(s, _, _) => fetchersMap.get(s)
      case FetcherDeferredRelOpt(s, _, _) => fetchersMap.get(s)
      case FetcherDeferredRelSeq(s, _, _) => fetchersMap.get(s)
      case FetcherDeferredRelSeqMany(s, _, _) => fetchersMap.get(s)
      case _ => None
    }

    val resolved = MutableMap[Deferred[Any], Future[Any]]()

    grouped.foreach {
      case (Some(fetcher), d) =>
        val fetcherCache = fetcherCaches.get(fetcher)
        val (relDeferred, normalDeferred) = d.partition(fetcher.isRel)
        val fetcherContext =
          FetcherContext[Ctx](ctx, fetcher, fetcherCache, fetcherCaches, fetchers)

        resolveRelations(fetcherContext, relDeferred, resolved)
        resolveEntities(fetcherContext, normalDeferred, resolved)

      case (None, deferred) =>
        fallback match {
          case Some(f) =>
            val res = f.resolve(deferred, ctx, queryState)

            for (i <- deferred.indices)
              resolved(deferred(i)) = res(i)
          case None =>
            deferred.foreach(d => resolved(d) = Future.failed(UnsupportedDeferError(d)))

        }
    }

    deferred.map(d => resolved(d))
  }

  private def resolveRelations(
      ctx: FetcherContext[Ctx] @uncheckedVariance,
      deferredToResolve: Vector[Deferred[Any]],
      resolved: MutableMap[Deferred[Any], Future[Any]]
  )(implicit ec: ExecutionContext) = {
    val f = ctx.fetcher.asInstanceOf[Fetcher[Ctx, Any, Any, Any]]
    val relIds = ctx.fetcher.relIds(deferredToResolve)

    val (nonCachedIds, cachedResults) = partitionCachedRel(ctx.cache, relIds)

    val groupedRelIds = ctx.fetcher.config.maxBatchSizeConfig match {
      case Some(size) => nonCachedIds.map { case (rel, ids) => (rel, ids.grouped(size)) }
      case None => nonCachedIds.map { case (rel, ids) => (rel, Iterator.single(ids)) }
    }

    val results = groupedRelIds.flatMap { case (rel, groupIds) =>
      groupIds.map { group =>
        if (group.nonEmpty)
          f.fetchRel(ctx, RelationIds(Map(rel -> group)))
            .map(groupAndCacheRelations(ctx, Map(rel -> group), _))
        else
          Future.successful(MutableMap.empty[Relation[Any, Any, Any], MutableMap[Any, Seq[Any]]])
      }
    }

    val futureRes = Future.sequence(results).map { allResults =>
      val byRel = MutableMap[Relation[Any, _, _], MutableMap[Any, Seq[Any]]]()

      allResults.foreach(relResult =>
        relResult.foreach { case (rel, v) =>
          if (byRel.contains(rel))
            byRel(rel) ++= v
          else
            byRel(rel) = v
        })

      byRel
    }

    deferredToResolve.foreach { deferred =>
      resolved(deferred) = futureRes.map { m =>
        val f = ctx.fetcher.asInstanceOf[Fetcher[Any, Any, Any, Any]]

        deferred match {
          case FetcherDeferredRel(_, rel: Relation[Any, Any, Any], relId)
              if cachedResults.contains(rel.asInstanceOf[Relation[Any, Any, Any]]) && cachedResults(
                rel.asInstanceOf[Relation[Any, Any, Any]]).contains(relId) =>
            cachedResults(rel.asInstanceOf[Relation[Any, Any, Any]])(relId).headOption match {
              case Some(head) => head
              case None => throw AbsentDeferredRelValueError(f, deferred, rel, relId)
            }

          case FetcherDeferredRel(_, rel, relId)
              if m.contains(rel.asInstanceOf[Relation[Any, Any, Any]]) && m(
                rel.asInstanceOf[Relation[Any, Any, Any]]).contains(relId) =>
            m(rel.asInstanceOf[Relation[Any, Any, Any]])(relId).headOption match {
              case Some(head) => head
              case None =>
                throw AbsentDeferredRelValueError(
                  f,
                  deferred,
                  rel.asInstanceOf[Relation[Any, Any, Any]],
                  relId)
            }

          case FetcherDeferredRel(_, rel, relId) =>
            throw AbsentDeferredRelValueError(
              f,
              deferred,
              rel.asInstanceOf[Relation[Any, Any, Any]],
              relId)

          case FetcherDeferredRelOpt(_, rel: Relation[Any, Any, Any], relId)
              if cachedResults.contains(rel.asInstanceOf[Relation[Any, Any, Any]]) && cachedResults(
                rel.asInstanceOf[Relation[Any, Any, Any]]).contains(relId) =>
            cachedResults(rel.asInstanceOf[Relation[Any, Any, Any]])(relId).headOption

          case FetcherDeferredRelOpt(_, rel, relId)
              if m.contains(rel.asInstanceOf[Relation[Any, Any, Any]]) && m(
                rel.asInstanceOf[Relation[Any, Any, Any]]).contains(relId) =>
            m(rel.asInstanceOf[Relation[Any, Any, Any]])(relId).headOption

          case FetcherDeferredRelOpt(_, _, _) =>
            None

          case FetcherDeferredRelSeq(_, rel: Relation[Any, Any, Any], relId)
              if cachedResults.contains(rel) && cachedResults(rel).contains(relId) =>
            cachedResults(rel)(relId)

          case FetcherDeferredRelSeq(_, rel: Relation[Any, Any, Any], relId)
              if m.contains(rel) && m(rel).contains(relId) =>
            m(rel)(relId)

          case FetcherDeferredRelSeq(_, _, _) =>
            Vector.empty

          case FetcherDeferredRelSeqMany(_, rel: Relation[Any, Any, Any], relIds)
              if cachedResults.contains(rel) =>
            removeDuplicates(
              f,
              relIds.flatMap(relId => cachedResults(rel).getOrElse(relId, Vector.empty)))

          case FetcherDeferredRelSeqMany(_, rel: Relation[Any, Any, Any], relIds)
              if m.contains(rel) =>
            removeDuplicates(f, relIds.flatMap(relId => m(rel).getOrElse(relId, Vector.empty)))

          case FetcherDeferredRelSeqMany(_, _, _) =>
            Vector.empty
        }
      }
    }
  }

  private def removeDuplicates(fetcher: Fetcher[Any, Any, Any, Any], values: Seq[Any]) = {
    val seen = MutableSet[Any]()

    values.filter { v =>
      val id = fetcher.idFn(v)

      if (seen contains id) false
      else {
        seen += id
        true
      }
    }
  }

  // This is here for scala 2.12 / 2.13 compat, as `Iterator.empty` does NOT
  // take type parameters in 2.12, but does in 2.13.
  private val emptyIterator = Iterator[Future[(Vector[Any], Try[Seq[Any]])]]()

  private def resolveConcurrentBatches(
      ctx: FetcherContext[Ctx] @uncheckedVariance,
      f: Fetcher[Ctx, Any, Any, Any] @uncheckedVariance,
      nonCachedIds: Vector[Any]
  )(implicit ec: ExecutionContext): Iterator[Future[(Vector[Any], Try[Seq[Any]])]] = {

    val groupedIds = ctx.fetcher.config.maxBatchSizeConfig match {
      case Some(size) => nonCachedIds.grouped(size)
      case None => Iterator(nonCachedIds)
    }

    val groupedBatches =
      ctx.fetcher.config.maxConcurrentBatchesConfig match {
        case Some(size) => groupedIds.grouped(size)
        case None => Iterator(groupedIds)
      }

    groupedBatches
      .foldLeft(emptyIterator) { (accumFutureSeq, groupedIds) =>
        val results: Iterator[Future[(Vector[Any], Try[Seq[Any]])]] =
          groupedIds.toIterator.collect {
            case group if group.nonEmpty =>
              f.fetch(ctx, group)
                .map(r => group -> Success(r): (Vector[Any], Try[Seq[Any]]))
                .recover { case e => group -> Failure(e) }
            case group =>
              Future.successful(group -> Success(Seq.empty))
          }
        accumFutureSeq ++ results
      }
  }

  private def resolveEntities(
      ctx: FetcherContext[Ctx] @uncheckedVariance,
      deferredToResolve: Vector[Deferred[Any]],
      resolved: MutableMap[Deferred[Any], Future[Any]]
  )(implicit ec: ExecutionContext) = {
    val f = ctx.fetcher.asInstanceOf[Fetcher[Ctx, Any, Any, Any]]
    val ids = ctx.fetcher.ids(deferredToResolve)
    val (nonCachedIds, cachedResults) = partitionCached(ctx.cache, ids)

    val results = resolveConcurrentBatches(ctx, f, nonCachedIds)

    val futureRes = Future.sequence(results).map { allResults =>
      val byId =
        MutableMap[Any, Any]() // can contain either exception or actual value! (using `Any` to avoid unnecessary boxing)

      allResults.toVector.foreach { case (group, groupResult) =>
        groupResult match {
          case Success(values) =>
            values.foreach(v => byId(f.idFn(v)) = v)

          case Failure(e) =>
            group.foreach(id => byId(id) = e)
        }
      }

      byId
    }

    deferredToResolve.foreach { deferred =>
      resolved(deferred) = futureRes.map { m =>
        val f = ctx.fetcher.asInstanceOf[Fetcher[Any, Any, Any, Any]]

        def updateCache[T](id: Any, v: T): T = ctx.cache match {
          case Some(cache) =>
            cache.update(id, v)

            v
          case None => v
        }

        deferred match {
          case FetcherDeferredOne(_, id) if cachedResults contains id =>
            cachedResults(id)

          case FetcherDeferredOne(_, id) =>
            m.get(id) match {
              case Some(t: Throwable) => throw t
              case Some(v) => updateCache(id, v)
              case None => throw AbsentDeferredValueError(f, deferred, id)
            }

          case FetcherDeferredOpt(_, id) if cachedResults contains id =>
            cachedResults.get(id)

          case FetcherDeferredOpt(_, id) =>
            m.get(id) match {
              case Some(t: Throwable) => throw t
              case v =>
                v.foreach(updateCache(id, _))

                v
            }

          case FetcherDeferredOptOpt(_, None) =>
            None

          case FetcherDeferredOptOpt(_, Some(id)) if cachedResults contains id =>
            cachedResults.get(id)

          case FetcherDeferredOptOpt(_, Some(id)) =>
            m.get(id) match {
              case Some(t: Throwable) => throw t
              case v =>
                v.foreach(updateCache(id, _))

                v
            }

          case FetcherDeferredSeq(_, ids) =>
            ids.map { id =>
              if (cachedResults contains id)
                cachedResults(id)
              else
                m.get(id) match {
                  case Some(t: Throwable) => throw t
                  case Some(v) => updateCache(id, v)
                  case None => throw AbsentDeferredValueError(f, deferred, id)
                }
            }

          case FetcherDeferredSeqOpt(_, ids) =>
            ids.flatMap { id =>
              if (cachedResults contains id)
                cachedResults.get(id)
              else
                m.get(id) match {
                  case Some(t: Throwable) => throw t
                  case v =>
                    v.foreach(updateCache(id, _))

                    v
                }
            }

          case FetcherDeferredSeqOptExplicit(_, ids) =>
            ids.map { id =>
              if (cachedResults contains id)
                cachedResults.get(id)
              else
                m.get(id) match {
                  case Some(t: Throwable) => throw t
                  case v =>
                    v.foreach(updateCache(id, _))

                    v
                }
            }
        }
      }
    }
  }

  private def partitionCached(
      cache: Option[FetcherCache],
      ids: Vector[Any]): (Vector[Any], MutableMap[Any, Any]) =
    cache match {
      case Some(c) =>
        val misses = new VectorBuilder[Any]
        val hits = MutableMap[Any, Any]()

        ids.foreach { id =>
          c.get(id) match {
            case Some(v) => hits(id) = v
            case None => misses += id
          }
        }

        misses.result() -> hits

      case None =>
        ids -> MutableMap.empty
    }

  private def partitionCachedRel(
      cache: Option[FetcherCache],
      ids: Map[Relation[Any, Any, Any], Vector[Any]]): (
      Map[Relation[Any, Any, Any], Vector[Any]],
      MutableMap[Relation[Any, Any, Any], MutableMap[Any, Seq[Any]]]) =
    cache match {
      case Some(c) =>
        val misses = MutableMap[Relation[Any, Any, Any], MutableSet[Any]]()
        val hits = MutableMap[Relation[Any, Any, Any], MutableMap[Any, Seq[Any]]]()

        def addHit(rel: Relation[Any, Any, Any], relId: Any, res: Seq[Any]) =
          hits.getOrElseUpdate(rel, MutableMap[Any, Seq[Any]]())(relId) = res

        def addMiss(rel: Relation[Any, Any, Any], relId: Any) =
          misses.getOrElseUpdate(rel, MutableSet[Any]()) += relId

        ids.foreach { case (rel, ids) =>
          ids.foreach { relId =>
            c.getRel(rel, relId) match {
              case Some(v) => addHit(rel, relId, v)
              case None => addMiss(rel, relId)
            }
          }
        }

        misses.map { case (k, v) => k -> v.toVector }.toMap -> hits

      case None =>
        ids -> MutableMap.empty
    }

  private def groupAndCacheRelations(
      ctx: FetcherContext[_],
      relIds: Map[Relation[Any, Any, Any], Vector[Any]],
      result: Seq[Any]): MutableMap[Relation[Any, Any, Any], MutableMap[Any, Seq[Any]]] = {
    val grouped = MutableMap[Relation[Any, Any, Any], MutableMap[Any, Seq[Any]]]()

    def updateCache[T](rel: Relation[Any, Any, Any], relId: Any, v: Seq[T]): Seq[T] =
      ctx.cache match {
        case Some(cache) =>
          cache.updateRel(rel, relId, ctx.fetcher.idFn.asInstanceOf[T => Any], v)

          v
        case None => v
      }

    relIds.foreach { case (rel, relIdsForRel) =>
      val identified = MutableMap[Any, VectorBuilder[Any]]()

      result.foreach { res =>
        val relIds = rel.relIds(res)
        val mappedRes = rel.map(res)

        relIds.foreach { relId =>
          identified.getOrElseUpdate(relId, new VectorBuilder[Any]) += mappedRes
        }
      }

      relIdsForRel.foreach { relId =>
        val res = identified.get(relId).fold(Vector.empty[Any])(_.result())

        updateCache(rel, relId, res)

        grouped.getOrElseUpdate(rel, MutableMap[Any, Seq[Any]]())(relId) = res
      }
    }

    grouped
  }
}

case class AbsentDeferredValueError(
    fetcher: Fetcher[Any, Any, Any, Any],
    deferred: Deferred[Any],
    id: Any)
    extends Exception(s"Fetcher has not resolved non-optional ID '$id'.")

case class AbsentDeferredRelValueError(
    fetcher: Fetcher[Any, Any, Any, Any],
    deferred: Deferred[Any],
    rel: Relation[Any, Any, Any],
    relId: Any)
    extends Exception(
      s"Fetcher has not resolved non-optional relation ID '$relId' for relation '$rel'.")
