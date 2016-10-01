package sangria.execution.deferred

import scala.annotation.unchecked.uncheckedVariance
import scala.collection.immutable.VectorBuilder
import scala.concurrent.{ExecutionContext, Future}
import scala.util.{Failure, Success, Try}
import scala.collection.mutable.{Map => MutableMap}

class FetcherBasedDeferredResolver[-Ctx](fetchers: Vector[Fetcher[Ctx, _, _]]) extends DeferredResolver[Ctx] {
  private val fetchersMap: Map[AnyRef, Fetcher[Ctx, _, _]] @uncheckedVariance =
    fetchers.map(f ⇒ f → f).toMap

  override def initialQueryState = fetchers.flatMap(f ⇒ f.cache.map(cacheFn ⇒ (f: AnyRef) → cacheFn())).toMap

  def resolve(deferred: Vector[Deferred[Any]], ctx: Ctx, queryState: Any)(implicit ec: ExecutionContext) =  {
    val fetcherCaches = queryState.asInstanceOf[Map[AnyRef, FetcherCache]]

    val grouped = deferred groupBy {
      case FetcherDeferredOne(s, _) ⇒ fetchersMap.get(s)
      case FetcherDeferredOpt(s, id) ⇒ fetchersMap.get(s)
      case FetcherDeferredSeq(s, ids) ⇒ fetchersMap.get(s)
      case FetcherDeferredSeqOpt(s, ids) ⇒ fetchersMap.get(s)
      case _ ⇒ None
    }

    val failed: Set[Deferred[Any]] = grouped.get(None).map(_.toSet).getOrElse(Set.empty)
    val resolved = MutableMap[Deferred[Any], Future[Any]]()

    if (failed.nonEmpty)
      println(fetchers)

    grouped foreach {
      case (Some(fetcher), d) ⇒
        val f = fetcher.asInstanceOf[Fetcher[Ctx, Any, Any]]
        val fetcherCache = fetcherCaches.get(fetcher)
        val ids = fetcher.ids(d)
        val (nonCachedIds, cachedResults) = partitionCached(fetcherCache, ids)

        val groupedIds = fetcher.maxBatchSize match {
          case Some(size) ⇒ nonCachedIds.grouped(size)
          case None ⇒ Iterator(nonCachedIds)
        }

        val results = groupedIds map { group ⇒
          if (group.nonEmpty)
            f.fetch(ctx, group).map(r ⇒ group → Success(r): (Vector[Any], Try[Seq[Any]])).recover {case e ⇒ group → Failure(e)}
          else
            Future.successful(group → Success(Seq.empty))
        }

        val futureRes = Future.sequence(results).map { allResults ⇒
          val byId = MutableMap[Any, Any]() // can contain either exception or actual value! (using `Any` to avoid unnecessary boxing)

          allResults.toVector.foreach { case (group, groupResult) ⇒
            groupResult match {
              case Success(values) ⇒
                values.foreach(v ⇒ byId(f.idFn(v)) = v)

              case Failure(e) ⇒
                group.foreach(id ⇒ byId(id) = e)
            }
          }

          byId
        }

        d foreach { deferred ⇒
          resolved(deferred) = futureRes.map { m ⇒
            val f = fetcher.asInstanceOf[Fetcher[Any, Any, Any]]

            def updateCache[T](id: Any, v: T): T = fetcherCache match {
              case Some(cache) ⇒
                cache.update(id, v)

                v
              case None ⇒ v
            }

            deferred match {
              case FetcherDeferredOne(_, id) if cachedResults contains id ⇒
                cachedResults(id)

              case FetcherDeferredOne(_, id) ⇒
                m.get(id) match {
                  case Some(e: Exception) ⇒ throw e
                  case Some(v) ⇒ updateCache(id, v)
                  case None ⇒ throw new AbsentDeferredValueError(f, deferred, id)
                }

              case FetcherDeferredOpt(_, id) if cachedResults contains id ⇒
                cachedResults.get(id)

              case FetcherDeferredOpt(_, id) ⇒
                m.get(id) match {
                  case Some(e: Exception) ⇒ throw e
                  case v ⇒
                    v foreach (updateCache(id, _))

                    v
                }

              case FetcherDeferredSeq(_, ids) ⇒
                ids map { id ⇒
                  if (cachedResults contains id)
                    cachedResults(id)
                  else
                    m.get(id) match {
                      case Some(e: Exception) ⇒ throw e
                      case Some(v) ⇒ updateCache(id, v)
                      case None ⇒ throw new AbsentDeferredValueError(f, deferred, id)
                    }
                }

              case FetcherDeferredSeqOpt(_, ids) ⇒
                ids flatMap { id ⇒
                  if (cachedResults contains id)
                    cachedResults.get(id)
                  else
                    m.get(id) match {
                      case Some(e: Exception) ⇒ throw e
                      case v ⇒
                        v foreach (updateCache(id, _))

                        v
                    }
                }
            }
          }
        }

      case (None, _) ⇒ // handled by failed
    }

    deferred map { d ⇒
      if (failed contains d) Future.failed(UnsupportedDeferError(d))
      else resolved(d)
    }
  }

  private def partitionCached(cache: Option[FetcherCache], ids: Vector[Any]): (Vector[Any], MutableMap[Any, Any]) =
    cache match {
      case Some(c) ⇒
        val misses = new VectorBuilder[Any]
        val hits = MutableMap[Any, Any]()

        ids.foreach { id ⇒
          c.get(id) match {
            case Some(v) ⇒ hits(id) = v
            case None ⇒ misses += id
          }
        }

        misses.result() → hits

      case None ⇒
        ids → MutableMap.empty
    }
}

case class AbsentDeferredValueError(fetcher: Fetcher[Any, Any, Any], deferred: Deferred[Any], id: Any)
  extends Exception(s"Fetcher has not resolved non-optional ID: $id.")
