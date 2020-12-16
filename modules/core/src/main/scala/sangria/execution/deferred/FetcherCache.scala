package sangria.execution.deferred

import sangria.util.Cache

trait FetcherCache {
  def cacheKey(id: Any): Any
  def cacheKeyRel(rel: Any, relId: Any): Any

  def cacheable(id: Any): Boolean
  def cacheableRel(rel: Any, relId: Any): Boolean

  def get(id: Any): Option[Any]
  def getRel(rel: Any, relId: Any): Option[Seq[Any]]

  def update(id: Any, value: Any): Unit
  def updateRel[T](rel: Any, relId: Any, idFn: T => Any, values: Seq[T]): Unit

  def clear(): Unit
  def clearId(id: Any): Unit
  def clearRel(rel: Any): Unit
  def clearRelId(rel: Any, relId: Any): Unit
}

object FetcherCache {
  def simple = new SimpleFetcherCache
}

class SimpleFetcherCache extends FetcherCache {
  private val cache = Cache.empty[Any, Any]
  private val relCache = Cache.empty[Any, Seq[Any]]

  def cacheKey(id: Any) = id
  def cacheKeyRel(rel: Any, relId: Any) = rel -> relId

  def cacheable(id: Any) = true
  def cacheableRel(rel: Any, relId: Any) = true

  def get(id: Any) = cache.get(cacheKey(id))
  def getRel(rel: Any, relId: Any) = relCache.get(cacheKeyRel(rel, relId))

  def update(id: Any, value: Any) =
    if (cacheable(id))
      cache.update(cacheKey(id), value)

  def updateRel[T](rel: Any, relId: Any, idFn: T => Any, values: Seq[T]) =
    if (cacheableRel(rel, relId)) {
      values.foreach { v =>
        update(idFn(v), v)
      }

      relCache.update(cacheKeyRel(rel, relId), values)
    }

  def clear() = {
    cache.clear()
    relCache.clear()
  }

  override def clearId(id: Any) =
    cache.remove(cacheKey(id))

  override def clearRel(rel: Any) =
    relCache.removeKeys {
      case key @ (r, _) if r == rel => true
      case _ => false
    }

  override def clearRelId(rel: Any, relId: Any) =
    relCache.remove(cacheKeyRel(rel, relId))
}
