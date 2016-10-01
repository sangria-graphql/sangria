package sangria.execution.deferred

import scala.collection.concurrent.TrieMap
import scala.concurrent.Future

trait FetcherCache {
  def cacheKey(id: Any): Any

  def contains(id: Any): Boolean

  def cacheable(id: Any): Boolean

  def get(id: Any): Option[Any]

  def update(id: Any, value: Any): Unit
}

object FetcherCache {
  def simple = new SimpleFetcherCache
}

class SimpleFetcherCache extends FetcherCache {
  private val cache = TrieMap[Any, Any]()

  def cacheKey(id: Any) = id

  def contains(id: Any) = cache.contains(cacheKey(id))

  def cacheable(id: Any) = true

  def get(id: Any) = cache.get(cacheKey(id))

  def update(id: Any, value: Any) = {
    if (cacheable(id))
      cache.update(cacheKey(id), value)
  }
}