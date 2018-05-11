package sangria.util

import java.util.concurrent.ConcurrentHashMap

class ConcurrentHashMapCache[Key, Value] extends Cache[Key, Value] {
  private val cache = new ConcurrentHashMap[Key, Value]()

  def size = cache.size

  def contains(key: Key) = cache.containsKey(key)
  def apply(key: Key) =
    cache.get(key) match {
      case null ⇒ throw new NoSuchElementException
      case v ⇒ v
    }

  def get(key: Key) = Option(cache.get(key))
  def getOrElse(key: Key, default: ⇒ Value) = cache.get(key) match {
    case null ⇒ default
    case v ⇒ v
  }

  def update(key: Key, value: Value) = cache.put(key, value)
  def remove(key: Key) = cache.remove(key)
  def clear() = cache.clear()

  def getOrElseUpdate(key: Key, fn: ⇒ Value) = cache.get(key) match {
    case null ⇒
      val res = fn
      cache.put(key, res)
      res

    case v ⇒ v
  }

  def find(fn: (Key, Value) ⇒ Boolean) = {
    val it = cache.entrySet().iterator()
    var res: Option[(Key, Value)] = None

    while (it.hasNext && res.isEmpty) {
      val elem = it.next()

      if (fn(elem.getKey, elem.getValue))
        res = Some(elem.getKey → elem.getValue)
    }

    res
  }

  def mapToSet[R](fn: (Key, Value) ⇒ R) = {
    val it = cache.entrySet().iterator()
    val res = scala.collection.mutable.Set[R]()

    while (it.hasNext) {
      val elem = it.next()

      res += fn(elem.getKey, elem.getValue)
    }

    res
  }

  def mapValues[R](fn: Value ⇒ R) = {
    val it = cache.entrySet().iterator()
    val res = scala.collection.mutable.Map[Key, R]()

    while (it.hasNext) {
      val elem = it.next()

      res(elem.getKey) = fn(elem.getValue)
    }

    res
  }

  def keyExists(fn: Key ⇒ Boolean): Boolean = {
    val it = cache.entrySet().iterator()

    while (it.hasNext) {
      val elem = it.next()

      if (fn(elem.getKey)) return true
    }

    false
  }

  def forEachValue(fn: Value ⇒ Unit) = {
    val it = cache.values().iterator()

    while (it.hasNext) {
      val elem = it.next()

      fn(elem)
    }
  }

  def removeKeys(fn: Key ⇒ Boolean) = {
    val it = cache.keySet().iterator()

    while (it.hasNext) {
      val elem = it.next()

      if (fn(elem)) it.remove()
    }
  }

  def canEqual(other: Any): Boolean = other.isInstanceOf[ConcurrentHashMapCache[_, _]]

  override def equals(other: Any): Boolean = other match {
    case that: ConcurrentHashMapCache[_, _] ⇒ (that canEqual this) && cache == that.cache
    case _ ⇒ false
  }

  override def hashCode(): Int =
    31 * cache.hashCode()
}
