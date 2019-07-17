package sangria.util

import scala.collection.concurrent.TrieMap

class TrieMapCache[Key, Value] extends Cache[Key, Value] {
  private val cache = TrieMap[Key, Value]()

  def size = cache.size

  def contains(key: Key) = cache.contains(key)
  def apply(key: Key) = cache(key)
  def get(key: Key) = cache.get(key)
  def getOrElse(key: Key, default: => Value) = cache.getOrElse(key, default)
  def update(key: Key, value: Value) = cache.update(key, value)
  def remove(key: Key) = cache.remove(key)
  def clear() = cache.clear()

  def getOrElseUpdate(key: Key, fn: => Value) = cache.getOrElseUpdate(key, fn)
  def find(fn: (Key, Value) => Boolean) = cache.find {case (key, value) => fn(key, value)}
  def mapToSet[R](fn: (Key, Value) => R) = cache.map {case (key, value) => fn(key, value)}.toSet
  def mapValues[R](fn: Value => R) = cache.mapValues(fn).toMap
  def keyExists(fn: Key => Boolean) = cache.keySet.exists(fn)
  def forEachValue(fn: Value => Unit) = cache.values.foreach(fn)
  def removeKeys(fn: Key => Boolean) = cache.keys.toVector.foreach(key => if (fn(key)) cache.remove(key))

  def canEqual(other: Any): Boolean = other.isInstanceOf[TrieMapCache[_, _]]

  override def equals(other: Any): Boolean = other match {
    case that: TrieMapCache[_, _] => (that canEqual this) && cache == that.cache
    case _ => false
  }

  override def hashCode(): Int =
    31 * cache.hashCode()
}
