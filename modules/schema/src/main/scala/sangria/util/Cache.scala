package sangria.util

import scala.collection.{Map, Set}

trait Cache[Key, Value] {
  def size: Int

  def contains(key: Key): Boolean
  def apply(key: Key): Value
  def get(key: Key): Option[Value]
  def getOrElse(key: Key, default: => Value): Value
  def update(key: Key, value: Value): Unit
  def remove(key: Key): Unit
  def clear(): Unit

  // NOTE: that `getOrElseUpdate` allows a race condition between value retrieval and cache update.
  // It is an explicit decision to avoid any kind of synchronization (it is preferred to recompute value multiple times than to synchronize)
  def getOrElseUpdate(key: Key, fn: => Value): Value
  def find(fn: (Key, Value) => Boolean): Option[(Key, Value)]
  def mapToSet[R](fn: (Key, Value) => R): Set[R]
  def mapValues[R](fn: Value => R): Map[Key, R]
  def keyExists(fn: Key => Boolean): Boolean
  def forEachValue(fn: Value => Unit): Unit
  def removeKeys(fn: Key => Boolean): Unit
}

object Cache {
  def empty[Key, Value]: Cache[Key, Value] = emptyConcurrentHashMap[Key, Value]

  def emptyTrieMap[Key, Value] = new TrieMapCache[Key, Value]
  def emptyConcurrentHashMap[Key, Value] = new ConcurrentHashMapCache[Key, Value]

  def apply[Key, Value](elems: (Key, Value)*) = {
    val c = empty[Key, Value]
    elems.foreach { case (key, value) => c(key) = value }
    c
  }
}
