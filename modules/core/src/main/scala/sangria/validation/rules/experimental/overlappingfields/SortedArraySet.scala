package sangria.validation.rules.experimental.overlappingfields

import java.util
import java.util.Comparator
import java.util.function.Consumer

/** A set representation that is well suited to hash and equality comparisons and fast iteration over members
  */
class SortedArraySet[T](private val sortedMembers: util.ArrayList[T])
    extends java.lang.Iterable[T] {

  //cache the hashCode for faster handling
  override val hashCode: Int =
    sortedMembers.hashCode()

  //equals and hash code delegate to the members
  override def equals(obj: Any): Boolean =
    obj match {
      case other: SortedArraySet[_] =>
        eq(other) || (hashCode == other.hashCode && size() == other
          .size() && sortedMembers == other.sortedMembers)
      case _ => false
    }

  def isEmpty: Boolean =
    sortedMembers.isEmpty

  def size(): Int =
    sortedMembers.size()

  override def iterator(): util.Iterator[T] =
    sortedMembers.iterator()

  override def forEach(action: Consumer[_ >: T]): Unit =
    sortedMembers.forEach(action)
}

object SortedArraySet {

  def newBuilder[T: Ordering](sizeHint: Int): Builder[T] =
    new Builder[T](sizeHint, implicitly[Ordering[T]])

  def newBuilder[T: Ordering](): Builder[T] =
    new Builder[T](implicitly[Ordering[T]])

  //Beware:
  //The comparator wont be used in the final set for equality or removing duplicates, it's only here for sorting.
  //As such it has to be compatible with the standard equality and hashCode implementations.
  class Builder[T] private (
      private val members: util.ArrayList[T],
      private val comparator: Comparator[T]) {

    def this(sizeHint: Int, ordering: Ordering[T]) {
      this(new util.ArrayList[T](sizeHint), ordering)
    }

    def this(ordering: Ordering[T]) {
      this(new util.ArrayList[T](), ordering)
    }

    def add(value: T): this.type = {
      members.add(value)
      this
    }

    def addAll(values: util.Collection[T]): this.type = {
      members.addAll(values)
      this
    }

    def build(): SortedArraySet[T] = {
      sortAndRemoveDuplicates()
      new SortedArraySet(members)
    }

    private def sortAndRemoveDuplicates(): Unit = {
      members.sort(comparator)
      var into = 0
      var from = 0
      while (from < members.size()) {
        val first_from = members.get(from)
        members.set(into, first_from)
        into += 1
        do from += 1 while (from < members.size() && members.get(from) == first_from)
      }
      members.subList(into, members.size()).clear()
    }
  }

}
