package sangria.validation.rules.overlappingfields

import scala.collection.mutable
import scala.reflect.ClassTag

/**
  * A set representation that is well suited to hash and equality comparisons and fast iteration over members
  */
class SortedArraySet[T](private val sortedMembers: mutable.WrappedArray[T]) extends Iterable[T] {

  //cache the hashCode for faster handling
  override val hashCode: Int = sortedMembers.hashCode()

  //equals and hash code delegate to the members
  override def equals(obj: Any): Boolean = {
    obj match {
      case other: SortedArraySet[_] => eq(other) || sortedMembers == other.sortedMembers
      case _                        => false
    }
  }

  override def isEmpty: Boolean = {
    sortedMembers.isEmpty
  }

  override def size: Int = {
    sortedMembers.length
  }

  override def foreach[U](f: T => U): Unit = {
    sortedMembers.foreach(f)
  }

  override def iterator: Iterator[T] = {
    sortedMembers.iterator
  }
}

object SortedArraySet {

  def newBuilder[T: Ordering](sizeHint: Int)(implicit t: ClassTag[T]): Builder[T] = {
    val builder = Array.newBuilder[T]
    builder.sizeHint(sizeHint)
    new Builder[T](builder, implicitly[Ordering[T]])
  }

  def newBuilder[T: Ordering]()(implicit t: ClassTag[T]): Builder[T] = {
    new Builder[T](Array.newBuilder[T], implicitly[Ordering[T]])
  }

  //Beware:
  //The ordering wont be used in the final set for equality or removing duplicates, it's only here for sorting.
  //As such it has to be compatible with the standard equality and hashCode implementations.
  class Builder[T](private val memberBuilder: mutable.ArrayBuilder[T], private val ordering: Ordering[T]) {


    def add(value: T): this.type = {
      memberBuilder += value
      this
    }

    def addAll(values: Traversable[T]): this.type = {
      memberBuilder ++= values
      this
    }

    def build(): SortedArraySet[T] = {
      new SortedArraySet(sortAndRemoveDuplicates())
    }

    private def sortAndRemoveDuplicates(): mutable.WrappedArray[T] = {
      val members = memberBuilder.result()
      scala.util.Sorting.quickSort(members)(ordering)
      var into = 0
      var from = 0
      while (from < members.length) {
        val first_from = members(from)
        members(into) = first_from
        into += 1
        do {
          from += 1
        } while (from < members.length && members(from) == first_from)
      }
      members.slice(0, into)
    }
  }
}
