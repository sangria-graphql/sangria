package sangria.validation.rules.overlappingfields

import java.util.function.Consumer

import org.scalatest.FunSuite

class SortedArraySetSpec extends FunSuite {

  test("zero elements can be build") {
    val set = SortedArraySet.newBuilder[Int]().build()
    assert(set.isEmpty)
    assert(set.size() === 0)
  }

  test("zero elements equality") {
    val set1 = SortedArraySet.newBuilder[Int]().build()
    val set2 = SortedArraySet.newBuilder[Int]().build()
    assert(set1 === set2)
    assert(set1.hashCode === set2.hashCode)
  }

  test("one element can be build") {
    val set = SortedArraySet.newBuilder[Int]().add(42).build()
    assert(!set.isEmpty)
    assert(set.size() === 1)
  }

  test("one element equality") {
    val set1 = SortedArraySet.newBuilder[Int]().add(42).build()
    val set2 = SortedArraySet.newBuilder[Int]().add(42).build()
    val set3 = SortedArraySet.newBuilder[Int]().add(41).build()
    assert(set1 === set2)
    assert(set1 !== set3)
    assert(set1.hashCode === set2.hashCode)
    assert(set1.hashCode !== set3.hashCode)
  }

  test("many elements removes duplicates and equals") {
    val set1 = SortedArraySet.newBuilder[Int]().add(42).add(7).add(3).add(7).add(42).build()
    val set2 = SortedArraySet.newBuilder[Int]().add(3).add(42).add(3).add(42).add(7).add(3).add(3).build()
    val set3 = SortedArraySet.newBuilder[Int]().add(3).add(42).add(2).add(42).add(7).build()
    assert(set1 === set2)
    assert(set1 !== set3)
    assert(set1.hashCode === set2.hashCode)
    assert(set1.hashCode !== set3.hashCode)
  }

  test("many elements are sorted") {
    val set = SortedArraySet.newBuilder[Int]().add(3).add(42).add(3).add(42).add(7).add(3).add(3).build()
    val list = List.newBuilder[Int]
    set.forEach {
      new Consumer[Int] {
        override def accept(member: Int): Unit = {
          list += member
        }
      }
    }
    assert(list.result() == List(3, 7, 42))
  }
}
