package sangria.validation

import scala.collection.mutable.ListBuffer

class ValidatorStack[T] {
  private val stack: ListBuffer[T] = ListBuffer.empty

  def push(element: T): Unit = stack.prepend(element)
  def pop(): T = stack.remove(0)
  def headOption = stack.headOption
  def headOption(toDrop: Int) = stack.drop(toDrop).headOption
  def nonEmpty = stack.nonEmpty
  def toSeq: Seq[T] = stack
}

object ValidatorStack {
  def empty[T] = new ValidatorStack[T]
}
