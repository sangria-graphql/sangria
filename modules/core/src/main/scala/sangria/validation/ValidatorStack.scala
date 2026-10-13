package sangria.validation

import scala.collection.mutable.ArrayBuffer

class ValidatorStack[T] {
  // the top of the stack is the last element
  private val stack: ArrayBuffer[T] = ArrayBuffer.empty

  def push(element: T): Unit = stack += element
  def pop(): T = stack.remove(stack.length - 1)
  def head = stack.last
  def headOption = stack.lastOption
  def head(toDrop: Int) = stack(stack.length - 1 - toDrop)
  def headOption(toDrop: Int) = {
    val idx = stack.length - 1 - toDrop
    if (idx >= 0) Some(stack(idx)) else None
  }
  def nonEmpty = stack.nonEmpty
  def toSeq: Seq[T] = stack.reverse.toSeq
}

object ValidatorStack {
  def empty[T] = new ValidatorStack[T]
}
