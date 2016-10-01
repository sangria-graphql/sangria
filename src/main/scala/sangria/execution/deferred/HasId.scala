package sangria.execution.deferred

trait HasId[T, Id] {
  def id(value: T): Id
}

object HasId {
  private class SimpleHasId[T, Id](fn: T ⇒ Id) extends HasId[T, Id] {
    def id(value: T) = fn(value)
  }

  def apply[T, Id](fn: T ⇒ Id): HasId[T, Id] = new SimpleHasId[T, Id](fn)
}
