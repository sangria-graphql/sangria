package sangria.execution.deferred

import scala.annotation.implicitNotFound

@implicitNotFound(msg = "Can't find suitable `HasId` type-class instance for type `${T}`. If you have defined it already, please consider defining an implicit instance `HasId[${T}]`.")
trait HasId[T, Id] {
  def id(value: T): Id
}

object HasId {
  private class SimpleHasId[T, Id](fn: T ⇒ Id) extends HasId[T, Id] {
    def id(value: T) = fn(value)
  }

  def apply[T, Id](fn: T ⇒ Id): HasId[T, Id] = new SimpleHasId[T, Id](fn)
}
