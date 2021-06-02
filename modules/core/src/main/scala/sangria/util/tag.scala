package sangria.util

object tag {
  def apply[U] = new Tagger[U]

  sealed trait Tagged[U]
  type @@[+T, U] = T with Tagged[U]

  class Tagger[U] {
    def apply[T](t: T): T @@ U = t.asInstanceOf[T @@ U]
  }
}
