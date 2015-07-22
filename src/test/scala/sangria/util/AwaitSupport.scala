package sangria.util

import language.postfixOps

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._

trait AwaitSupport {
  implicit class WithAwait[T](f: Future[T]) {
    def await = Await.result(f, 2 seconds)
  }
}
