package sangria.marshalling

import scala.annotation.implicitNotFound
import com.twitter.util.Try

@implicitNotFound(
  "Type ${T} cannot be used for schema materialization. Please consider defining an implicit instance of `InputParser` for it. It is required to parse the default values.")
trait InputParser[T] {
  def parse(str: String): Try[T]
}
