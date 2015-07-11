package sangria.util

import scala.io.Source

object FileUtil {
  def loadQuery(name: String) =
    Source.fromInputStream(this.getClass.getResourceAsStream(s"/queries/$name"), "UTF-8").mkString
}
