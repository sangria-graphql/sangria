package sangria.util

import scala.io.Source

object FileUtil extends StringMatchers {
  def loadQuery(name: String): String = loadResource("queries/" + name)

  def loadResource(path: String): String =
    Option(this.getClass.getResourceAsStream("/" + path)) match {
      case Some(res) => stripCarriageReturns(Source.fromInputStream(res, "UTF-8").mkString)
      case None => throw new IllegalArgumentException("Resource not found: /" + path)
    }
}
