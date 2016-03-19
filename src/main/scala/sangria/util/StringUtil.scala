package sangria.util

object StringUtil {
  private val camelToUpper = "_*([A-Z][a-z\\d]+)".r

  def camelCaseToUpperCase(name: String) =
    camelToUpper.findAllMatchIn(name).map(_.group(1).toUpperCase).mkString("_")
}
