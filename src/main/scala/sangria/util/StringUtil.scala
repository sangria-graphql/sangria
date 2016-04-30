package sangria.util

object StringUtil {
  private val camelToUpper = "_*([A-Z][a-z\\d]+)".r

  def camelCaseToUnderscore(name: String) =
    camelToUpper findAllMatchIn name map (_.group(1).toLowerCase) mkString "_"

  /**
    * Given [ A, B, C ] return '"A", "B" or "C"'.
    */
  def quotedOrList(items: Seq[String], limit: Int = 5): String =
    if (items.isEmpty)
      throw new IllegalArgumentException("List is empty")
    else {
      val quoted = items map ("\"" + _ + "\"") take limit
      val start = quoted dropRight 1
      val last = quoted.last

      if (start.nonEmpty) s"${start mkString ", "} or $last"
      else last
    }
}
