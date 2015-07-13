package sangria.renderer

import sangria.schema._

object SchemaRenderer {
  def renderTypeName(tpe: Type) = {
    def loop(t: Type, suffix: String): String = t match {
      case OptionType(ofType) => loop(tpe, "")
      case OptionInputType(ofType) => loop(tpe, "")
      case ListType(ofType) => s"[${loop(ofType, "!")}}]" + suffix
      case ListInputType(ofType) => s"[${loop(ofType, "!")}}]" + suffix
      case named: Named => named.name + suffix
    }

    loop(tpe, "!")
  }
}
