package sangria.macros

import sangria.parser.{SyntaxError, QueryParser}

import scala.reflect.macros.blackbox

class ParseMacro(context: blackbox.Context) extends {
  val c = context
} with MacroAstLiftable {

  import c.universe._

  def impl() = {
    c.prefix.tree match {
      // Expects a string interpolation that doesn't contain any
      // expressions, thus containing only a single tree
      case Apply(_, List(Apply(_, t :: Nil))) ⇒
        val q"${gql: String}" = t

        try {
          q"${QueryParser.parse(gql.stripMargin).get}"
        } catch {
          case syntaxError: SyntaxError ⇒
            c.abort(c.enclosingPosition, syntaxError.getMessage)
        }
      case _ ⇒
        c.abort(c.enclosingPosition, "Invalid `graphql` invocation syntax.")
    }
  }

  def implInput() = {
    c.prefix.tree match {
      // Expects a string interpolation that doesn't contain any
      // expressions, thus containing only a single tree
      case Apply(_, List(Apply(_, t :: Nil))) ⇒
        val q"${gql: String}" = t

        try {
          q"${QueryParser.parseInput(gql.stripMargin).get}"
        } catch {
          case syntaxError: SyntaxError ⇒
            c.abort(c.enclosingPosition, syntaxError.getMessage)
        }
      case _ ⇒
        c.abort(c.enclosingPosition, "Invalid `graphql` invocation syntax.")
    }
  }
}
