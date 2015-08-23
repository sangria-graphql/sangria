package sangria.macros

import sangria.ast.Document
import sangria.parser.QueryParser

import scala.language.experimental.macros
import scala.reflect.macros.blackbox

object literal {

  implicit class LiteralGraphQLStringContext(val sc: StringContext) extends AnyVal {
    def graphql(): Document = macro Macro.impl
  }

  class Macro(context: blackbox.Context) extends {
    val c = context
  } with MacroAstLiftable {

    import c.universe._

    def impl() = {
      c.prefix.tree match {
        // Expects a string interpolation that doesn't contain any
        // expressions, thus containing only a single tree
        case Apply(_, List(Apply(_, t :: Nil))) =>
          val q"${gql: String}" = t
          q"${QueryParser.parse(gql.stripMargin).get}"
        case _ =>
          c.abort(c.enclosingPosition, "invalid")
      }
    }
  }

}
