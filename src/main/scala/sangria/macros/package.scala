package sangria

import scala.language.experimental.{macros ⇒ `scalac, please just let me do it!`}

import sangria.ast.{Document, Value}
import sangria.parser.{SyntaxError, QueryParser}

import scala.reflect.macros.blackbox

package object macros {
  implicit class LiteralGraphQLStringContext(val sc: StringContext) extends AnyVal {
    def graphql(): Document = macro Macro.impl
    def graphqlInput(): Value = macro Macro.implInput
  }

  class Macro(context: blackbox.Context) extends {
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
}
