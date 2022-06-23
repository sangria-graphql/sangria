package sangria.macros

import sangria.parser.{QueryParser, SyntaxError}

import scala.reflect.macros.blackbox

class ParseMacro(context: blackbox.Context)
    extends {
      val c = context
    }
    with MacroAstLiftable {
  import c.universe._

  def impl(args: Expr[Any]*) =
    if (args.nonEmpty)
      c.abort(
        c.enclosingPosition,
        "String interpolation is not supported for `graphql`/`gql` macro at the moment.")
    else
      c.prefix.tree match {
        // Expects a string interpolation that doesn't contain any
        // expressions, thus containing only a single tree
        case Apply(_, List(Apply(_, t :: Nil))) =>
          val q"${gql: String}" = t

          try q"${QueryParser.parse(gql).get}"
          catch {
            case error: SyntaxError => syntaxError(error)
          }
        case _ =>
          c.abort(c.enclosingPosition, "Invalid `graphql` invocation syntax.")
      }

  def implInput(args: Expr[Any]*) =
    if (args.nonEmpty)
      c.abort(
        c.enclosingPosition,
        "String interpolation is not supported for `graphqlInput`/`gqlInp` macro at the moment.")
    else
      c.prefix.tree match {
        // Expects a string interpolation that doesn't contain any
        // expressions, thus containing only a single tree
        case Apply(_, List(Apply(_, t :: Nil))) =>
          val q"${gql: String}" = t

          try q"${QueryParser.parseInput(gql).get}"
          catch {
            case error: SyntaxError => syntaxError(error)
          }
        case _ =>
          c.abort(c.enclosingPosition, "Invalid `graphql` invocation syntax.")
      }

  def implInputDoc(args: Expr[Any]*) =
    if (args.nonEmpty)
      c.abort(
        c.enclosingPosition,
        "String interpolation is not supported for `gqlInpDoc` macro at the moment.")
    else
      c.prefix.tree match {
        // Expects a string interpolation that doesn't contain any
        // expressions, thus containing only a single tree
        case Apply(_, List(Apply(_, t :: Nil))) =>
          val q"${gql: String}" = t

          try q"${QueryParser.parseInputDocument(gql).get}"
          catch {
            case error: SyntaxError => syntaxError(error)
          }
        case _ =>
          c.abort(c.enclosingPosition, "Invalid `graphql` invocation syntax.")
      }

  def syntaxError(error: SyntaxError) = {
    val errorPos = error.originalError.position
    val enclosingCol = if (errorPos.line == 1) calcStringStart else 0
    val source = c.enclosingPosition.source
    val line = source.lineToOffset(c.enclosingPosition.line + (errorPos.line - 2))
    val col = line + enclosingCol + (errorPos.column - 1)
    val pos = c.enclosingPosition.withPoint(col)

    c.abort(pos, error.formattedError(showPosition = false))
  }

  def calcStringStart: Int = {
    val source = c.enclosingPosition.source
    val content = source.lineToString(c.enclosingPosition.line - 1)
    val contentStart = content.substring(c.enclosingPosition.column - 1)
    val offset = "(\\w+\"+)".r.findFirstMatchIn(contentStart).fold(0)(_.end)

    c.enclosingPosition.column - 1 + offset
  }
}
