package sangria.macros

import scala.quoted._
import sangria.parser.{QueryParser, SyntaxError}

object ParseMacro extends ToExprGivens {
  def impl(using Quotes)(strCtxExpr: Expr[StringContext]): Expr[sangria.ast.Document] =
    val parts = strCtxExpr.valueOrAbort.parts
    if parts.length > 1 then
      throw new Exception(
        "String interpolation is not supported for `graphql`/`gql` macro at the moment.")
    parts.headOption match
      case Some(str) =>
        Expr(QueryParser.parse(str).get)
      case None => throw new Exception("Invalid `graphql` invocation syntax.")

  def implInput(using Quotes)(strCtxExpr: Expr[StringContext]): Expr[sangria.ast.Value] =
    val parts = strCtxExpr.valueOrAbort.parts
    if parts.length > 1 then
      throw new Exception(
        "String interpolation is not supported for `graphqlInput`/`gqlInp` macro at the moment.")
    parts.headOption match
      case Some(str) =>
        Expr(QueryParser.parseInput(str).get)
      case None => throw new Exception("Invalid `graphql` invocation syntax.")

  def implInputDoc(using Quotes)(strCtxExpr: Expr[StringContext]): Expr[sangria.ast.InputDocument] =
    val parts = strCtxExpr.valueOrAbort.parts
    if parts.length > 1 then
      throw new Exception(
        "String interpolation is not supported for `gqlInpDoc` macro at the moment.")
    parts.headOption match
      case Some(str) =>
        Expr(QueryParser.parseInputDocument(str).get)
      case None => throw new Exception("Invalid `graphql` invocation syntax.")
}
