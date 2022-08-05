package sangria.macros.derive

import scala.quoted._

sealed trait PositionPointer
case class PositionByExpr(expr: Expr[Any]) extends PositionPointer
case class PositionByQuotes(quotes: Quotes) extends PositionPointer
