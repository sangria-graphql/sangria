package sangria.ast

import sangria.validation.Violation

object AstVisitorCommand extends Enumeration {
  val Skip, Continue, Break = Value

  val RightContinue: Either[Vector[Violation], AstVisitorCommand.Value] = Right(Continue)
  val RightSkip: Either[Vector[Violation], AstVisitorCommand.Value] = Right(Skip)
  val RightBreak: Either[Vector[Violation], AstVisitorCommand.Value] = Right(Break)
}
