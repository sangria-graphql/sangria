package sangria.validation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.ast
import sangria.ast.AstVisitorCommand

class TypedVisitSpec extends AnyWordSpec with Matchers {
  private object Rule extends ValidationRule {
    def visitor(ctx: ValidationContext): AstValidatingVisitor = new AstValidatingVisitor {}

    val visit: ValidationVisit = visits(
      on[ast.Field] { case field if field.name == "skip" => AstVisitorCommand.RightSkip },
      on[ast.SelectionContainer] { case _ => AstVisitorCommand.RightContinue },
      on[ast.Argument] { case _ => Left(Vector.empty) }
    )
  }

  private def field(name: String) = ast.Field(None, name, Vector.empty, Vector.empty, Vector.empty)

  "TypedVisit" should {
    "know the classes of the nodes it handles" in {
      TypedVisit.nodesOf(Rule.visit) should be(
        Some(Vector(classOf[ast.Field], classOf[ast.SelectionContainer], classOf[ast.Argument])))
    }

    "try the handlers in order, the first one that matches wins" in {
      // a field is also a selection container, but the first handler matches when it is applicable
      Rule.visit(field("skip")) should be(AstVisitorCommand.RightSkip)
      Rule.visit(field("other")) should be(AstVisitorCommand.RightContinue)
    }

    "not be defined for nodes without a matching handler" in {
      Rule.visit.isDefinedAt(ast.Argument("a", ast.IntValue(1))) should be(true)
      Rule.visit.isDefinedAt(ast.IntValue(1)) should be(false)
      Rule.visit.lift(ast.IntValue(1)) should be(None)
    }

    "treat the empty function as handling no node, and other functions as handling any" in {
      TypedVisit.nodesOf(PartialFunction.empty) should be(Some(Vector.empty))

      val custom: PartialFunction[ast.AstNode, Int] = { case _: ast.Field => 1 }
      TypedVisit.nodesOf(custom) should be(None)
    }
  }
}
