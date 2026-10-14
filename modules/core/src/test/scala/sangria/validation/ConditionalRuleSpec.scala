package sangria.validation

import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.ast
import sangria.parser.QueryParser
import sangria.schema._
import sangria.validation.rules

/** The validator shares what it computes about the visitors between requests. A rule can validate
  * only under some condition (its visitor has no `onEnter` otherwise), so the same visitor class
  * can be used with and without handlers.
  */
class ConditionalRuleSpec extends AnyWordSpec with Matchers {
  private case object TestViolation extends Violation {
    val errorMessage = "test violation"
  }

  private class ConditionalRule(enabled: () => Boolean) extends ValidationRule {
    // the same (anonymous) visitor class, with a function that depends on the request
    override def visitor(ctx: ValidationContext): AstValidatingVisitor = new AstValidatingVisitor {
      override val onEnter: ValidationVisit =
        if (enabled()) visits(on[ast.Field] { case _ => Left(Vector(TestViolation)) })
        else PartialFunction.empty

      override val onLeave: ValidationVisit =
        if (enabled()) visits(on[ast.Field] { case _ => Left(Vector(TestViolation)) })
        else PartialFunction.empty
    }
  }

  // like a rule that does not use `visits` and so is called for every node
  private class ConditionalPlainRule(enabled: () => Boolean) extends ValidationRule {
    override def visitor(ctx: ValidationContext): AstValidatingVisitor = new AstValidatingVisitor {
      override val onEnter: ValidationVisit =
        if (enabled()) { case _: ast.Field => Left(Vector(TestViolation)) }
        else PartialFunction.empty
    }
  }

  private val schema =
    Schema(ObjectType("Query", fields[Unit, Unit](Field("a", StringType, resolve = _ => "a"))))
  private val query = QueryParser.parse("{ a }").get

  // the validator only dispatches by the kind of node with more than two rules
  private def validatorWith(rule: ValidationRule) =
    QueryValidator.ruleBased(
      List(rule, new rules.UniqueOperationNames, new rules.UniqueFragmentNames))

  private def violations(validator: QueryValidator) =
    validator.validateQuery(schema, query, Map.empty, None)

  for ((name, newRule) <- List[(String, (() => Boolean) => ValidationRule)](
      "a rule built with visits" -> (new ConditionalRule(_)),
      "a rule with a plain partial function" -> (new ConditionalPlainRule(_))))
    s"The validator, with $name that only validates under a condition" should {
      "validate when the rule was disabled for an earlier request" in {
        var enabled = false
        val validator = validatorWith(newRule(() => enabled))

        violations(validator) should be(Vector.empty)

        enabled = true
        violations(validator) should not be empty
      }

      "not validate when the rule was enabled for an earlier request" in {
        var enabled = true
        val validator = validatorWith(newRule(() => enabled))

        violations(validator) should not be empty

        enabled = false
        violations(validator) should be(Vector.empty)
      }
    }
}
