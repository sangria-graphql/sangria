package sangria.validation

import sangria.ast.Document
import sangria.schema.Schema

trait QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: Document): List[Violation]
}

object QueryValidator {
  val allRules: List[ValidationRule] = Nil

  val empty = new QueryValidator {
    def validateQuery(schema: Schema[_, _], queryAst: Document): List[Violation] = Nil
  }

  val default = new RuleBasedQueryValidator(allRules)
}

class RuleBasedQueryValidator(rules: List[ValidationRule]) extends QueryValidator {
  def validateQuery(schema: Schema[_, _], queryAst: Document): List[Violation] = ???
}
