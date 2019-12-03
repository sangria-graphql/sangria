package sangria.validation.rules.experimental.overlappingfields

import sangria.ast.AstLocation
import sangria.parser.SourceMapper
import sangria.validation.AstNodeViolation

case class SelectionConflictViolation(outputName: OutputName, reason: String, sourceMapper: Option[SourceMapper], override val locations: List[AstLocation]) extends AstNodeViolation {
  override val simpleErrorMessage: String = s"Conflict at '$outputName' because $reason. Use different aliases on the fields to fetch both if this was intentional."
}
