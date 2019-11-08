package sangria.validation.rules.overlappingfields

import sangria.ast
import sangria.parser.SourceMapper

class SelectionConflictViolationsBuilder(sourceMapper: Option[SourceMapper]) {

  private val violations = Vector.newBuilder[SelectionConflictViolation]

  def addConflict(outputName: OutputName, reason: String, fields1: Traversable[SelectionField], fields2: Traversable[SelectionField]): Unit = {
    val locations = List.newBuilder[ast.AstLocation]
    fields1.foreach { field =>
      field.astField.location.foreach(locations += _)
    }
    fields2.foreach { field =>
      field.astField.location.foreach(locations += _)
    }
    violations += SelectionConflictViolation(outputName, reason, sourceMapper, locations.result())
  }

  def result(): Vector[SelectionConflictViolation] = {
    violations.result()
  }
}
