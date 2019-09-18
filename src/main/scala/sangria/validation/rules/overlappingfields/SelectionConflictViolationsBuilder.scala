package sangria.validation.rules.overlappingfields

import java.util

import sangria.ast
import sangria.parser.SourceMapper

class SelectionConflictViolationsBuilder(sourceMapper: Option[SourceMapper]) {

  private val reasons = new util.ArrayList[ConflictData]

  def addConflict(outputName: OutputName, reason: String, fields1: java.lang.Iterable[SelectionField], fields2: java.lang.Iterable[SelectionField]): Unit = {
    reasons.add(new ConflictData(outputName, reason, fields1, fields2))
  }

  def result(): Vector[SelectionConflictViolation] = {
    val builder = Vector.newBuilder[SelectionConflictViolation]
    reasons.forEach { reason =>
      builder += reason.result()
    }
    builder.result()
  }

  private class ConflictData(outputName: OutputName, reason: String, fields1: java.lang.Iterable[SelectionField], fields2: java.lang.Iterable[SelectionField]) {

    def result(): SelectionConflictViolation = {
      val locations = List.newBuilder[ast.AstLocation]
      fields1.forEach { field =>
        field.astField.location.foreach(locations += _)
      }
      fields2.forEach { field =>
        field.astField.location.foreach(locations += _)
      }
      SelectionConflictViolation(outputName, reason, sourceMapper, locations.result())
    }
  }

}
