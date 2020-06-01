package sangria.validation.rules.experimental.overlappingfields

import java.util
import java.util.function.Consumer

import sangria.ast
import sangria.parser.SourceMapper

class SelectionConflictViolationsBuilder(sourceMapper: Option[SourceMapper]) {

  private val violations = Vector.newBuilder[SelectionConflictViolation]

  def addConflict(outputName: OutputName, reason: String, fields1: util.ArrayList[SelectionField], fields2: util.ArrayList[SelectionField]): Unit = {
    val locations = List.newBuilder[ast.AstLocation]
    fields1.forEach {
      new Consumer[SelectionField] {
        override def accept(field: SelectionField): Unit = {
          locations ++= field.astField.location
        }
      }
    }
    fields2.forEach {
      new Consumer[SelectionField] {
        override def accept(field: SelectionField): Unit = {
          locations ++= field.astField.location
        }
      }
    }
    violations += SelectionConflictViolation(outputName, reason, sourceMapper, locations.result())
  }

  def result(): Vector[SelectionConflictViolation] = {
    violations.result()
  }
}
