package sangria.execution.batch

import sangria.ast
import sangria.schema.OutputType
import sangria.validation.SchemaBasedDocumentAnalyzer.VariableUsage

case class BatchExecutionPlan(
    exportOperations: Map[String, BatchExecutionPlan.ExportOperation],
    dependencies: Map[String, Vector[(String, Set[String])]])

object BatchExecutionPlan {
  case class Export(
      exportedName: String,
      path: Vector[String],
      astDirective: ast.Directive,
      resultType: OutputType[_])

  case class SpreadInfo(fragmentName: String, path: Vector[String])

  case class ExportOperation(
      operationName: String,
      variableDefs: Vector[ast.VariableDefinition],
      variableUsages: Vector[VariableUsage],
      exports: Vector[Export],
      fragmentSpreads: Set[SpreadInfo])

  case class ExportFragment(
      fragmentName: String,
      variableUsages: Vector[VariableUsage],
      exports: Vector[Export],
      fragmentSpreads: Set[SpreadInfo])
}
