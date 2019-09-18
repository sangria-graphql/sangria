package sangria.validation.rules.overlappingfields

import sangria.ast
import sangria.schema.{CompositeType, OutputType}

class SelectionField private(val id: Int,
                             val astField: ast.Field,
                             private val parentType: Option[CompositeType[_]],
                             private val outputType: Option[OutputType[_]]) {

  val outputName: OutputName = {
    new OutputName(astField.outputName)
  }

  val parentTypeAbstractness: TypeAbstractness = {
    TypeAbstractness(parentType)
  }

  val outputTypeShape: TypeShape = {
    TypeShape(outputType)
  }

  val fieldNameAndArguments: FieldNameAndArguments = {
    new FieldNameAndArguments(astField)
  }

  val childSelection: SelectionContainer = {
    new SelectionContainer
  }
}

object SelectionField {

  /**
    * This gives us a stable order of fields in sets.
    * It is determined by the order of traversal of the query.
    */
  implicit object OrderById extends Ordering[SelectionField] {
    override def compare(x: SelectionField, y: SelectionField): Int = {
      x.id.compareTo(y.id)
    }
  }

  class Builder {
    private var id: Int = 0

    def build(astField: ast.Field,
              parentType: Option[CompositeType[_]],
              outputType: Option[OutputType[_]]): SelectionField = {
      id += 1
      new SelectionField(
        id = id,
        astField = astField,
        parentType = parentType,
        outputType = outputType
      )
    }
  }

}
