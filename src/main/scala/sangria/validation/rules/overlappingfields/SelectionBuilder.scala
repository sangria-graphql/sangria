package sangria.validation.rules.overlappingfields

import sangria.ast
import sangria.schema.{CompositeType, OutputType}

import scala.collection.mutable

/**
  * For the validation we need another representation of the query that
  * already contains the effective selection sets for each field and
  * certain properties of the fields. As we don't want to adapt the sangria
  * representation, we build our own here during traversal of the query.
  */
class SelectionBuilder {

  private val fieldBuilder = new SelectionField.Builder()

  private val fragments: mutable.Map[String, SelectionContainer] = new mutable.HashMap()

  private val roots: mutable.ArrayBuffer[SelectionContainer] = new mutable.ArrayBuffer()

  private val stack: mutable.ArrayBuffer[SelectionContainer] = new mutable.ArrayBuffer()

  def enterField(parentType: Option[CompositeType[_]], astField: ast.Field, outputType: Option[OutputType[_]]): Unit = {
    val field = fieldBuilder.build(astField, parentType, outputType)
    if (stack.nonEmpty) {
      stack.last.addField(field)
    }
    stack += field.childSelection
  }

  def spreadFragment(name: String): Unit = {
    if (stack.nonEmpty) {
      stack.last.addSpread(getOrInitFragment(name))
    }
  }

  def enterFragmentDefinition(name: String): Unit = {
    stack += getOrInitFragment(name)
  }

  def enterGenericSelectionContainer(): Unit = {
    val selectionContainer = new SelectionContainer
    if (stack.isEmpty) {
      roots += selectionContainer
    } else {
      stack.last.addSpread(selectionContainer)
    }
    stack += selectionContainer
  }

  def leaveSelectionContainer(): Unit = {
    stack.remove(stack.size - 1)
  }

  /**
    * @return A list of roots of the document
    */
  def build(): mutable.ArrayBuffer[SelectionContainer] = {
    roots.foreach { root =>
      root.computeEffectiveSelections()
    }
    roots
  }

  private def getOrInitFragment(name: String): SelectionContainer = {
    fragments.getOrElseUpdate(name, new SelectionContainer)
  }
}
