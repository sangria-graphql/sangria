package sangria.validation.rules.overlappingfields

import java.util
import java.util.function
import java.util.function.Consumer

import sangria.ast
import sangria.schema.{CompositeType, OutputType}

/** For the validation we need another representation of the query that already contains the
  * effective selection sets for each field and certain properties of the fields. As we don't want
  * to adapt the sangria representation, we build our own here during traversal of the query.
  */
class SelectionBuilder {

  private val fieldBuilder = new SelectionField.Builder()

  // Not thread-safe on purpose: a SelectionBuilder is created per validation run (in the rule's
  // `visitor`) and the AST is visited sequentially, so no two threads share this counter.
  private var containerId: Int = 0

  private def newContainer(): SelectionContainer = {
    containerId += 1
    new SelectionContainer(containerId)
  }

  private val fragments: util.HashMap[String, SelectionContainer] = new util.HashMap()

  private val roots: util.ArrayList[SelectionContainer] = new util.ArrayList()

  private val stack: util.ArrayDeque[SelectionContainer] = new util.ArrayDeque[SelectionContainer]()

  def enterField(
      parentType: Option[CompositeType[_]],
      astField: ast.Field,
      outputType: Option[OutputType[_]]): Unit = {
    val field = fieldBuilder.build(astField, parentType, outputType)
    if (!stack.isEmpty) {
      stack.peek().addField(field)
    }
    stack.push(field.childSelection)
  }

  def spreadFragment(name: String): Unit =
    if (!stack.isEmpty) {
      stack.peek().addSpread(getOrInitFragment(name))
    }

  def enterFragmentDefinition(name: String): Unit =
    stack.push(getOrInitFragment(name))

  def enterGenericSelectionContainer(): Unit = {
    val selectionContainer = newContainer()
    if (stack.isEmpty) {
      roots.add(selectionContainer)
    } else {
      stack.peek().addSpread(selectionContainer)
    }
    stack.push(selectionContainer)
  }

  def leaveSelectionContainer(): Unit =
    stack.pop()

  /** @return
    *   A list of roots of the document
    */
  def build(): util.ArrayList[SelectionContainer] = {
    roots.forEach {
      new Consumer[SelectionContainer] {
        override def accept(root: SelectionContainer): Unit =
          root.computeEffectiveSelections()
      }
    }
    roots
  }

  private def getOrInitFragment(name: String): SelectionContainer =
    fragments.computeIfAbsent(
      name,
      new function.Function[String, SelectionContainer] {
        override def apply(key: String): SelectionContainer = newContainer()
      })
}
