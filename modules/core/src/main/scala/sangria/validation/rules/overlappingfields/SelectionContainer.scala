package sangria.validation.rules.overlappingfields

import java.util
import java.util.function.Consumer

class SelectionContainer {

  // Tracking the state of computeEffectiveSelections
  private var inProgress: Boolean = false
  private var done: Boolean = false

  private val directSpreads: util.ArrayList[SelectionContainer] = new util.ArrayList()

  private val directFields: util.ArrayList[SelectionField] = new util.ArrayList()

  /** This selection set and all directly or indirectly included spreads. Indirectly included
    * spreads come from spreads in directly included spreads, etc.
    */
  private val effectiveSelections: util.LinkedHashSet[SelectionContainer] = {
    val l = new util.LinkedHashSet[SelectionContainer]()
    l.add(this)
    l
  }

  def addSpread(selectionContainer: SelectionContainer): Unit =
    directSpreads.add(selectionContainer)

  def addField(field: SelectionField): Unit =
    directFields.add(field)

  def fieldSet: SortedArraySet[SelectionField] =
    SelectionContainer.fieldSet(effectiveSelections)

  def computeEffectiveSelections(): Unit = {
    if (inProgress || done) return
    inProgress = true
    directFields.forEach {
      new Consumer[SelectionField] {
        override def accept(field: SelectionField): Unit =
          field.childSelection.computeEffectiveSelections()
      }
    }
    directSpreads.forEach {
      new Consumer[SelectionContainer] {
        override def accept(spread: SelectionContainer): Unit =
          // prevent building cycles
          if (!spread.inProgress) {
            // prevent doing work twice
            if (spread.done) {
              if (!effectiveSelections.contains(spread)) {
                effectiveSelections.addAll(spread.effectiveSelections)
              }
            } else {
              spread.computeEffectiveSelections()
              // effective selections of spread are also done after spread.computeEffectiveSelections()
              effectiveSelections.addAll(spread.effectiveSelections)
            }
          }
      }
    }
    inProgress = false
    done = true
  }
}

object SelectionContainer {

  def children(fields: SortedArraySet[SelectionField]): SortedArraySet[SelectionField] = {
    val childSelections = new util.LinkedHashSet[SelectionContainer]()
    fields.forEach {
      new Consumer[SelectionField] {
        override def accept(field: SelectionField): Unit =
          childSelections.addAll(field.childSelection.effectiveSelections)
      }
    }
    SelectionContainer.fieldSet(childSelections)
  }

  private def fieldSet(effectiveSelections: util.LinkedHashSet[SelectionContainer])
      : SortedArraySet[SelectionField] = {
    var expectedSize: Int = 0
    effectiveSelections.forEach {
      new Consumer[SelectionContainer] {
        override def accept(selection: SelectionContainer): Unit =
          expectedSize += selection.directFields.size()
      }
    }
    val builder = SortedArraySet.newBuilder[SelectionField](expectedSize)
    effectiveSelections.forEach {
      new Consumer[SelectionContainer] {
        override def accept(selection: SelectionContainer): Unit =
          builder.addAll(selection.directFields)
      }
    }
    builder.build()
  }
}
