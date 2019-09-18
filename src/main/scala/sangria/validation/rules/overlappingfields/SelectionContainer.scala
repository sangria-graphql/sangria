package sangria.validation.rules.overlappingfields

import java.util

class SelectionContainer {

  // Tracking the state of computeEffectiveSelections
  private var inProgress: Boolean = false
  private var done: Boolean = false

  private val directSpreads: util.ArrayList[SelectionContainer] = new util.ArrayList()

  val directFields: util.ArrayList[SelectionField] = new util.ArrayList()

  /**
    * This selection set and all directly or indirectly included spreads.
    * Indirectly included spreads come from spreads in directly included
    * spreads, etc.
    */
  val effectiveSelections: util.LinkedHashSet[SelectionContainer] = {
    val l = new util.LinkedHashSet[SelectionContainer]()
    l.add(this)
    l
  }

  def addSpread(selectionContainer: SelectionContainer): Unit = {
    directSpreads.add(selectionContainer)
  }

  def addField(field: SelectionField): Unit = {
    directFields.add(field)
  }

  def computeEffectiveSelections(): Unit = {
    if (inProgress || done) return
    inProgress = true
    directFields.forEach { field =>
      field.childSelection.computeEffectiveSelections()
    }
    directSpreads.forEach { spread =>
      // prevent building cycles
      if (!spread.inProgress) {
        // prevent doing work twice
        if (spread.done) {
          if (!effectiveSelections.contains(spread)) {
            effectiveSelections.addAll(spread.effectiveSelections)
          }
        } else {
          spread.computeEffectiveSelections()
          //effective selections of spread are also done after spread.computeEffectiveSelections()
          effectiveSelections.addAll(spread.effectiveSelections)
        }
      }
    }
    inProgress = false
    done = true
  }
}
