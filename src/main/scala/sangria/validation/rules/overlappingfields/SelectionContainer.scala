package sangria.validation.rules.overlappingfields

import scala.collection.mutable

class SelectionContainer {

  // Tracking the state of computeEffectiveSelections
  private var inProgress: Boolean = false
  private var done: Boolean = false

  private val directSpreads: mutable.ArrayBuffer[SelectionContainer] = new mutable.ArrayBuffer()

  val directFields: mutable.ArrayBuffer[SelectionField] = new mutable.ArrayBuffer()

  /**
    * This selection set and all directly or indirectly included spreads.
    * Indirectly included spreads come from spreads in directly included
    * spreads, etc.
    */
  val effectiveSelections: mutable.LinkedHashSet[SelectionContainer] = {
    val l = new mutable.LinkedHashSet[SelectionContainer]()
    l.add(this)
    l
  }

  def addSpread(selectionContainer: SelectionContainer): Unit = {
    directSpreads += selectionContainer
  }

  def addField(field: SelectionField): Unit = {
    directFields += field
  }

  def computeEffectiveSelections(): Unit = {
    if (inProgress || done) return
    inProgress = true
    directFields.foreach { field =>
      field.childSelection.computeEffectiveSelections()
    }
    directSpreads.foreach { spread =>
      // prevent building cycles
      if (!spread.inProgress) {
        // prevent doing work twice
        if (spread.done) {
          if (!effectiveSelections.contains(spread)) {
            effectiveSelections ++= spread.effectiveSelections
          }
        } else {
          spread.computeEffectiveSelections()
          //effective selections of spread are also done after spread.computeEffectiveSelections()
          effectiveSelections ++= spread.effectiveSelections
        }
      }
    }
    inProgress = false
    done = true
  }
}
