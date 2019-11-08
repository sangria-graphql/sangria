package sangria.validation.rules.overlappingfields

import scala.collection.mutable

class SelectionContainer {

  // Tracking the state of computeEffectiveSelections
  private var inProgress: Boolean = false
  private var done: Boolean = false

  // We initialize the collections with null to reduce allocations for fields that do not have child selections or fragment spreads

  private var directSpreads: mutable.ArrayBuffer[SelectionContainer] = _

  private var directFields: mutable.ArrayBuffer[SelectionField] = _

  private val effectiveSelectionsSet: mutable.LinkedHashSet[SelectionContainer] = {
    val l = new mutable.LinkedHashSet[SelectionContainer]()
    l.add(this)
    l
  }

  /**
    * This selection set and all directly or indirectly included spreads.
    * Indirectly included spreads come from spreads in directly included
    * spreads, etc.
    */
  def effectiveSelections: Iterable[SelectionContainer] = effectiveSelectionsSet

  def addSpread(selectionContainer: SelectionContainer): Unit = {
    if (directSpreads == null) {
      directSpreads = new mutable.ArrayBuffer()
    }
    directSpreads += selectionContainer
  }

  def addField(field: SelectionField): Unit = {
    if (directFields == null) {
      directFields = new mutable.ArrayBuffer()
    }
    directFields += field
  }

  def fieldSet: SortedArraySet[SelectionField] = SelectionContainer.fieldSet(effectiveSelectionsSet)

  def computeEffectiveSelections(): Unit = {
    if (inProgress || done) return
    inProgress = true
    if (directFields != null) {
      directFields.foreach { field =>
        field.childSelection.computeEffectiveSelections()
      }
    }
    if (directSpreads != null) {
      directSpreads.foreach { spread =>
        // prevent building cycles
        if (!spread.inProgress) {
          // prevent doing work twice
          if (spread.done) {
            if (!effectiveSelectionsSet.contains(spread)) {
              effectiveSelectionsSet ++= spread.effectiveSelectionsSet
            }
          } else {
            spread.computeEffectiveSelections()
            //effective selections of spread are also done after spread.computeEffectiveSelections()
            effectiveSelectionsSet ++= spread.effectiveSelectionsSet
          }
        }
      }
    }
    inProgress = false
    done = true
  }
}

object SelectionContainer {

  def fieldSet(selections: Iterable[SelectionContainer]): SortedArraySet[SelectionField] = {
    var expectedSize = 0
    selections.foreach { selection =>
      if (selection.directFields != null) {
        expectedSize += selection.directFields.size
      }
    }
    val builder = SortedArraySet.newBuilder[SelectionField](expectedSize)
    selections.foreach { selection =>
      if (selection.directFields != null) {
        builder.addAll(selection.directFields)
      }
    }
    builder.build()
  }
}
