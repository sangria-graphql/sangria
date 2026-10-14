package sangria.validation.rules.overlappingfields

import java.util
import java.util.function.Consumer

/** @param id
  *   unique per `SelectionBuilder`; only used for the hash code
  */
class SelectionContainer(id: Int) {

  // cheaper than the identity hash code, which is computed lazily by the JVM
  override def hashCode: Int = id * 0x9e3779b1

  // The last call of `SelectionContainer.children` that collected this container
  private var mark: Int = 0

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

  /** @param mark
    *   identifies this call among the calls on the same containers, and so must be unique per call:
    *   containers are only used by one validation at a time, so marking them is cheaper than
    *   collecting them in a set to remove duplicates
    */
  def children(
      fields: SortedArraySet[SelectionField],
      mark: Int): SortedArraySet[SelectionField] = {
    val childSelections = new util.ArrayList[SelectionContainer]()
    var expectedSize: Int = 0
    fields.forEach {
      new Consumer[SelectionField] {
        override def accept(field: SelectionField): Unit =
          field.childSelection.effectiveSelections.forEach {
            new Consumer[SelectionContainer] {
              override def accept(selection: SelectionContainer): Unit =
                if (selection.mark != mark) {
                  selection.mark = mark
                  childSelections.add(selection)
                  expectedSize += selection.directFields.size()
                }
            }
          }
      }
    }
    fieldSet(childSelections, expectedSize)
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
    fieldSet(effectiveSelections, expectedSize)
  }

  private def fieldSet(
      effectiveSelections: java.lang.Iterable[SelectionContainer],
      expectedSize: Int): SortedArraySet[SelectionField] = {
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
