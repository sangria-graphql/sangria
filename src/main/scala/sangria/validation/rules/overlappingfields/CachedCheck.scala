package sangria.validation.rules.overlappingfields

import scala.collection.mutable

/**
  * Implements the algorithm for validating "Field Selection Merging" as described in:
  * https://tech.xing.com/graphql-overlapping-fields-can-be-merged-fast-ea6e92e0a01
  *
  * Should have the same effect as the algorithm in the GraphQL Specification:
  * https://graphql.github.io/graphql-spec/draft/#sec-Field-Selection-Merging
  */
class CachedCheck {

  /**
    * We cache by FieldSet and use SortedArraySet, as it is fast to compare and iterate over
    */
  private type FieldSet = SortedArraySet[SelectionField]
  private type FieldSetBuilder = SortedArraySet.Builder[SelectionField]

  private val cache: mutable.Map[FieldSet, FieldSetCache] = new mutable.HashMap()

  def checkFieldsInSetCanMerge(selectionContainer: SelectionContainer, builder: SelectionConflictViolationsBuilder): Unit = {
    getCacheLine(selectionContainer.fieldSet).checkFieldsInSetCanMerge(builder)
  }

  private def getCacheLine(fields: FieldSet): FieldSetCache = {
    cache.getOrElseUpdate(fields, new FieldSetCache(fields))
  }

  private class FieldSetCache(val fields: FieldSet) {
    private var cacheGroupByOutputNames: mutable.ArrayBuffer[FieldSetCache] = _
    private var cacheMergeChildSelections: FieldSetCache = _
    private var cacheGroupByCommonParentTypes: mutable.ArrayBuffer[FieldSetCache] = _

    private var didRequireSameResponseShape: Boolean = false
    private var didRequireSameFieldNameAndArguments: Boolean = false
    private var didCheckSameResponseShape: Boolean = false
    private var didCheckSameFieldsForCoincidentParentTypes: Boolean = false

    def checkFieldsInSetCanMerge(builder: SelectionConflictViolationsBuilder): Unit = {
      checkSameResponseShape(builder)
      checkSameFieldsForCoincidentParentTypes(builder)
    }

    private def checkSameResponseShape(builder: SelectionConflictViolationsBuilder): Unit = {
      if (didCheckSameResponseShape) return
      didCheckSameResponseShape = true
      groupByOutputNames().foreach { fieldSet =>
        fieldSet
          .requireSameResponseShape(builder)
          .mergeChildSelections()
          .checkSameResponseShape(builder)
      }
    }

    private def checkSameFieldsForCoincidentParentTypes(builder: SelectionConflictViolationsBuilder): Unit = {
      if (didCheckSameFieldsForCoincidentParentTypes) return
      didCheckSameFieldsForCoincidentParentTypes = true
      groupByOutputNames().foreach { fieldSet =>
        fieldSet.groupByCommonParentTypes().foreach { fieldSet =>
          fieldSet
            .requireSameFieldNameAndArguments(builder)
            .mergeChildSelections()
            .checkSameFieldsForCoincidentParentTypes(builder)
        }
      }
    }

    private def requireSameResponseShape(builder: SelectionConflictViolationsBuilder): this.type = {
      if (didRequireSameResponseShape) return this
      didRequireSameResponseShape = true
      val fieldsWithKnownResponseShapes = groupByKnownResponseShape()
      val responseShapesNumber = fieldsWithKnownResponseShapes.size
      if (responseShapesNumber > 1) {
        val buckets = fieldsWithKnownResponseShapes.toArray
        val outputName = buckets(0)._2.head.outputName
        for {
          i <- 0 until buckets.length - 1
          j <- i + 1 until buckets.length
        } {
          val (a_key, a_value) = buckets(i)
          val (b_key, b_value) = buckets(j)
          val reason = a_key.conflictReason(b_key)
          builder.addConflict(outputName, reason, a_value, b_value)
        }
      }
      this
    }

    private def groupByKnownResponseShape(): mutable.LinkedHashMap[TypeShape.Known, mutable.ArrayBuffer[SelectionField]] = {
      val fieldsWithKnownResponseShapes = new mutable.LinkedHashMap[TypeShape.Known, mutable.ArrayBuffer[SelectionField]]()
      fields.foreach { field =>
        field.outputTypeShape match {
          case TypeShape.Unknown           => // ignore unknown response shapes
          case knownShape: TypeShape.Known =>
            val bucket = fieldsWithKnownResponseShapes.getOrElseUpdate(knownShape, new mutable.ArrayBuffer())
            bucket += field
        }
      }
      fieldsWithKnownResponseShapes
    }

    private def requireSameFieldNameAndArguments(builder: SelectionConflictViolationsBuilder): this.type = {
      if (didRequireSameFieldNameAndArguments) return this
      didRequireSameFieldNameAndArguments = true
      val fieldsWithSameNameAndArguments = groupByFieldNameAndArguments()
      val fieldNameAndArgumentsNumber = fieldsWithSameNameAndArguments.size
      if (fieldNameAndArgumentsNumber > 1) {
        val buckets = fieldsWithSameNameAndArguments.toArray
        val outputName = buckets(0)._2.head.outputName
        for {
          i <- 0 until buckets.length - 1
          j <- i + 1 until buckets.length
        } {
          val (a_key, a_value) = buckets(i)
          val (b_key, b_value) = buckets(j)
          val reason = a_key.conflictReason(b_key)
          builder.addConflict(outputName, reason, a_value, b_value)
        }
      }
      this
    }

    private def groupByFieldNameAndArguments(): mutable.LinkedHashMap[FieldNameAndArguments, mutable.ArrayBuffer[SelectionField]] = {
      val fieldsWithSameNameAndArguments = new mutable.LinkedHashMap[FieldNameAndArguments, mutable.ArrayBuffer[SelectionField]]()
      fields.foreach { field =>
        val bucket = fieldsWithSameNameAndArguments.getOrElseUpdate(field.fieldNameAndArguments, new mutable.ArrayBuffer())
        bucket += field
      }
      fieldsWithSameNameAndArguments
    }

    private def groupByOutputNames(): mutable.ArrayBuffer[FieldSetCache] = {
      if (cacheGroupByOutputNames == null) {
        val outputNames = new mutable.LinkedHashMap[OutputName, FieldSetBuilder]()
        fields.foreach { field =>
          outputNames
            .getOrElseUpdate(field.outputName, newFieldSetBuilder())
            .add(field)
        }
        val result = new mutable.ArrayBuffer[FieldSetCache](outputNames.size)
        outputNames.values.foreach(builder => result.append(getCacheLine(builder.build())))
        cacheGroupByOutputNames = result
        cacheGroupByOutputNames
      } else {
        cacheGroupByOutputNames
      }
    }

    private def mergeChildSelections(): FieldSetCache = {
      if (cacheMergeChildSelections == null) {
        val children = new mutable.LinkedHashSet[SelectionContainer]()
        fields.foreach { field => children ++= field.childSelection.effectiveSelections }
        val result = getCacheLine(SelectionContainer.fieldSet(children))
        cacheMergeChildSelections = result
        cacheMergeChildSelections
      } else {
        cacheMergeChildSelections
      }
    }

    private def groupByCommonParentTypes(): mutable.ArrayBuffer[FieldSetCache] = {
      if (cacheGroupByCommonParentTypes == null) {
        val fieldsWithAbstractParentTypes = new mutable.ArrayBuffer[SelectionField]()
        val fieldsWithConreteParents = new mutable.LinkedHashMap[TypeAbstractness.Concrete, FieldSetBuilder]()
        fields.foreach { field =>
          field.parentTypeAbstractness match {
            case TypeAbstractness.Abstract           =>
              fieldsWithAbstractParentTypes.append(field)
            case concrete: TypeAbstractness.Concrete =>
              fieldsWithConreteParents
                .getOrElseUpdate(concrete, newFieldSetBuilder())
                .add(field)
          }
        }
        val result = combineAbstractAndConcreteParentTypes(fieldsWithAbstractParentTypes, fieldsWithConreteParents)
        cacheGroupByCommonParentTypes = result
        cacheGroupByCommonParentTypes
      } else {
        cacheGroupByCommonParentTypes
      }
    }

    private def combineAbstractAndConcreteParentTypes(fieldsWithAbstractParentTypes: mutable.ArrayBuffer[SelectionField],
                                                      fieldsWithConreteParents: mutable.LinkedHashMap[TypeAbstractness.Concrete, FieldSetBuilder]): mutable.ArrayBuffer[FieldSetCache] = {
      if (fieldsWithConreteParents.isEmpty) {
        val list = new mutable.ArrayBuffer[FieldSetCache](1)
        val set = newFieldSetBuilder(fieldsWithAbstractParentTypes.size)
          .addAll(fieldsWithAbstractParentTypes)
          .build()
        list += getCacheLine(set)
        list
      } else {
        val list = new mutable.ArrayBuffer[FieldSetCache](fieldsWithConreteParents.size)
        if (fieldsWithAbstractParentTypes.isEmpty) {
          fieldsWithConreteParents.values.foreach { builder =>
            list += getCacheLine(builder.build())
          }
        } else {
          fieldsWithConreteParents.values.foreach { builder =>
            val set = builder.addAll(fieldsWithAbstractParentTypes).build()
            list += getCacheLine(set)
          }
        }
        list
      }
    }
  }

  private def newFieldSetBuilder(): FieldSetBuilder = {
    SortedArraySet.newBuilder[SelectionField]()
  }

  private def newFieldSetBuilder(sizeHint: Int): FieldSetBuilder = {
    SortedArraySet.newBuilder[SelectionField](sizeHint)
  }
}
