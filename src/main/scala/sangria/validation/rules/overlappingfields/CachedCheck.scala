package sangria.validation.rules.overlappingfields

import java.util

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

  private val cache: util.HashMap[FieldSet, FieldSetCache] = new util.HashMap()

  def checkFieldsInSetCanMerge(selectionContainer: SelectionContainer, builder: SelectionConflictViolationsBuilder): Unit = {
    getCacheLine(computeFieldSet(selectionContainer.effectiveSelections)).checkFieldsInSetCanMerge(builder)
  }

  private def getCacheLine(fields: FieldSet): FieldSetCache = {
    cache.computeIfAbsent(fields, fields => new FieldSetCache(fields))
  }

  private def computeFieldSet(effectiveSelections: util.LinkedHashSet[SelectionContainer]): FieldSet = {
    var expectedSize = 0
    effectiveSelections.forEach(selection => expectedSize += selection.directFields.size())
    val builder = newFieldSetBuilder(expectedSize)
    effectiveSelections.forEach(selection => builder.addAll(selection.directFields))
    builder.build()
  }

  private class FieldSetCache(val fields: FieldSet) {
    private var cacheGroupByOutputNames: util.ArrayList[FieldSetCache] = _
    private var cacheMergeChildSelections: FieldSetCache = _
    private var cacheGroupByCommonParentTypes: util.ArrayList[FieldSetCache] = _

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
      groupByOutputNames().forEach { fieldSet =>
        fieldSet
          .requireSameResponseShape(builder)
          .mergeChildSelections()
          .checkSameResponseShape(builder)
      }
    }

    private def checkSameFieldsForCoincidentParentTypes(builder: SelectionConflictViolationsBuilder): Unit = {
      if (didCheckSameFieldsForCoincidentParentTypes) return
      didCheckSameFieldsForCoincidentParentTypes = true
      groupByOutputNames().forEach { fieldSet =>
        fieldSet.groupByCommonParentTypes().forEach { fieldSet =>
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
      val responseShapesNumber = fieldsWithKnownResponseShapes.size()
      if (responseShapesNumber > 1) {
        val buckets = fieldsWithKnownResponseShapes.entrySet().toArray(
          new Array[util.Map.Entry[TypeShape.Known, util.ArrayList[SelectionField]]](responseShapesNumber))
        val outputName = buckets(0).getValue.get(0).outputName
        for {
          i <- 0 until buckets.length - 1
          j <- i + 1 until buckets.length
        } {
          val a = buckets(i)
          val b = buckets(j)
          val reason = a.getKey.conflictReason(b.getKey)
          builder.addConflict(outputName, reason, a.getValue, b.getValue)
        }
      }
      this
    }

    private def groupByKnownResponseShape(): util.LinkedHashMap[TypeShape.Known, util.ArrayList[SelectionField]] = {
      val fieldsWithKnownResponseShapes = new util.LinkedHashMap[TypeShape.Known, util.ArrayList[SelectionField]]()
      fields.forEach { field =>
        field.outputTypeShape match {
          case TypeShape.Unknown           => // ignore unknown response shapes
          case knownShape: TypeShape.Known =>
            val bucket = fieldsWithKnownResponseShapes.computeIfAbsent(knownShape, _ => new util.ArrayList[SelectionField]())
            bucket.add(field)
        }
      }
      fieldsWithKnownResponseShapes
    }

    private def requireSameFieldNameAndArguments(builder: SelectionConflictViolationsBuilder): this.type = {
      if (didRequireSameFieldNameAndArguments) return this
      didRequireSameFieldNameAndArguments = true
      val fieldsWithSameNameAndArguments = groupByFieldNameAndArguments()
      val fieldNameAndArgumentsNumber = fieldsWithSameNameAndArguments.size()
      if (fieldNameAndArgumentsNumber > 1) {
        val buckets = fieldsWithSameNameAndArguments.entrySet().toArray(
          new Array[util.Map.Entry[FieldNameAndArguments, util.ArrayList[SelectionField]]](fieldNameAndArgumentsNumber))
        val outputName = buckets(0).getValue.get(0).outputName
        for {
          i <- 0 until buckets.length - 1
          j <- i + 1 until buckets.length
        } {
          val a = buckets(i)
          val b = buckets(j)
          val reason = a.getKey.conflictReason(b.getKey)
          builder.addConflict(outputName, reason, a.getValue, b.getValue)
        }
      }
      this
    }

    private def groupByFieldNameAndArguments(): util.LinkedHashMap[FieldNameAndArguments, util.ArrayList[SelectionField]] = {
      val fieldsWithSameNameAndArguments = new util.LinkedHashMap[FieldNameAndArguments, util.ArrayList[SelectionField]]()
      fields.forEach { field =>
        val bucket = fieldsWithSameNameAndArguments.computeIfAbsent(field.fieldNameAndArguments, _ => new util.ArrayList[SelectionField]())
        bucket.add(field)
      }
      fieldsWithSameNameAndArguments
    }

    private def groupByOutputNames(): util.ArrayList[FieldSetCache] = {
      if (cacheGroupByOutputNames == null) {
        val outputNames = new util.LinkedHashMap[OutputName, FieldSetBuilder]()
        fields.forEach { field =>
          outputNames
            .computeIfAbsent(field.outputName, _ => newFieldSetBuilder())
            .add(field)
        }
        val result = new util.ArrayList[FieldSetCache](outputNames.size())
        outputNames.values().forEach(builder => result.add(getCacheLine(builder.build())))
        cacheGroupByOutputNames = result
        cacheGroupByOutputNames
      } else {
        cacheGroupByOutputNames
      }
    }

    private def mergeChildSelections(): FieldSetCache = {
      if (cacheMergeChildSelections == null) {
        val children = new util.LinkedHashSet[SelectionContainer]()
        fields.forEach { field => children.addAll(field.childSelection.effectiveSelections) }
        val result = getCacheLine(computeFieldSet(children))
        cacheMergeChildSelections = result
        cacheMergeChildSelections
      } else {
        cacheMergeChildSelections
      }
    }

    private def groupByCommonParentTypes(): util.ArrayList[FieldSetCache] = {
      if (cacheGroupByCommonParentTypes == null) {
        val fieldsWithAbstractParentTypes = new util.ArrayList[SelectionField]()
        val fieldsWithConreteParents = new util.LinkedHashMap[TypeAbstractness.Concrete, FieldSetBuilder]()
        fields.forEach { field =>
          field.parentTypeAbstractness match {
            case TypeAbstractness.Abstract           =>
              fieldsWithAbstractParentTypes.add(field)
            case concrete: TypeAbstractness.Concrete =>
              fieldsWithConreteParents
                .computeIfAbsent(concrete, _ => newFieldSetBuilder())
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

    private def combineAbstractAndConcreteParentTypes(fieldsWithAbstractParentTypes: util.ArrayList[SelectionField],
                                                      fieldsWithConreteParents: util.LinkedHashMap[TypeAbstractness.Concrete, FieldSetBuilder]): util.ArrayList[FieldSetCache] = {
      if (fieldsWithConreteParents.isEmpty) {
        if (fieldsWithAbstractParentTypes.isEmpty) {
          new util.ArrayList[FieldSetCache](0)
        } else {
          val list = new util.ArrayList[FieldSetCache](1)
          val set = newFieldSetBuilder(fieldsWithAbstractParentTypes.size())
            .addAll(fieldsWithAbstractParentTypes)
            .build()
          list.add(getCacheLine(set))
          list
        }
      } else {
        val list = new util.ArrayList[FieldSetCache](fieldsWithConreteParents.size())
        if (fieldsWithAbstractParentTypes.isEmpty) {
          fieldsWithConreteParents.values().forEach { builder =>
            list.add(getCacheLine(builder.build()))
          }
        } else {
          fieldsWithConreteParents.values().forEach { builder =>
            val set = builder.addAll(fieldsWithAbstractParentTypes).build()
            list.add(getCacheLine(set))
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
