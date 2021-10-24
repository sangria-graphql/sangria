package sangria.validation.rules.overlappingfields

import java.util
import java.util.function
import java.util.function.Consumer

/** Implements the algorithm for validating "Field Selection Merging" as described in:
  * https://tech.xing.com/graphql-overlapping-fields-can-be-merged-fast-ea6e92e0a01
  *
  * Should have the same effect as the algorithm in the GraphQL Specification:
  * https://graphql.github.io/graphql-spec/draft/#sec-Field-Selection-Merging
  */
class CachedCheck {

  /** We cache by FieldSet and use SortedArraySet, as it is fast to compare and iterate over
    */
  private type FieldSet = SortedArraySet[SelectionField]
  private type FieldSetBuilder = SortedArraySet.Builder[SelectionField]

  private val cache: util.HashMap[FieldSet, FieldSetCache] = new util.HashMap()

  def checkFieldsInSetCanMerge(
      fields: FieldSet,
      builder: SelectionConflictViolationsBuilder): Unit =
    getCacheLine(fields).checkFieldsInSetCanMerge(builder)

  private def getCacheLine(fields: FieldSet): FieldSetCache =
    cache.computeIfAbsent(
      fields,
      new function.Function[FieldSet, FieldSetCache] {
        override def apply(key: FieldSet): FieldSetCache = new FieldSetCache(key)
      })

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
      groupByOutputNames().forEach {
        new Consumer[FieldSetCache] {
          override def accept(fieldSet: FieldSetCache): Unit =
            fieldSet
              .requireSameResponseShape(builder)
              .mergeChildSelections()
              .checkSameResponseShape(builder)
        }
      }
    }

    private def checkSameFieldsForCoincidentParentTypes(
        builder: SelectionConflictViolationsBuilder): Unit = {
      if (didCheckSameFieldsForCoincidentParentTypes) return
      didCheckSameFieldsForCoincidentParentTypes = true
      groupByOutputNames().forEach {
        new Consumer[FieldSetCache] {
          override def accept(fieldSet: FieldSetCache): Unit =
            fieldSet.groupByCommonParentTypes().forEach {
              new Consumer[FieldSetCache] {
                override def accept(fieldSet: FieldSetCache): Unit =
                  fieldSet
                    .requireSameFieldNameAndArguments(builder)
                    .mergeChildSelections()
                    .checkSameFieldsForCoincidentParentTypes(builder)
              }
            }
        }
      }
    }

    private def requireSameResponseShape(builder: SelectionConflictViolationsBuilder): this.type = {
      if (didRequireSameResponseShape) return this
      didRequireSameResponseShape = true
      val fieldsWithKnownResponseShapes = groupByKnownResponseShape()
      val responseShapesNumber = fieldsWithKnownResponseShapes.size()
      if (responseShapesNumber > 1) {
        val buckets = fieldsWithKnownResponseShapes
          .entrySet()
          .toArray(
            new Array[util.Map.Entry[TypeShape.Known, util.ArrayList[SelectionField]]](
              responseShapesNumber))
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

    private def groupByKnownResponseShape()
        : util.LinkedHashMap[TypeShape.Known, util.ArrayList[SelectionField]] = {
      val fieldsWithKnownResponseShapes =
        new util.LinkedHashMap[TypeShape.Known, util.ArrayList[SelectionField]]()
      fields.forEach {
        new Consumer[SelectionField] {
          override def accept(field: SelectionField): Unit =
            field.outputTypeShape match {
              case TypeShape.Unknown => // ignore unknown response shapes
              case knownShape: TypeShape.Known =>
                fieldsWithKnownResponseShapes
                  .computeIfAbsent(
                    knownShape,
                    new function.Function[TypeShape.Known, util.ArrayList[SelectionField]] {
                      override def apply(key: TypeShape.Known): util.ArrayList[SelectionField] =
                        new util.ArrayList()
                    }
                  )
                  .add(field)
            }
        }
      }
      fieldsWithKnownResponseShapes
    }

    private def requireSameFieldNameAndArguments(
        builder: SelectionConflictViolationsBuilder): this.type = {
      if (didRequireSameFieldNameAndArguments) return this
      didRequireSameFieldNameAndArguments = true
      val fieldsWithSameNameAndArguments = groupByFieldNameAndArguments()
      val fieldNameAndArgumentsNumber = fieldsWithSameNameAndArguments.size()
      if (fieldNameAndArgumentsNumber > 1) {
        val buckets = fieldsWithSameNameAndArguments
          .entrySet()
          .toArray(
            new Array[util.Map.Entry[FieldNameAndArguments, util.ArrayList[SelectionField]]](
              fieldNameAndArgumentsNumber))
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

    private def groupByFieldNameAndArguments()
        : util.LinkedHashMap[FieldNameAndArguments, util.ArrayList[SelectionField]] = {
      val fieldsWithSameNameAndArguments =
        new util.LinkedHashMap[FieldNameAndArguments, util.ArrayList[SelectionField]]()
      fields.forEach {
        new Consumer[SelectionField] {
          override def accept(field: SelectionField): Unit =
            fieldsWithSameNameAndArguments
              .computeIfAbsent(
                field.fieldNameAndArguments,
                new function.Function[FieldNameAndArguments, util.ArrayList[SelectionField]] {
                  override def apply(key: FieldNameAndArguments): util.ArrayList[SelectionField] =
                    new util.ArrayList()
                }
              )
              .add(field)
        }
      }
      fieldsWithSameNameAndArguments
    }

    private def groupByOutputNames(): util.ArrayList[FieldSetCache] =
      if (cacheGroupByOutputNames == null) {
        val outputNames = new util.LinkedHashMap[OutputName, FieldSetBuilder]()
        fields.forEach {
          new Consumer[SelectionField] {
            override def accept(field: SelectionField): Unit =
              outputNames
                .computeIfAbsent(
                  field.outputName,
                  new function.Function[OutputName, FieldSetBuilder] {
                    override def apply(t: OutputName): FieldSetBuilder = newFieldSetBuilder()
                  })
                .add(field)
          }
        }
        val result = new util.ArrayList[FieldSetCache](outputNames.size())
        outputNames.values().forEach {
          new Consumer[FieldSetBuilder] {
            override def accept(builder: FieldSetBuilder): Unit =
              result.add(getCacheLine(builder.build()))
          }
        }
        cacheGroupByOutputNames = result
        cacheGroupByOutputNames
      } else {
        cacheGroupByOutputNames
      }

    private def mergeChildSelections(): FieldSetCache =
      if (cacheMergeChildSelections == null) {
        val result = getCacheLine(SelectionField.children(fields))
        cacheMergeChildSelections = result
        cacheMergeChildSelections
      } else {
        cacheMergeChildSelections
      }

    private def groupByCommonParentTypes(): util.ArrayList[FieldSetCache] =
      if (cacheGroupByCommonParentTypes == null) {
        val fieldsWithAbstractParentTypes = new util.ArrayList[SelectionField]()
        val fieldsWithConreteParents =
          new util.LinkedHashMap[TypeAbstractness.Concrete, FieldSetBuilder]()
        fields.forEach {
          new Consumer[SelectionField] {
            override def accept(field: SelectionField): Unit =
              field.parentTypeAbstractness match {
                case TypeAbstractness.Abstract =>
                  fieldsWithAbstractParentTypes.add(field)
                case concrete: TypeAbstractness.Concrete =>
                  fieldsWithConreteParents
                    .computeIfAbsent(
                      concrete,
                      new function.Function[TypeAbstractness.Concrete, FieldSetBuilder] {
                        override def apply(t: TypeAbstractness.Concrete): FieldSetBuilder =
                          newFieldSetBuilder()
                      }
                    )
                    .add(field)
              }
          }
        }
        val result = combineAbstractAndConcreteParentTypes(
          fieldsWithAbstractParentTypes,
          fieldsWithConreteParents)
        cacheGroupByCommonParentTypes = result
        cacheGroupByCommonParentTypes
      } else {
        cacheGroupByCommonParentTypes
      }

    private def combineAbstractAndConcreteParentTypes(
        fieldsWithAbstractParentTypes: util.ArrayList[SelectionField],
        fieldsWithConreteParents: util.LinkedHashMap[TypeAbstractness.Concrete, FieldSetBuilder])
        : util.ArrayList[FieldSetCache] =
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
          fieldsWithConreteParents.values().forEach {
            new Consumer[FieldSetBuilder] {
              override def accept(builder: FieldSetBuilder): Unit =
                list.add(getCacheLine(builder.build()))
            }
          }
        } else {
          fieldsWithConreteParents.values().forEach {
            new Consumer[FieldSetBuilder] {
              override def accept(builder: FieldSetBuilder): Unit = {
                val set = builder.addAll(fieldsWithAbstractParentTypes).build()
                list.add(getCacheLine(set))
              }
            }
          }
        }
        list
      }
  }

  private def newFieldSetBuilder(): FieldSetBuilder =
    SortedArraySet.newBuilder[SelectionField]()

  private def newFieldSetBuilder(sizeHint: Int): FieldSetBuilder =
    SortedArraySet.newBuilder[SelectionField](sizeHint)
}
