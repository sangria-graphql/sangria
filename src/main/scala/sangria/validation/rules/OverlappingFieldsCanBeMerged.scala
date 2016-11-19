package sangria.validation.rules

import scala.collection.mutable
import scala.language.postfixOps

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema._
import sangria.validation._
import scala.collection.mutable.{ListBuffer, Set ⇒ MutableSet, ListMap ⇒ MutableMap, LinkedHashSet}

/**
 * Overlapping fields can be merged
 *
 * A selection set is only valid if all fields (including spreading any
 * fragments) either correspond to distinct response names or can be merged
 * without ambiguity.
 */
class OverlappingFieldsCanBeMerged extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    // A memoization for when two fragments are compared "between" each other for
    // conflicts. Two fragments may be compared many times, so memoizing this can
    // dramatically improve the performance of this validator.
    val comparedFragments = new PairSet[String]

    // A cache for the "field map" and list of fragment names found in any given
    // selection set. Selection sets may be asked for this information multiple
    // times, so this improves the performance of this validator.
    val cachedFieldsAndFragmentNames = new MutableMap[List[ast.Selection], (MutableMap[String, ListBuffer[AstAndDef]], LinkedHashSet[String])]()

    /**
     * Algorithm:
     *
     * Conflicts occur when two fields exist in a query which will produce the same
     * response name, but represent differing values, thus creating a conflict.
     * The algorithm below finds all conflicts via making a series of comparisons
     * between fields. In order to compare as few fields as possible, this makes
     * a series of comparisons "within" sets of fields and "between" sets of fields.
     *
     * Given any selection set, a collection produces both a set of fields by
     * also including all inline fragments, as well as a list of fragments
     * referenced by fragment spreads.
     *
     * A) Each selection set represented in the document first compares "within" its
     * collected set of fields, finding any conflicts between every pair of
     * overlapping fields.
     * Note: This is the *only time* that a the fields "within" a set are compared
     * to each other. After this only fields "between" sets are compared.
     *
     * B) Also, if any fragment is referenced in a selection set, then a
     * comparison is made "between" the original set of fields and the
     * referenced fragment.
     *
     * C) Also, if multiple fragments are referenced, then comparisons
     * are made "between" each referenced fragment.
     *
     * D) When comparing "between" a set of fields and a referenced fragment, first
     * a comparison is made between each field in the original set of fields and
     * each field in the the referenced set of fields.
     *
     * E) Also, if any fragment is referenced in the referenced selection set,
     * then a comparison is made "between" the original set of fields and the
     * referenced fragment (recursively referring to step D).
     *
     * F) When comparing "between" two fragments, first a comparison is made between
     * each field in the first referenced set of fields and each field in the the
     * second referenced set of fields.
     *
     * G) Also, any fragments referenced by the first must be compared to the
     * second, and any fragments referenced by the second must be compared to the
     * first (recursively referring to step F).
     *
     * H) When comparing two fields, if both have selection sets, then a comparison
     * is made "between" both selection sets, first comparing the set of fields in
     * the first selection set with the set of fields in the second.
     *
     * I) Also, if any fragment is referenced in either selection set, then a
     * comparison is made "between" the other set of fields and the
     * referenced fragment.
     *
     * J) Also, if two fragments are referenced in both selection sets, then a
     * comparison is made "between" the two fragments.
     *
     */
    override val onEnter: ValidationVisit = {
      case selCont: ast.SelectionContainer if selCont.selections.nonEmpty ⇒
        val conflicts = findConflictsWithinSelectionSet(ctx.typeInfo.parentType, selCont)

        if (conflicts.nonEmpty)
          Left(conflicts.toVector.map(c ⇒ FieldsConflictViolation(c.reason.fieldName, c.reason.reason, ctx.sourceMapper, (c.fields1 ++ c.fields2) flatMap (_.position))))
        else
          AstVisitorCommand.RightContinue
    }

    def findConflictsWithinSelectionSet(parentType: Option[Type], selCont: ast.SelectionContainer): ListBuffer[Conflict] = {
      val conflicts = ListBuffer[Conflict]()

      val (fieldMap, fragmentNames) = getFieldsAndFragmentNames(parentType.asInstanceOf[Option[CompositeType[_]]], selCont)

      // (A) Find find all conflicts "within" the fields of this selection set.
      // Note: this is the *only place* `collectConflictsWithin` is called.
      collectConflictsWithin(conflicts, fieldMap)

      val fragmentNamesList = fragmentNames.toVector

      // (B) Then collect conflicts between these fields and those represented by
      // each spread fragment name found.
      fragmentNames.zipWithIndex foreach { case (fragmentName, idx) ⇒
        collectConflictsBetweenFieldsAndFragment(conflicts, fieldMap, fragmentName, false)

        for (i ← (idx + 1) until fragmentNamesList.size)
          collectConflictsBetweenFragments(conflicts, fragmentName, fragmentNamesList(i), false)
      }

      conflicts
    }

    // Collect all Conflicts between two collections of fields. This is similar to,
    // but different from the `collectConflictsWithin` function above. This check
    // assumes that `collectConflictsWithin` has already been called on each
    // provided collection of fields. This is true because this validator traverses
    // each individual selection set.
    def collectConflictsBetween(
        conflicts: ListBuffer[Conflict],
        fieldMap1: MutableMap[String, ListBuffer[AstAndDef]],
        fieldMap2: MutableMap[String, ListBuffer[AstAndDef]],
        parentFieldsAreMutuallyExclusive: Boolean): Unit = {
      // A field map is a keyed collection, where each key represents a response
      // name and the value at that key is a list of all fields which provide that
      // response name. For any response name which appears in both provided field
      // maps, each field from the first field map must be compared to every field
      // in the second field map to find potential conflicts.
      fieldMap1.keys foreach { outputName ⇒
        fieldMap2.get(outputName) match {
          case Some(fields2) ⇒
            val fields1 = fieldMap1(outputName)

            for {
              f1 ← fields1
              f2 ← fields2
            } findConflict(outputName, f1, f2, parentFieldsAreMutuallyExclusive) foreach (conflicts += _)

          case None ⇒ // It's ok, do nothing
        }
      }
    }

    // Collect all Conflicts "within" one collection of fields.
    def collectConflictsWithin(conflicts: ListBuffer[Conflict], fieldMap: MutableMap[String, ListBuffer[AstAndDef]]): Unit = {
      // A field map is a keyed collection, where each key represents a response
      // name and the value at that key is a list of all fields which provide that
      // response name. For every response name, if there are multiple fields, they
      // must be compared to find a potential conflict.
      fieldMap.keys foreach { outputName ⇒
        val fields = fieldMap(outputName)

        if (fields.size > 1)
          for {
            i ← 0 until fields.size
            j ← (i + 1) until fields.size
          } findConflict(outputName, fields(i), fields(j), false) foreach (conflicts += _)
      }
    }

    def getFieldsAndFragmentNames(
        parentType: Option[CompositeType[_]],
        selCont: ast.SelectionContainer): (MutableMap[String, ListBuffer[AstAndDef]], LinkedHashSet[String]) = {
      cachedFieldsAndFragmentNames.get(selCont.selections) match {
        case Some(cached) ⇒ cached
        case None ⇒
          val astAndDefs = MutableMap[String, ListBuffer[AstAndDef]]()
          val fragmentNames = mutable.LinkedHashSet[String]()

          collectFieldsAndFragmentNames(parentType, selCont, astAndDefs, fragmentNames)

          val cached = astAndDefs → fragmentNames

          cachedFieldsAndFragmentNames(selCont.selections) = cached
          cached
      }
    }

    // Given a reference to a fragment, return the represented collection of fields
    // as well as a list of nested fragment names referenced via fragment spreads.
    def getReferencedFieldsAndFragmentNames(fragment: ast.FragmentDefinition): (MutableMap[String, ListBuffer[AstAndDef]], LinkedHashSet[String]) = {
      cachedFieldsAndFragmentNames.get(fragment.selections) match {
        case Some(cached) ⇒ cached
        case None ⇒
          val fragmentType = ctx.schema.getOutputType(fragment.typeCondition, true).asInstanceOf[Option[CompositeType[_]]]

          getFieldsAndFragmentNames(fragmentType, fragment)
      }
    }

    def collectFieldsAndFragmentNames(
        parentType: Option[OutputType[_]],
        selCont: ast.SelectionContainer,
        astAndDefs: MutableMap[String, ListBuffer[AstAndDef]],
        fragmentNames: MutableSet[String]): Unit = {
      selCont.selections foreach {
        case field: ast.Field ⇒
          val fieldDef: Option[Field[_, _]] = parentType flatMap {
            case obj: ObjectLikeType[_, _] ⇒ obj.getField(ctx.schema, field.name).headOption
            case _ ⇒ None
          }

          val astAndDef = astAndDefs.get(field.outputName) match {
            case Some(list) ⇒ list
            case None ⇒
              val list = ListBuffer.empty[AstAndDef]
              astAndDefs(field.outputName) = list
              list
          }

          astAndDef += AstAndDef(field, parentType, fieldDef)

        case fragment: ast.FragmentSpread ⇒
          fragmentNames += fragment.name

        case fragment: ast.InlineFragment ⇒
          val inlineFragmentType = fragment.typeCondition flatMap (ctx.schema.getOutputType(_, true)) orElse parentType

          collectFieldsAndFragmentNames(inlineFragmentType, fragment, astAndDefs, fragmentNames)
      }
    }

    def findConflict(
        outputName: String,
        fieldInfo1: AstAndDef,
        fieldInfo2: AstAndDef,
        parentFieldsAreMutuallyExclusive: Boolean): Option[Conflict] = {
      val AstAndDef(ast1, parentType1, def1) = fieldInfo1
      val AstAndDef(ast2, parentType2, def2) = fieldInfo2

      // If it is known that two fields could not possibly apply at the same
      // time, due to the parent types, then it is safe to permit them to diverge
      // in aliased field or arguments used as they will not present any ambiguity
      // by differing.
      // It is known that two parent types could never overlap if they are
      // different Object types. Interface or Union types might overlap - if not
      // in the current state of the schema, then perhaps in some future version,
      // thus may not safely diverge.
      val areMutuallyExclusive = parentFieldsAreMutuallyExclusive || ((parentType1, parentType2) match {
        case (Some(pt1: ObjectType[_, _]), Some(pt2: ObjectType[_, _])) if pt1.name != pt2.name ⇒ true
        case _ ⇒ false
      })

      if (!areMutuallyExclusive && ast1.name != ast2.name)
        Some(Conflict(ConflictReason(outputName, Left(s"'${ast1.name}' and '${ast2.name}' are different fields")), ast1 :: Nil, ast2 :: Nil))
      else if (!areMutuallyExclusive && !sameArguments(ast1.arguments, ast2.arguments))
        Some(Conflict(ConflictReason(outputName, Left("they have differing arguments")), ast1 :: Nil, ast2 :: Nil))
      else {
        val typeRes = for {
          field1 ← def1
          field2 ← def2
        } yield if (doTypesConflict(field1.fieldType, field2.fieldType)) {
          val type1 = SchemaRenderer.renderTypeName(field1.fieldType)
          val type2 = SchemaRenderer.renderTypeName(field2.fieldType)

          Some(Conflict(ConflictReason(outputName, Left(s"they return conflicting types '$type1' and '$type2'")), ast1 :: Nil, ast2 :: Nil))
        } else None

        typeRes.flatten match {
          case s @ Some(_) ⇒ s
          case None ⇒
            val type1 = def1 map (d ⇒ ctx.typeInfo.getNamedType(d.fieldType))
            val type2 = def2 map (d ⇒ ctx.typeInfo.getNamedType(d.fieldType))
            val conflicts = findConflictsBetweenSubSelectionSets(areMutuallyExclusive, type1.asInstanceOf[Option[CompositeType[_]]], ast1, type2.asInstanceOf[Option[CompositeType[_]]], ast2)

            subfieldConflicts(conflicts, outputName, ast1, ast2)
        }
      }
    }

    // Find all conflicts found between two selection sets, including those found
    // via spreading in fragments. Called when determining if conflicts exist
    // between the sub-fields of two overlapping fields.
    def findConflictsBetweenSubSelectionSets(
        areMutuallyExclusive: Boolean,
        parentType1: Option[CompositeType[_]],
        selCont1: ast.SelectionContainer,
        parentType2: Option[CompositeType[_]],
        selCont2: ast.SelectionContainer): ListBuffer[Conflict] = {
      val conflicts = ListBuffer[Conflict]()

      val (fieldMap1, fragmentNames1) = getFieldsAndFragmentNames(parentType1, selCont1)
      val (fieldMap2, fragmentNames2) = getFieldsAndFragmentNames(parentType2, selCont2)

      // (H) First, collect all conflicts between these two collections of field.
      collectConflictsBetween(conflicts, fieldMap1, fieldMap2, areMutuallyExclusive)

      // (I) Then collect conflicts between the first collection of fields and
      // those referenced by each fragment name associated with the second.
      fragmentNames2 foreach (fragmentName ⇒
        collectConflictsBetweenFieldsAndFragment(conflicts, fieldMap1, fragmentName, areMutuallyExclusive))

      // (I) Then collect conflicts between the second collection of fields and
      // those referenced by each fragment name associated with the first.
      fragmentNames1 foreach (fragmentName ⇒
        collectConflictsBetweenFieldsAndFragment(conflicts, fieldMap2, fragmentName, areMutuallyExclusive))

      // (J) Also collect conflicts between any fragment names by the first and
      // fragment names by the second. This compares each item in the first set of
      // names to each item in the second set of names.
      for {
        fragmentName1 ← fragmentNames1
        fragmentName2 ← fragmentNames2
      } collectConflictsBetweenFragments(conflicts, fragmentName1, fragmentName2, areMutuallyExclusive)

      conflicts
    }

    // Collect all conflicts found between two fragments, including via spreading in
    // any nested fragments.
    def collectConflictsBetweenFragments(
        conflicts: ListBuffer[Conflict],
        fragmentName1: String,
        fragmentName2: String,
        areMutuallyExclusive: Boolean): Unit = {
      (ctx.fragments.get(fragmentName1), ctx.fragments.get(fragmentName2)) match {
        case (None, _) | (_, None) ⇒ // do nothing

        case (Some(f1), Some(f2)) if f1.name == f2.name ⇒
          // No need to compare a fragment to itself.
        case (Some(f1), Some(f2)) if comparedFragments.contains(f1.name, f2.name, areMutuallyExclusive) ⇒
          // Memoize so two fragments are not compared for conflicts more than once.

        case (Some(f1), Some(f2)) ⇒
          comparedFragments.add(f1.name, f2.name, areMutuallyExclusive)

          val (fieldMap1, fragmentNames1) = getReferencedFieldsAndFragmentNames(f1)
          val (fieldMap2, fragmentNames2) = getReferencedFieldsAndFragmentNames(f2)

          // (F) First, collect all conflicts between these two collections of fields
          // (not including any nested fragments).
          collectConflictsBetween(conflicts, fieldMap1, fieldMap2, areMutuallyExclusive)

          // (G) Then collect conflicts between the first fragment and any nested
          // fragments spread in the second fragment.
          fragmentNames2 foreach (fragmentName ⇒
            collectConflictsBetweenFragments(conflicts, fragmentName1, fragmentName, areMutuallyExclusive))

          // (G) Then collect conflicts between the first fragment and any nested
          // fragments spread in the second fragment.
          fragmentNames1 foreach (fragmentName ⇒
            collectConflictsBetweenFragments(conflicts, fragmentName, fragmentName2, areMutuallyExclusive))
      }
    }

    def collectConflictsBetweenFieldsAndFragment(
        conflicts: ListBuffer[Conflict],
        fieldMap: MutableMap[String, ListBuffer[AstAndDef]],
        fragmentName: String,
        areMutuallyExclusive: Boolean): Unit = {
      ctx.fragments.get(fragmentName) match {
        case Some(fragment) ⇒
          val (fieldMap2, fragmentNames2) = getReferencedFieldsAndFragmentNames(fragment)

          // (D) First collect any conflicts between the provided collection of fields
          // and the collection of fields represented by the given fragment.
          collectConflictsBetween(conflicts, fieldMap, fieldMap2, areMutuallyExclusive)

          // (E) Then collect any conflicts between the provided collection of fields
          // and any fragment names found in the given fragment.
          fragmentNames2 foreach (fragmentName ⇒
            collectConflictsBetweenFieldsAndFragment(conflicts, fieldMap, fragmentName, areMutuallyExclusive))

        case None ⇒ // do nothing
      }
    }

    // Given a series of Conflicts which occurred between two sub-fields, generate a single Conflict.
    def subfieldConflicts(conflicts: Seq[Conflict], outputName: String, ast1: ast.Field, ast2: ast.Field): Option[Conflict] =
      if (conflicts.nonEmpty)
        Some(Conflict(ConflictReason(outputName, Right(conflicts map (_.reason) toVector)),
          conflicts.foldLeft(ast1 :: Nil){case (acc, Conflict(_, fields, _)) ⇒ acc ++ fields},
          conflicts.foldLeft(ast2 :: Nil){case (acc, Conflict(_, _, fields)) ⇒ acc ++ fields}))
      else
        None

    // Two types conflict if both types could not apply to a value simultaneously.
    // Composite types are ignored as their individual field types will be compared
    // later recursively. However List and Non-Null types must match.
    def doTypesConflict(type1: OutputType[_], type2: OutputType[_]): Boolean = (type1, type2) match {
      case (ListType(ot1), ListType(ot2)) ⇒ doTypesConflict(ot1, ot2)
      case (ListType(_), _) | (_, ListType(_)) ⇒ true
      case (OptionType(ot1), OptionType(ot2)) ⇒ doTypesConflict(ot1, ot2)
      case (OptionType(_), _) | (_, OptionType(_)) ⇒ true
      case (nt1: LeafType, nt2: Named) ⇒ nt1.name != nt2.name
      case (nt1: Named, nt2: LeafType) ⇒ nt1.name != nt2.name
      case _ ⇒ false
    }

    def sameArguments(args1: List[ast.Argument], args2: List[ast.Argument]) =
      if (args1.size != args2.size) false
      else args1.forall { a1 ⇒
        args2.find(_.name == a1.name) match {
          case Some(a2) ⇒ sameValue(a1.value, a2.value)
          case None ⇒ false
        }
      }

    def sameValue(v1: ast.Value, v2: ast.Value) =
      QueryRenderer.render(v1, QueryRenderer.Compact) == QueryRenderer.render(v2, QueryRenderer.Compact)

  }
}

case class Conflict(reason: ConflictReason, fields1: List[ast.Field], fields2: List[ast.Field])
case class ConflictReason(fieldName: String, reason: Either[String, Vector[ConflictReason]])
case class AstAndDef(astField: ast.Field, tpe: Option[OutputType[_]], field: Option[Field[_, _]])

/**
 * A way to keep track of pairs of things when the ordering of the pair does
 * not matter. We do this by maintaining a sort of double adjacency sets.
 */
private class PairSet[T] {
  private val data = MutableMap[(T, T), Boolean]()

  def contains(a: T, b: T, areMutuallyExclusive: Boolean) =
    data get (a → b) match {
      case None ⇒ false
      // areMutuallyExclusive being false is a superset of being true,
      // hence if we want to know if this PairSet "has" these two with no
      // exclusivity, we have to ensure it was added as such.
      case Some(res) if !areMutuallyExclusive ⇒ !res
      case Some(_) ⇒ true
    }

  def add(a: T, b: T, areMutuallyExclusive: Boolean) = {
    addPair(a, b, areMutuallyExclusive)
    addPair(b, a, areMutuallyExclusive)
  }

  private def addPair(a: T, b: T, areMutuallyExclusive: Boolean) =
    data(a → b) = areMutuallyExclusive
}