package sangria.validation.rules

import scala.language.postfixOps

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema._
import sangria.validation._
import scala.collection.mutable.{ListBuffer, Set => MutableSet, ListMap => MutableMap}

/**
 * Overlapping fields can be merged
 *
 * A selection set is only valid if all fields (including spreading any
 * fragments) either correspond to distinct response names or can be merged
 * without ambiguity.
 */
class OverlappingFieldsCanBeMerged extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val comparedSet = new PairSet[ast.AstNode]

    override val onLeave: ValidationVisit = {
      case selCont: ast.SelectionContainer =>
        val fields = collectFieldASTsAndDefs(ctx, ctx.typeInfo.previousParentType, selCont)
        val conflicts = findConflicts(fields)

        if (conflicts.nonEmpty)
          Left(conflicts.toVector.map(c => FieldsConflictViolation(c.reason.fieldName, c.reason.reason, ctx.sourceMapper, c.fields flatMap (_.position))))
        else
          Right(Continue)
    }

    def findConflicts(fieldMap: CollectedFields): Seq[Conflict] = {
      val conflicts = ListBuffer[Conflict]()

      fieldMap.keys.foreach { outputName =>
        val fields  = fieldMap(outputName)

        if (fields.size > 1)
          for (i <- 0 until fields.size) {
            for (j <- i until fields.size) {
              findConflict(outputName, fields(i), fields(j)) match {
                case Some(conflict) => conflicts += conflict
                case None => // do nothing
              }
            }
          }
      }

      conflicts
    }

    def findConflict(outputName: String, pair1: (ast.Field, Option[Field[_, _]]), pair2: (ast.Field, Option[Field[_, _]])): Option[Conflict] = {
      val (ast1, def1) = pair1
      val (ast2, def2) = pair2

      if ((ast1 eq ast2) || comparedSet.contains(ast1, ast2)) {
        None
      } else {
        comparedSet.add(ast1, ast2)

        if (ast1.name != ast2.name) {
          Some(Conflict(ConflictReason(outputName, Left(s"'${ast1.name}' and '${ast2.name}' are different fields")), ast1 :: ast2 :: Nil))
        } else {
          val typeRes = for {
            field1 <- def1
            field2 <- def2
            type1 = SchemaRenderer.renderTypeName(field1.fieldType)
            type2 = SchemaRenderer.renderTypeName(field2.fieldType)
          } yield if (!sameType(type1, type2))
            Some(Conflict(ConflictReason(outputName, Left(s"they return differing types '$type1' and '$type2'")), ast1 :: ast2 :: Nil))
          else
            None

          typeRes.flatten match {
            case s @ Some(_) => s
            case None =>
              if (!sameArguments(ast1.arguments, ast2.arguments))
                Some(Conflict(ConflictReason(outputName, Left("they have differing arguments")), ast1 :: ast2 :: Nil))
              else if (!sameDirectives(ast1.directives, ast2.directives))
                Some(Conflict(ConflictReason(outputName, Left("they have differing directives")), ast1 :: ast2 :: Nil))
              else {
                val visitedFragmentNames = MutableSet[String]()
                val subfieldMap1 = collectFieldASTsAndDefs(ctx, def1.map (d => ctx.typeInfo.getNamedType(d.fieldType)), ast1, visitedFragmentNames)
                val subfieldMap2 = collectFieldASTsAndDefs(ctx, def2.map (d => ctx.typeInfo.getNamedType(d.fieldType)), ast2, visitedFragmentNames, subfieldMap1)
                val conflicts = findConflicts(subfieldMap2)

                if (conflicts.nonEmpty)
                  Some(Conflict(ConflictReason(outputName, Right(conflicts map (_.reason) toVector)),
                    conflicts.foldLeft(ast1 :: ast2 :: Nil){case (acc, Conflict(_, fields)) => acc ++ fields}))
                else
                  None
              }
          }
        }
      }
    }
  }

  type CollectedFields = MutableMap[String, ListBuffer[(ast.Field, Option[Field[_, _]])]]

  def sameDirectives(directives1: List[ast.Directive], directives2: List[ast.Directive]) =
    if (directives1.size != directives2.size) false
    else directives1.forall { d1 =>
      directives2.find(_.name == d1.name) match {
        case Some(d2) => sameArguments(d1.arguments, d2.arguments)
        case None => false
      }
    }

  def sameArguments(args1: List[ast.Argument], args2: List[ast.Argument]) =
    if (args1.size != args2.size) false
    else args1.forall { a1 =>
      args2.find(_.name == a1.name) match {
        case Some(a2) => sameValue(a1.value, a2.value)
        case None => false
      }
    }

  def sameValue(v1: ast.Value, v2: ast.Value) =
    QueryRenderer.render(v1, QueryRenderer.Compact) ==
      QueryRenderer.render(v2, QueryRenderer.Compact)

  def sameType(type1: String, type2: String) = type1 == type2

  /**
   * Given a selectionSet, adds all of the fields in that selection to
   * the passed in map of fields, and returns it at the end.
   *
   * Note: This is not the same as execution's collectFields because at static
   * time we do not know what object type will be used, so we unconditionally
   * spread in all fragments.
   */
  def collectFieldASTsAndDefs(
      ctx: ValidationContext,
      parentType: Option[Type],
      selCont: ast.SelectionContainer,
      visitedFragmentNames: MutableSet[String] = MutableSet(),
      astAndDefs: CollectedFields = MutableMap()): CollectedFields = {
    var aad = astAndDefs

    selCont.selections foreach {
      case astField: ast.Field =>
        val fieldDef = parentType flatMap {
          case tpe: ObjectLikeType[Any @unchecked, Any @unchecked] => tpe.getField(ctx.schema, astField.name).headOption
          case _ => None
        }

        if (!aad.contains(astField.outputName))
          aad(astField.outputName) = ListBuffer(astField -> fieldDef)
        else
          aad(astField.outputName) += astField -> fieldDef
      case frag: ast.InlineFragment =>
        aad = collectFieldASTsAndDefs(ctx, ctx.schema.getOutputType(frag.typeCondition, true), frag, visitedFragmentNames, aad)
      case frag: ast.FragmentSpread if visitedFragmentNames contains frag.name =>
        // do nothing at all
      case frag: ast.FragmentSpread  =>
        visitedFragmentNames += frag.name
        ctx.fragments.get(frag.name) match {
          case Some(fragDef) =>
            aad = collectFieldASTsAndDefs(ctx, ctx.schema.getOutputType(fragDef.typeCondition, true), fragDef, visitedFragmentNames, aad)
          case None => // do nothing
        }
    }

    aad
  }
}

case class Conflict(reason: ConflictReason, fields: List[ast.Field])
case class ConflictReason(fieldName: String, reason: Either[String, Vector[ConflictReason]])

/**
 * A way to keep track of pairs of things when the ordering of the pair does
 * not matter. We do this by maintaining a sort of double adjacency sets.
 */
private class PairSet[T] {
  val pairs = MutableMap[T, MutableSet[T]]()

  def contains(a: T, b: T) =
    pairs.contains(a) && pairs(a).contains(b)

  def add(a: T, b: T) = {
    addPair(a, b)
    addPair(b, a)
  }

  private def addPair(a: T, b: T) =
    if (!pairs.contains(a))
      pairs(a) = MutableSet(b)
    else
      pairs(a).add(b)
}