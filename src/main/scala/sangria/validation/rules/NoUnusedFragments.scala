package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand._
import sangria.validation._

import scala.collection.mutable.{Set ⇒ MutableSet, Map ⇒ MutableMap, ListBuffer}
import scala.language.postfixOps

/**
 * No unused fragments
 *
 * A GraphQL document is only valid if all fragment definitions are spread
 * within operations, or spread within other fragments spread within operations.
 */
class NoUnusedFragments extends ValidationRule {
  override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
    val fragmentDefs = ListBuffer[ast.FragmentDefinition]()
    val spreadsWithinOperation = ListBuffer[MutableSet[String]]()
    val fragAdjacencies = MutableMap[String, MutableSet[String]]()
    var spreadNames = MutableSet[String]()

    override val onEnter: ValidationVisit = {
      case o: ast.OperationDefinition ⇒
        spreadNames = MutableSet[String]()
        spreadsWithinOperation += spreadNames
        Right(Continue)
      case fd: ast.FragmentDefinition ⇒
        fragmentDefs += fd
        spreadNames = MutableSet[String]()
        fragAdjacencies(fd.name) = spreadNames
        Right(Continue)
      case fs: ast.FragmentSpread ⇒
        spreadNames += fs.name
        Right(Continue)
     }

    override def onLeave: ValidationVisit = {
      case ast.Document(_, _, _) ⇒
        val fragmentNameUsed = MutableSet[String]()

        def reduceSpreadFragments(spreads: scala.collection.Set[String]): Unit = {
          spreads foreach { fragName ⇒
            if (!fragmentNameUsed.contains(fragName)) {
              fragmentNameUsed += fragName
              fragAdjacencies get fragName foreach reduceSpreadFragments
            }
          }
        }

        spreadsWithinOperation foreach reduceSpreadFragments

        val errors = fragmentDefs.toVector
          .filter(fd ⇒ !fragmentNameUsed.contains(fd.name))
          .map(fd ⇒ UnusedFragmentViolation(fd.name, ctx.sourceMapper, fd.position.toList))

        if (errors.nonEmpty) Left(errors) else Right(Continue)
    }
  }
}