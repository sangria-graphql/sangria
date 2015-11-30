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
    val operationDefs = ListBuffer[ast.OperationDefinition]()

    override val onEnter: ValidationVisit = {
      case od: ast.OperationDefinition ⇒
        operationDefs += od
        Right(Skip)

      case fd: ast.FragmentDefinition ⇒
        fragmentDefs += fd
        Right(Skip)
     }

    override def onLeave: ValidationVisit = {
      case ast.Document(_, _, _) ⇒
        val fragmentNameUsed = MutableSet[String]()

        operationDefs.foreach(operation ⇒
          ctx.getRecursivelyReferencedFragments(operation)
            .foreach(fragment ⇒ fragmentNameUsed += fragment.name))

        val errors = fragmentDefs.toVector
          .filter(fd ⇒ !fragmentNameUsed.contains(fd.name))
          .map(fd ⇒ UnusedFragmentViolation(fd.name, ctx.sourceMapper, fd.position.toList))

        if (errors.nonEmpty) Left(errors) else Right(Continue)
    }
  }
}