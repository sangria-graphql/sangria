package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.validation._

import scala.collection.mutable.{ListBuffer, Set => MutableSet}

/** No unused fragments
  *
  * A GraphQL document is only valid if all fragment definitions are spread within operations, or
  * spread within other fragments spread within operations.
  */
class NoUnusedFragments extends ValidationRule {
  override def visitor(ctx: ValidationContext): AstValidatingVisitor = new AstValidatingVisitor {
    val fragmentDefs: ListBuffer[ast.FragmentDefinition] = ListBuffer[ast.FragmentDefinition]()
    val operationDefs: ListBuffer[ast.OperationDefinition] = ListBuffer[ast.OperationDefinition]()

    override val onEnter: ValidationVisit = visits(
      on[ast.OperationDefinition] { case od =>
        operationDefs += od
        AstVisitorCommand.RightSkip
      },
      on[ast.FragmentDefinition] { case fd =>
        fragmentDefs += fd
        AstVisitorCommand.RightSkip
      }
    )

    override val onLeave: ValidationVisit = visits(
      on[ast.Document] { case _ =>
        val fragmentNameUsed = MutableSet[String]()

        operationDefs.foreach(operation =>
          ctx.documentAnalyzer
            .getRecursivelyReferencedFragments(operation)
            .foreach(fragment => fragmentNameUsed += fragment.name))

        val errors = fragmentDefs.toVector
          .filter(fd => !fragmentNameUsed.contains(fd.name))
          .map(fd => UnusedFragmentViolation(fd.name, ctx.sourceMapper, fd.location.toList))

        if (errors.nonEmpty) Left(errors) else AstVisitorCommand.RightContinue
      }
    )
  }
}
