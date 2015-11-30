package sangria.validation.rules

import sangria.ast
import sangria.ast.{FragmentSpread, AstVisitor}
import sangria.ast.AstVisitorCommand._
import sangria.validation._

import scala.collection.mutable.{ListBuffer, Set ⇒ MutableSet, Stack ⇒ MutableStack}
import scala.language.postfixOps

class NoFragmentCycles extends ValidationRule {
   override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
     val spreadsInFragment =
       ctx.doc.definitions.collect{case fd: ast.FragmentDefinition ⇒ fd}.groupBy(_.name).mapValues { v ⇒
         val fd  = v.head
         val acc = ListBuffer[ast.FragmentSpread]()

         AstVisitor.visitAst(fd, onEnter = {
           case fs: ast.FragmentSpread ⇒
             acc += fs
             Continue
           case _ ⇒ Continue
         })

         acc.toVector
       }

     val knownToLeadToCycle = MutableSet[FragmentSpread]()

     override val onEnter: ValidationVisit = {
       case ast.FragmentDefinition(initialName, _, _, _, _) ⇒
         val spreadPath = MutableStack[FragmentSpread]()
         val errors = ListBuffer[Violation]()

         def detectCycleRecursive(fragmentName: String): Unit = {
           if (spreadsInFragment.contains(fragmentName)) {
             spreadsInFragment(fragmentName) foreach { spreadNode ⇒
               if (!knownToLeadToCycle.contains(spreadNode)) {
                 if (spreadNode.name == initialName) {
                   val cyclePath = spreadNode +: spreadPath

                   cyclePath foreach knownToLeadToCycle.add

                   val reversedPath = spreadPath.toList.reverse

                   errors += CycleErrorViolation(initialName, reversedPath map (_.name), ctx.sourceMapper, cyclePath.toList.reverse.flatMap(_.position))
                 } else if (!spreadPath.contains(spreadNode)) {
                   spreadPath.push(spreadNode)
                   detectCycleRecursive(spreadNode.name)
                   spreadPath.pop
                 }
               }
             }
           }
         }

         detectCycleRecursive(initialName)

         if (errors.nonEmpty) Left(errors.toVector) else Right(Continue)
     }
   }
 }