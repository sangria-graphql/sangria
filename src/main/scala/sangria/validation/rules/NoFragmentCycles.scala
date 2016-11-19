package sangria.validation.rules

import sangria.ast
import sangria.ast.AstVisitorCommand
import sangria.ast.AstVisitorCommand._
import sangria.validation._

import scala.collection.mutable.{Set ⇒ MutableSet, Map ⇒ MutableMap, Stack ⇒ MutableStack}
import scala.language.postfixOps

class NoFragmentCycles extends ValidationRule {
   override def visitor(ctx: ValidationContext) = new AstValidatingVisitor {
     val visitedFrags = MutableSet[String]()
     val spreadPath = MutableStack[ast.FragmentSpread]()
     val spreadPathIndexByName = MutableMap[String, Int]()

     def detectCycleRecursive(fragmentDef: ast.FragmentDefinition): Vector[Violation] = {
       visitedFrags += fragmentDef.name

       val spreadNodes = ctx.getFragmentSpreads(fragmentDef)

       if (spreadNodes.nonEmpty) {
         var errors: Vector[Violation] = Vector.empty

         spreadPathIndexByName(fragmentDef.name) = spreadPath.size

         spreadNodes.foreach { spreadNode ⇒
           spreadPathIndexByName.get(spreadNode.name) match {
             case None ⇒
               spreadPath.push(spreadNode)

               if (!visitedFrags.contains(spreadNode.name)) {
                 ctx.fragments.get(spreadNode.name) match {
                   case Some(frag) ⇒ errors = errors ++ detectCycleRecursive(frag)
                   case _ ⇒ // do nothing
                 }
               }

               spreadPath.pop()

             case Some(cycleIndex) ⇒
               val cyclePath = spreadPath.toList.reverse.slice(cycleIndex, spreadPath.size)

               errors = errors :+ CycleErrorViolation(
                 spreadNode.name,
                 cyclePath map (_.name),
                 ctx.sourceMapper,
                 (cyclePath :+ spreadNode).flatMap(_.position))
           }
         }

         spreadPathIndexByName.remove(fragmentDef.name)

         errors
       } else Vector.empty
     }

     override val onEnter: ValidationVisit = {
       case fragmentDef @ ast.FragmentDefinition(fragmentName, _, _, _, _, _, _) ⇒
         if (visitedFrags.contains(fragmentName)) AstVisitorCommand.RightSkip
         else {
           val errors = detectCycleRecursive(fragmentDef)

           if (errors.nonEmpty) Left(errors)
           else AstVisitorCommand.RightContinue
         }

       case _: ast.OperationDefinition ⇒ AstVisitorCommand.RightSkip
     }
   }
 }