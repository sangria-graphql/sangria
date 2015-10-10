package sangria.ast

import sangria.ast
import scala.util.control.Breaks._

object AstVisitor {
  import ast.AstVisitorCommand._

  def visitAst(
      doc: AstNode,
      onEnter: AstNode ⇒ AstVisitorCommand.Value = _ ⇒ Continue,
      onLeave: AstNode ⇒ AstVisitorCommand.Value = _ ⇒ Continue): Unit = {

    def breakOrSkip(cmd: AstVisitorCommand.Value) = cmd match {
      case Break ⇒ break()
      case Skip ⇒ false
      case Continue ⇒ true
    }

    def loop(node: AstNode): Unit =
      node match {
        case n @ Document(defs, _, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            defs.foreach(d ⇒ loop(d))
            breakOrSkip(onLeave(n))
          }
        case n @ FragmentDefinition(_, cond, dirs, sels, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(cond)
            dirs.foreach(d ⇒ loop(d))
            sels.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ OperationDefinition(_, _, vars, dirs, sels, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            vars.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            sels.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ VariableDefinition(_, tpe, default, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(tpe)
            default.foreach(d ⇒ loop(d))
            breakOrSkip(onLeave(n))
          }
        case n @ InlineFragment(cond, dirs, sels, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            cond.foreach(c ⇒ loop(c))
            dirs.foreach(d ⇒ loop(d))
            sels.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FragmentSpread(_, dirs, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d ⇒ loop(d))
            breakOrSkip(onLeave(n))
          }
        case n @ NotNullType(ofType, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(ofType)
            breakOrSkip(onLeave(n))
          }
        case n @ ListType(ofType, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(ofType)
            breakOrSkip(onLeave(n))
          }
        case n @ Field(_, _, args, dirs, sels, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            sels.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Argument(_, v, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(v)
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectField(_, v, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(v)
            breakOrSkip(onLeave(n))
          }
        case n @ Directive(_, args, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d ⇒ loop(d))
            breakOrSkip(onLeave(n))
          }
        case n @ ListValue(vals, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            vals.foreach(v ⇒ loop(v))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectValue(fields, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(f ⇒ loop(f))
            breakOrSkip(onLeave(n))
          }
        case n ⇒
          if (breakOrSkip(onEnter(n))) {
            breakOrSkip(onLeave(n))
          }
      }

    breakable {
      loop(doc)
    }

  }
}

object AstVisitorCommand extends Enumeration {
  val Skip, Continue, Break = Value
}
