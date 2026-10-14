package sangria.validation

import sangria.ast.AstNode

import scala.reflect.ClassTag

/** A partial function over AST nodes that is made of handlers for specific kinds of nodes.
  *
  * Because the kinds of nodes it handles are known, the validator can skip it for the other nodes,
  * without a declaration that could get out of sync with what the function matches. The handlers
  * are tried in order and the first one that matches wins, like the cases of a pattern match.
  */
final class TypedVisit[R](val handlers: Vector[TypedVisit.Handler[R]])
    extends PartialFunction[AstNode, R] {

  /** The classes of the nodes the handlers apply to. */
  def nodes: Vector[Class[_]] = handlers.map(_.nodeClass)

  override def isDefinedAt(node: AstNode): Boolean = {
    var i = 0
    while (i < handlers.length) {
      if (handlers(i).isDefinedAt(node)) return true
      i += 1
    }
    false
  }

  override def applyOrElse[A1 <: AstNode, B1 >: R](node: A1, default: A1 => B1): B1 = {
    var i = 0
    while (i < handlers.length) {
      val result = handlers(i).applyOrNoMatch(node)
      if (result.asInstanceOf[AnyRef] ne TypedVisit.NoMatch) return result.asInstanceOf[B1]
      i += 1
    }
    default(node)
  }

  override def apply(node: AstNode): R =
    applyOrElse(node, (n: AstNode) => throw new MatchError(n))
}

object TypedVisit {

  // compared by reference
  private val NoMatch: AnyRef = new Object
  private val noMatch: Any => Any = _ => NoMatch

  final class Handler[R](val nodeClass: Class[_], pf: PartialFunction[Any, R]) {
    def isDefinedAt(node: AstNode): Boolean = nodeClass.isInstance(node) && pf.isDefinedAt(node)

    def applyOrNoMatch(node: AstNode): Any =
      if (nodeClass.isInstance(node)) pf.applyOrElse(node, noMatch) else NoMatch
  }

  /** A handler for the nodes of type `T`. */
  def on[T <: AstNode, R](pf: PartialFunction[T, R])(implicit tag: ClassTag[T]): Handler[R] =
    new Handler[R](tag.runtimeClass, pf.asInstanceOf[PartialFunction[Any, R]])

  /** The classes of the nodes a function handles: known for a [[TypedVisit]] and for the empty
    * function, `None` (any node) otherwise.
    */
  def nodesOf(visit: PartialFunction[AstNode, _]): Option[Vector[Class[_]]] =
    visit match {
      case typed: TypedVisit[_] => Some(typed.nodes)
      case empty if empty eq PartialFunction.empty => Some(Vector.empty)
      case _ => None
    }
}
