package sangria.validation

import sangria.ast.{AstNode, AstVisitorCommand}

import scala.reflect.ClassTag

trait ValidationRule {
  type ValidationVisit =
    PartialFunction[AstNode, Either[Vector[Violation], AstVisitorCommand.Value]]

  trait AstValidatingVisitor {
    def onEnter: ValidationVisit = PartialFunction.empty
    def onLeave: ValidationVisit = PartialFunction.empty

    /** The classes of the nodes that `onEnter` can match (subclasses included), or `None` if it can
      * match any node. The validator skips the visitor for the other nodes. This is known when
      * `onEnter` is built with [[visits]]; it only needs to be overridden for other kinds of
      * partial functions. The declaration must not be narrower than what `onEnter` matches, and
      * must be the same for all the visitors of a class (the validator caches it per class).
      */
    def onEnterNodes: Option[Vector[Class[_]]] = TypedVisit.nodesOf(onEnter)

    /** Same as `onEnterNodes`, for `onLeave`. */
    def onLeaveNodes: Option[Vector[Class[_]]] = TypedVisit.nodesOf(onLeave)
  }

  type VisitResult = Either[Vector[Violation], AstVisitorCommand.Value]

  /** A handler for the nodes of type `T`, to be used with [[visits]]. */
  protected final def on[T <: AstNode](pf: PartialFunction[T, VisitResult])(implicit
      tag: ClassTag[T]): TypedVisit.Handler[VisitResult] =
    TypedVisit.on[T, VisitResult](pf)

  /** A [[ValidationVisit]] made of handlers for specific kinds of nodes. The validator only calls
    * it for those nodes.
    */
  protected final def visits(handlers: TypedVisit.Handler[VisitResult]*): ValidationVisit =
    new TypedVisit(handlers.toVector)

  def visitor(ctx: ValidationContext): AstValidatingVisitor
}
