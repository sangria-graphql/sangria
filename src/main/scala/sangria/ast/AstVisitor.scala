package sangria.ast

import sangria.ast
import sangria.schema.Schema
import sangria.validation.{TypeInfo, Violation}
import sangria.visitor._

import scala.util.control.Breaks._

trait AstVisitor {
  def onEnter: PartialFunction[AstNode, VisitorCommand] = {case _ ⇒ VisitorCommand.Continue}
  def onLeave: PartialFunction[AstNode, VisitorCommand] = {case _ ⇒ VisitorCommand.Continue}
}

case class DefaultAstVisitor(
  override val onEnter: PartialFunction[AstNode, VisitorCommand] = {case _ ⇒ VisitorCommand.Continue},
  override val onLeave: PartialFunction[AstNode, VisitorCommand] = {case _ ⇒ VisitorCommand.Continue}
) extends AstVisitor

object AstVisitor {
  import ast.AstVisitorCommand._

  def apply(
    onEnter: PartialFunction[AstNode, VisitorCommand] = {case _ ⇒ VisitorCommand.Continue},
    onLeave: PartialFunction[AstNode, VisitorCommand] = {case _ ⇒ VisitorCommand.Continue}
  ) = DefaultAstVisitor(onEnter, onLeave)

  def simple(
    onEnter: PartialFunction[AstNode, Unit] = {case _ ⇒ ()},
    onLeave: PartialFunction[AstNode, Unit] = {case _ ⇒ ()}
  ) = DefaultAstVisitor(
    {
      case node if onEnter.isDefinedAt(node) ⇒
        onEnter(node)
        VisitorCommand.Continue
    }, {
      case node if onLeave.isDefinedAt(node) ⇒
        onLeave(node)
        VisitorCommand.Continue
    })

  def visit[T <: AstNode](root: T, visitor: AstVisitor): T =
    visit(root,
      node ⇒ if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue,
      node ⇒ if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue)

  def visitAstWithTypeInfo[T <: AstNode](schema: Schema[_, _], root: T)(visitorFn: TypeInfo ⇒ AstVisitor): T = {
    val typeInfo = new TypeInfo(schema)
    val visitor = visitorFn(typeInfo)

    visit(root,
      node ⇒ {
        typeInfo.enter(node)
        if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue
      },
      node ⇒ {
        typeInfo.leave(node)
        if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue
      })
  }

  def visitAstWithState[S](schema: Schema[_, _], root: AstNode, state: S)(visitorFn: (TypeInfo, S) ⇒ AstVisitor): S = {
    val typeInfo = new TypeInfo(schema)
    val visitor = visitorFn(typeInfo, state)

    visit(root,
      node ⇒ {
        typeInfo.enter(node)
        if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue
      },
      node ⇒ {
        typeInfo.leave(node)
        if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue
      })

    state
  }

  def visit[T <: AstNode](
      root: T,
      onEnter: AstNode ⇒ VisitorCommand,
      onLeave: AstNode ⇒ VisitorCommand): T =
    sangria.visitor.visit[AstNode](root, Visit[AstNode](onEnter, onLeave)).asInstanceOf[T]

  @deprecated("Please use `visit` instead. Its implementation is not recursive (so it will not overflow the stack) and allows AST modification.", "1.1.0")
  def visitAst(
      doc: AstNode,
      onEnter: AstNode ⇒ AstVisitorCommand.Value = _ ⇒ Continue,
      onLeave: AstNode ⇒ AstVisitorCommand.Value = _ ⇒ Continue): Unit =
    visitAstRecursive(doc, onEnter, onLeave)

  private[sangria] def visitAstRecursive(
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
        case n @ Document(defs, trailingComments, _, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            defs.foreach(d ⇒ loop(d))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FragmentDefinition(_, cond, dirs, sels, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(cond)
            dirs.foreach(d ⇒ loop(d))
            sels.foreach(s ⇒ loop(s))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ OperationDefinition(_, _, vars, dirs, sels, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            vars.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            sels.foreach(s ⇒ loop(s))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ VariableDefinition(_, tpe, default, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(tpe)
            default.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InlineFragment(cond, dirs, sels, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            cond.foreach(c ⇒ loop(c))
            dirs.foreach(d ⇒ loop(d))
            sels.foreach(s ⇒ loop(s))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FragmentSpread(_, dirs, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
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
        case n @ Field(_, _, args, dirs, sels, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            sels.foreach(s ⇒ loop(s))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Argument(_, v, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(v)
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectField(_, v, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(v)
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Directive(_, args, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ListValue(vals, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            vals.foreach(v ⇒ loop(v))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectValue(fields, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(f ⇒ loop(f))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BigDecimalValue(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BooleanValue(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Comment(_, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            breakOrSkip(onLeave(n))
          }
        case n @ VariableValue(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumValue(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ NullValue(comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ NamedType(_, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            breakOrSkip(onLeave(n))
          }
        case n @ StringValue(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BigIntValue(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ IntValue(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FloatValue(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }

        // IDL schema definition

        case n @ ScalarTypeDefinition(_, dirs, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FieldDefinition(_, fieldType, args, dirs, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(fieldType)
            args.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputValueDefinition(_, valueType, default, dirs, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(valueType)
            default.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectTypeDefinition(_, interfaces, fields, dirs, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            interfaces.foreach(d ⇒ loop(d))
            fields.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InterfaceTypeDefinition(_, fields, dirs, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ UnionTypeDefinition(_, types, dirs, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            types.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumTypeDefinition(_, values, dirs, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            values.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumValueDefinition(_, dirs, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputObjectTypeDefinition(_, fields, dirs, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d ⇒ loop(d))
            dirs.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ TypeExtensionDefinition(definition, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(definition)
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ DirectiveDefinition(_, args, locations, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d ⇒ loop(d))
            locations.foreach(d ⇒ loop(d))
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ DirectiveLocation(_, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ SchemaDefinition(ops, dirs, comment, trailingComments, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            ops.foreach(s ⇒ loop(s))
            dirs.foreach(s ⇒ loop(s))
            comment.foreach(s ⇒ loop(s))
            trailingComments.foreach(s ⇒ loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ OperationTypeDefinition(_, tpe, comment, _) ⇒
          if (breakOrSkip(onEnter(n))) {
            loop(tpe)
            comment.foreach(s ⇒ loop(s))
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

  val RightContinue: Either[Vector[Violation], AstVisitorCommand.Value] = Right(Continue)
  val RightSkip: Either[Vector[Violation], AstVisitorCommand.Value] = Right(Skip)
  val RightBreak: Either[Vector[Violation], AstVisitorCommand.Value] = Right(Break)
}
