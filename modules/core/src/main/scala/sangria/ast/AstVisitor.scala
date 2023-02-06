package sangria.ast

import sangria.schema.Schema
import sangria.validation.TypeInfo
import sangria.visitor._

import scala.util.control.Breaks.{break, breakable}

trait AstVisitor {
  def onEnter: PartialFunction[AstNode, VisitorCommand] = PartialFunction.empty
  def onLeave: PartialFunction[AstNode, VisitorCommand] = PartialFunction.empty
}

object AstVisitor {
  import AstVisitorCommand._

  def apply(
      onEnter: PartialFunction[AstNode, VisitorCommand] = PartialFunction.empty,
      onLeave: PartialFunction[AstNode, VisitorCommand] = PartialFunction.empty
  ): DefaultAstVisitor = DefaultAstVisitor(onEnter, onLeave)

  def simple(
      onEnter: PartialFunction[AstNode, Unit] = PartialFunction.empty,
      onLeave: PartialFunction[AstNode, Unit] = PartialFunction.empty
  ): DefaultAstVisitor = DefaultAstVisitor(
    {
      case node if onEnter.isDefinedAt(node) =>
        onEnter(node)
        VisitorCommand.Continue
    },
    {
      case node if onLeave.isDefinedAt(node) =>
        onLeave(node)
        VisitorCommand.Continue
    }
  )

  def visit[T <: AstNode](root: T, visitor: AstVisitor): T =
    visit(
      root,
      node =>
        if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue,
      node =>
        if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue
    )

  def visitAstWithTypeInfo[T <: AstNode](schema: Schema[_, _], root: T)(
      visitorFn: TypeInfo => AstVisitor): T = {
    val typeInfo = new TypeInfo(schema)
    val visitor = visitorFn(typeInfo)

    visit(
      root,
      node => {
        typeInfo.enter(node)
        if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue
      },
      node => {
        typeInfo.leave(node)
        if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue
      }
    )
  }

  def visitAstWithState[S](schema: Schema[_, _], root: AstNode, state: S)(
      visitorFn: (TypeInfo, S) => AstVisitor): S = {
    val typeInfo = new TypeInfo(schema)
    val visitor = visitorFn(typeInfo, state)

    visit(
      root,
      node => {
        typeInfo.enter(node)
        if (visitor.onEnter.isDefinedAt(node)) visitor.onEnter(node) else VisitorCommand.Continue
      },
      node => {
        typeInfo.leave(node)
        if (visitor.onLeave.isDefinedAt(node)) visitor.onLeave(node) else VisitorCommand.Continue
      }
    )

    state
  }

  def visit[T <: AstNode](
      root: T,
      onEnter: AstNode => VisitorCommand,
      onLeave: AstNode => VisitorCommand): T =
    sangria.visitor.visit[AstNode](root, Visit[AstNode](onEnter, onLeave)).asInstanceOf[T]

  private[sangria] def visitAstRecursive(
      doc: AstNode,
      onEnter: AstNode => AstVisitorCommand.Value = _ => Continue,
      onLeave: AstNode => AstVisitorCommand.Value = _ => Continue): Unit = {

    def breakOrSkip(cmd: AstVisitorCommand.Value) = cmd match {
      case Break => break()
      case Skip => false
      case Continue => true
    }

    def loop(node: AstNode): Unit =
      node match {
        case n @ Document(defs, trailingComments, _, _) =>
          if (breakOrSkip(onEnter(n))) {
            defs.foreach(d => loop(d))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputDocument(defs, trailingComments, _, _) =>
          if (breakOrSkip(onEnter(n))) {
            defs.foreach(d => loop(d))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FragmentDefinition(_, cond, dirs, sels, vars, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(cond)
            dirs.foreach(d => loop(d))
            sels.foreach(s => loop(s))
            vars.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ OperationDefinition(_, _, vars, dirs, sels, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            vars.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            sels.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ VariableDefinition(_, tpe, default, dirs, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(tpe)
            default.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InlineFragment(cond, dirs, sels, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            cond.foreach(c => loop(c))
            dirs.foreach(d => loop(d))
            sels.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FragmentSpread(_, dirs, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ NotNullType(ofType, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(ofType)
            breakOrSkip(onLeave(n))
          }
        case n @ ListType(ofType, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(ofType)
            breakOrSkip(onLeave(n))
          }
        case n @ Field(_, _, args, dirs, sels, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            sels.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Argument(_, v, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(v)
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectField(_, v, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(v)
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Directive(_, args, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ListValue(vals, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            vals.foreach(v => loop(v))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectValue(fields, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(f => loop(f))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BigDecimalValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BooleanValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ Comment(_, _) =>
          if (breakOrSkip(onEnter(n))) {
            breakOrSkip(onLeave(n))
          }
        case n @ VariableValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ NullValue(comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ NamedType(_, _) =>
          if (breakOrSkip(onEnter(n))) {
            breakOrSkip(onLeave(n))
          }
        case n @ StringValue(_, _, _, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ BigIntValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ IntValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FloatValue(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }

        // SDL schema definition

        case n @ ScalarTypeDefinition(_, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ FieldDefinition(_, fieldType, args, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(fieldType)
            args.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputValueDefinition(_, valueType, default, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(valueType)
            default.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectTypeDefinition(
              _,
              interfaces,
              fields,
              dirs,
              description,
              comment,
              trailingComments,
              _) =>
          if (breakOrSkip(onEnter(n))) {
            interfaces.foreach(d => loop(d))
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InterfaceTypeDefinition(
              _,
              fields,
              dirs,
              description,
              comment,
              trailingComments,
              _,
              _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ UnionTypeDefinition(_, types, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            types.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumTypeDefinition(_, values, dirs, description, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            values.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumValueDefinition(_, dirs, description, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputObjectTypeDefinition(_, fields, dirs, _, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ObjectTypeExtensionDefinition(_, ints, fields, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            ints.foreach(d => loop(d))
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InterfaceTypeExtensionDefinition(_, fields, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ InputObjectTypeExtensionDefinition(_, fields, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            fields.foreach(d => loop(d))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ UnionTypeExtensionDefinition(_, types, dirs, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            types.foreach(t => loop(t))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ EnumTypeExtensionDefinition(_, values, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            values.foreach(t => loop(t))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ ScalarTypeExtensionDefinition(_, dirs, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ SchemaExtensionDefinition(ops, dirs, comment, tc, _) =>
          if (breakOrSkip(onEnter(n))) {
            ops.foreach(op => loop(op))
            dirs.foreach(d => loop(d))
            comment.foreach(s => loop(s))
            tc.foreach(c => loop(c))
            breakOrSkip(onLeave(n))
          }
        case n @ DirectiveDefinition(_, args, locations, description, _, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            args.foreach(d => loop(d))
            locations.foreach(d => loop(d))
            description.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ DirectiveLocation(_, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ SchemaDefinition(ops, dirs, descr, comment, trailingComments, _) =>
          if (breakOrSkip(onEnter(n))) {
            ops.foreach(s => loop(s))
            dirs.foreach(s => loop(s))
            descr.foreach(s => loop(s))
            comment.foreach(s => loop(s))
            trailingComments.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
        case n @ OperationTypeDefinition(_, tpe, comment, _) =>
          if (breakOrSkip(onEnter(n))) {
            loop(tpe)
            comment.foreach(s => loop(s))
            breakOrSkip(onLeave(n))
          }
      }

    breakable {
      loop(doc)
    }
  }
}
