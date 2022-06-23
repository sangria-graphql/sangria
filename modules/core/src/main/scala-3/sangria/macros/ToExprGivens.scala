package sangria.macros

import scala.quoted._
import sangria.ast._

trait ToExprGivens {

  given [T: scala.quoted.Type: ToExpr]: ToExpr[Vector[T]] with
    def apply(vector: Vector[T])(using Quotes): Expr[Vector[T]] =
      '{ ${ Expr.ofSeq(vector.map(summon[ToExpr[T]].apply)) }.toVector }

  given ToExpr[AstLocation] with
    def apply(position: AstLocation)(using Quotes) = position match
      case sangria.ast.AstLocation(id, i, l, c) =>
        '{ sangria.ast.AstLocation(${ Expr(id) }, ${ Expr(i) }, ${ Expr(l) }, ${ Expr(c) }) }

  given ToExpr[OperationType] with
    def apply(operationType: OperationType)(using Quotes) = operationType match
      case OperationType.Query => '{ sangria.ast.OperationType.Query }
      case OperationType.Mutation => '{ sangria.ast.OperationType.Mutation }
      case OperationType.Subscription => '{ sangria.ast.OperationType.Subscription }

  given ToExpr[sangria.ast.Type] with
    def apply(tpe: sangria.ast.Type)(using Quotes): Expr[sangria.ast.Type] = tpe match
      case NamedType(n, p) => '{ sangria.ast.NamedType(${ Expr(n) }, ${ Expr(p) }) }
      case NotNullType(o, p) => '{ sangria.ast.NotNullType(${ Expr(o) }, ${ Expr(p) }) }
      case ListType(o, p) => '{ sangria.ast.ListType(${ Expr(o) }, ${ Expr(p) }) }

  given ToExpr[sangria.ast.NamedType] with
    def apply(tpe: sangria.ast.NamedType)(using Quotes): Expr[sangria.ast.NamedType] = tpe match
      case NamedType(n, p) => '{ sangria.ast.NamedType(${ Expr(n) }, ${ Expr(p) }) }

  given ToExpr[Comment] with
    def apply(comment: Comment)(using Quotes) = comment match
      case Comment(l, p) =>
        '{ sangria.ast.Comment(${ Expr(l) }, ${ Expr(p) }) }

  given ToExpr[VariableDefinition] with
    def apply(varDef: VariableDefinition)(using Quotes) = varDef match
      case VariableDefinition(n, t, d, dirs, c, p) =>
        '{
          sangria.ast.VariableDefinition(
            ${ Expr(n) },
            ${ Expr(t) },
            ${ Expr(d) },
            ${ Expr(dirs) },
            ${ Expr(c) },
            ${ Expr(p) })
        }

  given ToExpr[InputValueDefinition] with
    def apply(inpValDef: InputValueDefinition)(using Quotes) = inpValDef match
      case InputValueDefinition(n, v, de, di, desc, c, p) =>
        '{
          sangria.ast.InputValueDefinition(
            ${ Expr(n) },
            ${ Expr(v) },
            ${ Expr(de) },
            ${ Expr(di) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(p) })
        }

  given ToExpr[OperationTypeDefinition] with
    def apply(inpOpTpeDef: OperationTypeDefinition)(using Quotes) = inpOpTpeDef match
      case OperationTypeDefinition(o, t, c, p) =>
        '{ OperationTypeDefinition(${ Expr(o) }, ${ Expr(t) }, ${ Expr(c) }, ${ Expr(p) }) }

  given ToExpr[EnumValueDefinition] with
    def apply(enumValDef: EnumValueDefinition)(using Quotes) = enumValDef match
      case EnumValueDefinition(n, d, desc, c, p) =>
        '{
          EnumValueDefinition(
            ${ Expr(n) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(p) })
        }

  given ToExpr[FieldDefinition] with
    def apply(liftFieldDef: FieldDefinition)(using Quotes) = liftFieldDef match
      case FieldDefinition(n, f, a, d, desc, c, p) =>
        '{
          FieldDefinition(
            ${ Expr(n) },
            ${ Expr(f) },
            ${ Expr(a) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(p) })
        }

  given ToExpr[DirectiveLocation] with
    def apply(dirLocDef: DirectiveLocation)(using Quotes) = dirLocDef match
      case DirectiveLocation(n, c, p) =>
        '{ DirectiveLocation(${ Expr(n) }, ${ Expr(c) }, ${ Expr(p) }) }

  given ToExpr[Definition] with
    def apply(definition: Definition)(using Quotes) = definition match
      case OperationDefinition(o, n, v, d, s, c, tc, p) =>
        '{
          sangria.ast.OperationDefinition(
            ${ Expr(o) },
            ${ Expr(n) },
            ${ Expr(v) },
            ${ Expr(d) },
            ${ Expr(s) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case FragmentDefinition(n, t, d, s, v, c, tc, p) =>
        '{
          sangria.ast.FragmentDefinition(
            ${ Expr(n) },
            ${ Expr(t) },
            ${ Expr(d) },
            ${ Expr(s) },
            ${ Expr(v) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }

      case DirectiveDefinition(n, a, l, desc, c, p) =>
        '{
          sangria.ast.DirectiveDefinition(
            ${ Expr(n) },
            ${ Expr(a) },
            ${ Expr(l) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(p) })
        }
      case SchemaDefinition(o, d, desc, c, tc, p) =>
        '{
          sangria.ast.SchemaDefinition(
            ${ Expr(o) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }

      case ObjectTypeExtensionDefinition(n, i, f, d, c, tc, p) =>
        '{
          sangria.ast.ObjectTypeExtensionDefinition(
            ${ Expr(n) },
            ${ Expr(i) },
            ${ Expr(f) },
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case InterfaceTypeExtensionDefinition(n, f, d, c, tc, p) =>
        '{
          sangria.ast.InterfaceTypeExtensionDefinition(
            ${ Expr(n) },
            ${ Expr(f) },
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case InputObjectTypeExtensionDefinition(n, f, d, c, tc, p) =>
        '{
          sangria.ast.InputObjectTypeExtensionDefinition(
            ${ Expr(n) },
            ${ Expr(f) },
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case UnionTypeExtensionDefinition(n, t, d, c, p) =>
        '{
          sangria.ast.UnionTypeExtensionDefinition(
            ${ Expr(n) },
            ${ Expr(t) },
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(p) })
        }
      case EnumTypeExtensionDefinition(n, v, d, c, tc, p) =>
        '{
          sangria.ast.EnumTypeExtensionDefinition(
            ${ Expr(n) },
            ${ Expr(v) },
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case ScalarTypeExtensionDefinition(n, d, c, p) =>
        '{
          sangria.ast.ScalarTypeExtensionDefinition(
            ${ Expr(n) },
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(p) })
        }
      case SchemaExtensionDefinition(o, d, c, tc, p) =>
        '{
          sangria.ast.SchemaExtensionDefinition(
            ${ Expr(o) },
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }

      case EnumTypeDefinition(n, v, d, desc, c, tc, p) =>
        '{
          sangria.ast.EnumTypeDefinition(
            ${ Expr(n) },
            ${ Expr(v) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case InputObjectTypeDefinition(n, f, d, desc, c, tc, p) =>
        '{
          sangria.ast.InputObjectTypeDefinition(
            ${ Expr(n) },
            ${ Expr(f) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case InterfaceTypeDefinition(n, f, d, desc, c, tc, p) =>
        '{
          sangria.ast.InterfaceTypeDefinition(
            ${ Expr(n) },
            ${ Expr(f) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case ObjectTypeDefinition(n, i, f, d, desc, c, tc, p) =>
        '{
          sangria.ast.ObjectTypeDefinition(
            ${ Expr(n) },
            ${ Expr(i) },
            ${ Expr(f) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case ScalarTypeDefinition(n, d, desc, c, p) =>
        '{
          sangria.ast.ScalarTypeDefinition(
            ${ Expr(n) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(p) })
        }
      case UnionTypeDefinition(n, t, d, desc, c, p) =>
        '{
          sangria.ast.UnionTypeDefinition(
            ${ Expr(n) },
            ${ Expr(t) },
            ${ Expr(d) },
            ${ Expr(desc) },
            ${ Expr(c) },
            ${ Expr(p) })
        }

  given ToExpr[Argument] with
    def apply(argument: Argument)(using Quotes) = argument match
      case Argument(n, v, c, p) =>
        '{ sangria.ast.Argument(${ Expr(n) }, ${ Expr(v) }, ${ Expr(c) }, ${ Expr(p) }) }

  given ToExpr[ObjectField] with
    def apply(namedValue: ObjectField)(using Quotes) = namedValue match
      case ObjectField(n, v, c, p) =>
        '{ sangria.ast.ObjectField(${ Expr(n) }, ${ Expr(v) }, ${ Expr(c) }, ${ Expr(p) }) }

  given ToExpr[sangria.ast.Value] with
    def apply(value: sangria.ast.Value)(using Quotes): Expr[Value] = value match
      case IntValue(v, c, p) => '{ sangria.ast.IntValue(${ Expr(v) }, ${ Expr(c) }, ${ Expr(p) }) }
      case FloatValue(v, c, p) =>
        '{ sangria.ast.FloatValue(${ Expr(v) }, ${ Expr(c) }, ${ Expr(p) }) }
      case StringValue(v, b, r, c, p) =>
        '{
          sangria.ast.StringValue(
            ${ Expr(v) },
            ${ Expr(b) },
            ${ Expr(r) },
            ${ Expr(c) },
            ${ Expr(p) })
        }
      case BooleanValue(v, c, p) =>
        '{ sangria.ast.BooleanValue(${ Expr(v) }, ${ Expr(c) }, ${ Expr(p) }) }
      case NullValue(c, p) => '{ sangria.ast.NullValue(${ Expr(c) }, ${ Expr(p) }) }
      case EnumValue(v, c, p) =>
        '{ sangria.ast.EnumValue(${ Expr(v) }, ${ Expr(c) }, ${ Expr(p) }) }
      case ListValue(v, c, p) =>
        '{ sangria.ast.ListValue(${ Expr(v) }, ${ Expr(c) }, ${ Expr(p) }) }
      case ObjectValue(f, c, p) =>
        '{ sangria.ast.ObjectValue(${ Expr(f) }, ${ Expr(c) }, ${ Expr(p) }) }
      case VariableValue(n, c, p) =>
        '{ sangria.ast.VariableValue(${ Expr(n) }, ${ Expr(c) }, ${ Expr(p) }) }
      case BigIntValue(v, c, p) =>
        '{
          sangria.ast.BigIntValue(
            scala.math.BigInt(${ Expr(v.toByteArray) }),
            ${ Expr(c) },
            ${ Expr(p) })
        }
      case sangria.ast.BigDecimalValue(v, c, p) =>
        '{
          sangria.ast.BigDecimalValue(
            scala.math.BigDecimal(${ Expr(v.toString()) }),
            ${ Expr(c) },
            ${ Expr(p) })
        }

  given ToExpr[sangria.ast.StringValue] with
    def apply(value: sangria.ast.StringValue)(using Quotes): Expr[StringValue] = value match
      case StringValue(v, b, r, c, p) =>
        '{
          sangria.ast.StringValue(
            ${ Expr(v) },
            ${ Expr(b) },
            ${ Expr(r) },
            ${ Expr(c) },
            ${ Expr(p) })
        }

  given ToExpr[Directive] with
    def apply(directive: Directive)(using Quotes) = directive match
      case Directive(n, a, c, p) =>
        '{ sangria.ast.Directive(${ Expr(n) }, ${ Expr(a) }, ${ Expr(c) }, ${ Expr(p) }) }

  given ToExpr[Selection] with
    def apply(selection: Selection)(using Quotes) = selection match
      case Field(a, n, arg, d, s, c, tc, p) =>
        '{
          sangria.ast.Field(
            ${ Expr(a) },
            ${ Expr(n) },
            ${ Expr(arg) },
            ${ Expr(d) },
            ${ Expr(s) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }
      case FragmentSpread(n, d, c, p) =>
        '{ sangria.ast.FragmentSpread(${ Expr(n) }, ${ Expr(d) }, ${ Expr(c) }, ${ Expr(p) }) }
      case InlineFragment(t, d, s, c, tc, p) =>
        '{
          sangria.ast.InlineFragment(
            ${ Expr(t) },
            ${ Expr(d) },
            ${ Expr(s) },
            ${ Expr(c) },
            ${ Expr(tc) },
            ${ Expr(p) })
        }

  given ToExpr[Document] with
    def apply(document: Document)(using Quotes) = document match
      case doc @ Document(d, c, p, _) =>
        '{
          Document(
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(p) },
            Some(
              new DefaultSourceMapper(
                ${ Expr(document.sourceMapper.get.id) },
                sangria.parser.ParserConfig.parboiledToSourceMapper(org.parboiled2.ParserInput(${
                  Expr(doc.source.get)
                }))
              )
            )
          )
        }

  given ToExpr[InputDocument] with
    def apply(inputDocument: InputDocument)(using Quotes) = inputDocument match
      case doc @ InputDocument(d, c, p, _) =>
        '{
          sangria.ast.InputDocument(
            ${ Expr(d) },
            ${ Expr(c) },
            ${ Expr(p) },
            _root_.scala.Some(
              new _root_.sangria.ast.DefaultSourceMapper(
                ${ Expr { doc.sourceMapper.get.id } },
                _root_.sangria.parser.ParserConfig
                  .parboiledToSourceMapper(_root_.org.parboiled2.ParserInput(${
                    Expr(doc.source.get)
                  }))
              )
            )
          )
        }
}
