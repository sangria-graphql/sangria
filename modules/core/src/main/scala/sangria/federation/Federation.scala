package sangria.federation

import sangria.ast.Document
import sangria.marshalling.InputUnmarshaller
import sangria.schema._

object Federation {
  import Query._

  def federate[Ctx, Node](
    schema: Schema[Ctx, Any],
    um: InputUnmarshaller[Node],
    resolvers: EntityResolver[Ctx, Node]*
  ): (Schema[Ctx, Any], InputUnmarshaller[Node])= {

    val resolversMap = resolvers.map(r => r.typename -> r).toMap
    val representationsArg = Argument("representations", ListInputType(_Any.__type[Node]))

    val entities = schema.allTypes.values.collect {
      case obj: ObjectType[Ctx, _]@unchecked if obj.astDirectives.exists(_.name == "key") => obj
    }.toList

    (schema.extend(
      Document(definitions = Vector(queryType(_entities))),
      AstSchemaBuilder.resolverBased[Ctx](
        FieldResolver.map(
          "Query" -> Map(
            "_entities" -> (ctx => ctx.withArgs(representationsArg) { anys =>
              Action.sequence(
                anys.map { any =>
                  val resolver = resolversMap(any.__typename)

                  any.fields.decode[resolver.Arg](resolver.decoder) match {
                    case Right(value) =>  resolver.resolve(value)
                    case Left(_) => LeafAction(None)
                  }
                })
            })
          )
        ),
        AdditionalTypes(_Any.__type[Node], _Entity(entities)))),
    upgrade(um))
  }

  def upgrade[Node](default: InputUnmarshaller[Node]): InputUnmarshaller[Node] = new InputUnmarshaller[Node] {

    override def getRootMapValue(node: Node, key: String): Option[Node] =
      default.getRootMapValue(node, key)
    override def isMapNode(node: Node): Boolean = default.isMapNode(node)
    override def getMapValue(node: Node, key: String): Option[Node] =
      default.getMapValue(node, key)
    override def getMapKeys(node: Node): Traversable[String] = default.getMapKeys(node)

    override def isListNode(node: Node): Boolean = default.isListNode(node)
    override def getListValue(node: Node): Seq[Node] = default.getListValue(node)

    override def isDefined(node: Node): Boolean = default.isDefined(node)
    override def isEnumNode(node: Node): Boolean = default.isEnumNode(node)
    override def isVariableNode(node: Node): Boolean = default.isVariableNode(node)

    override def getScalaScalarValue(node: Node): Any =
      default.getScalaScalarValue(node)

    override def getVariableName(node: Node): String = default.getVariableName(node)

    override def render(node: Node): String = default.render(node)

    override def isScalarNode(node: Node): Boolean =
      default.isMapNode(node) || default.isScalarNode(node)
    override def getScalarValue(node: Node): Any =
      if (default.isMapNode(node)) new NodeObject[Node] {
        override def __typename: String =
          getScalarValue(getMapValue(node, "__typename").get).asInstanceOf[String]

        override def decode[T](implicit ev: Decoder[Node, T]): Either[Exception, T] =
          ev.decode(node)
      }
      else default.getScalarValue(node)
  }
}
