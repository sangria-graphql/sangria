package sangria.federation

import sangria.schema._

trait EntityResolver[Ctx] {

  def typename: String

  def resolve(obj: NodeObject): LeafAction[Ctx, Option[_]]
}

object EntityResolver {

  def apply[Ctx, Val, Arg](
    __typeName: String,
    resolver: Arg => LeafAction[Ctx, Option[Val]]
  ) = new EntityResolver[Ctx] {

    def typename = __typeName

    /** @todo better error management */
    def resolve(obj: NodeObject): LeafAction[Ctx, Option[Val]] =
      obj.decode[Arg] match {
        case Right(value) => resolver(value)
        case Left(_) => None
      }
  }
}
