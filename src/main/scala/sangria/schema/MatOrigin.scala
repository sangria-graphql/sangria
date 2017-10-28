package sangria.schema

import sangria.ast.Document

trait MatOrigin {
  def description: String

  override def toString = description
}

abstract class BaseMatOrigin(val description: String) extends MatOrigin

case class SDLOrigin(document: Document) extends BaseMatOrigin("SDL")
case class ExistingSchemaOrigin[Ctx, Val](schema: Schema[Ctx, Val]) extends BaseMatOrigin("existing schema")
case object StandaloneOrigin extends BaseMatOrigin("standalone")
