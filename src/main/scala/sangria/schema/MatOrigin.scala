package sangria.schema

trait MatOrigin {
  def description: String

  override def toString = description
}

abstract class BaseMatOrigin(val description: String) extends MatOrigin

case object SDLOrigin extends BaseMatOrigin("SDL")
case object ExistingOrigin extends BaseMatOrigin("existing schema")
