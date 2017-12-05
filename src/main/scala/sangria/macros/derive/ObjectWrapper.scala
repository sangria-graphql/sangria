package sangria.macros.derive

import scala.language.higherKinds

import sangria.schema.{Context, OutputType}

import scala.annotation.implicitNotFound

trait ObjectWrapper[W[_]] {
  def get[T](wrapper: W[T]): T
  def wrap[Ctx, Old, New](ctx: Context[Ctx, W[Old]], content: New): W[New]
}

@implicitNotFound("Argument does not satisfy constraints: ${A} Or ${B}")
trait Or[A, B] {
  def get: Either[A, B]
}

object Or {
  implicit def aExistsEv[A, B](implicit a: A) = new Or[A, B] {
    override def get = Left(a)
  }

  implicit def bExistsEv[A, B](implicit b: B) = new Or[A, B] {
    override def get = Right(b)
  }
}

object IsWrapped {
  def getWrapped[Normal, Wrapped](implicit ev: Or[GraphQLOutputTypeLookup[Normal], GraphQLOutputTypeLookup[Wrapped]]): OutputType[Any] =
    ev.get.fold(_.graphqlType, _.graphqlType)

  def isWrapped[Wrapped](implicit wrappedEv: GraphQLOutputTypeLookup[Wrapped] = null): Boolean = wrappedEv != null
}
