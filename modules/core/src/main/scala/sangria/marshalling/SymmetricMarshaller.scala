package sangria.marshalling

trait SymmetricMarshaller[T] {
  def marshaller: ResultMarshaller
  def inputUnmarshaller: InputUnmarshaller[T]
}

object SymmetricMarshaller extends SymmetricMarshallerLowProImplicits {
  case class DefaultSymmetricMarshaller[T](
      marshaller: ResultMarshaller,
      inputUnmarshaller: InputUnmarshaller[T])
      extends SymmetricMarshaller[T]

  implicit def symmetric[T: ResultMarshallerForType: InputUnmarshaller]: SymmetricMarshaller[T] =
    DefaultSymmetricMarshaller(
      implicitly[ResultMarshallerForType[T]].marshaller,
      implicitly[InputUnmarshaller[T]])
}

abstract class SymmetricMarshallerLowProImplicits {
  val defaultInput =
    SymmetricMarshaller.DefaultSymmetricMarshaller[Any](
      scalaMarshalling.scalaResultMarshaller,
      scalaMarshalling.scalaInputUnmarshaller[Any].asInstanceOf[InputUnmarshaller[Any]])

  // `T =:= Any` is the only constraint that makes type inference work
  implicit def default[T](implicit ev: T =:= Any): SymmetricMarshaller[T] =
    defaultInput.asInstanceOf[SymmetricMarshaller[T]]
}
