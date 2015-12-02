package sangria.marshalling

import sangria.schema.InputObjectType

import scala.annotation.implicitNotFound
import scala.language.higherKinds

@implicitNotFound("Type ${Val} cannot be used as an input. Please consider defining an implicit instance of `FromInput` for it.")
trait FromInput[Val] {
  val marshaller: ResultMarshaller
  def fromResult(node: marshaller.Node): Val
}

object FromInput {
  private object ScalarFromInput extends FromInput[Any] {
    val marshaller = CoercedScalaResultMarshaller.default
    def fromResult(node: marshaller.Node) = node
  }

  class SeqFromInput[T](delegate: FromInput[T]) extends FromInput[Seq[T]] {
    val marshaller = delegate.marshaller

    def fromResult(node: marshaller.Node) =
      node.asInstanceOf[Seq[Any]].map {
        case optElem: Option[_] ⇒
          optElem map (elem ⇒ delegate.fromResult(elem.asInstanceOf[delegate.marshaller.Node]))
        case elem ⇒
          delegate.fromResult(elem.asInstanceOf[delegate.marshaller.Node])
      }.asInstanceOf[Seq[T]]
  }

  import sangria.util.tag._

  implicit def coercedScalaInput[T] = ScalarFromInput.asInstanceOf[FromInput[T @@ CoercedScalaResult]]
  implicit def fooInput[T] = ScalarFromInput.asInstanceOf[FromInput[InputObjectType.DefaultInput]]
  implicit def foo1Input[T](implicit ev: FromInput[T]) = ev.asInstanceOf[FromInput[T @@ InputObjectResult]]

  implicit def optionInput[T](implicit ev: FromInput[T]) = ev.asInstanceOf[FromInput[Option[T]]]
  implicit def seqInput[T](implicit ev: FromInput[T]) = new SeqFromInput[T](ev)
  
  trait CoercedScalaResult
  trait InputObjectResult
}