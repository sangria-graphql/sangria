package sangria.integration

import language.higherKinds

import sangria.util.tag
import sangria.util.tag._

import scala.annotation.implicitNotFound

@implicitNotFound("Type ${Val} cannot be used as a default value. Please consider defining an implicit instance of `ToInput` for it.")
trait ToInput[Val, Raw] {
  def toInput(value: Val): (Raw, InputUnmarshaller[Raw])
}

object ToInput {
  class ScalarToInput[T] extends ToInput[T, T @@ ScalaInput] {
    def toInput(value: T) = (tag[ScalaInput](value), InputUnmarshaller.scalaInputUnmarshaller)
  }

  implicit def normalScalaInput[T] = new ToInput[T @@ ScalaInput, T @@ ScalaInput] {
    def toInput(value: T @@ ScalaInput) = (value, InputUnmarshaller.scalaInputUnmarshaller)
  }

  implicit val intInput = new ScalarToInput[Int]
  implicit val bigDecimalInput = new ScalarToInput[BigDecimal]
  implicit val bigIntInput = new ScalarToInput[BigInt]
  implicit val longInput = new ScalarToInput[Long]
  implicit val floatInput = new ScalarToInput[Double]
  implicit val booleanInput = new ScalarToInput[Boolean]
  implicit val stringInput = new ScalarToInput[String]

  implicit def listInput[T, C[_] <: Seq[_]]: ToInput[C[T], C[T] @@ ScalaInput] =
    new ToInput[C[T], C[T] @@ ScalaInput] {
      def toInput(value: C[T]) = (tag[ScalaInput](value), InputUnmarshaller.scalaInputUnmarshaller)
    }

  implicit def mapInput[T]: ToInput[Map[String, T], Map[String, T] @@ ScalaInput] =
    new ToInput[Map[String, T], Map[String, T] @@ ScalaInput] {
      def toInput(value: Map[String, T]) = (tag[ScalaInput](value), InputUnmarshaller.scalaInputUnmarshaller)
    }
}