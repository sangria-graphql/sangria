package sangria.marshalling

import sangria.util.tag._

import scala.annotation.implicitNotFound
import scala.language.higherKinds

@implicitNotFound("Type ${Val} cannot be used as a default value. Please consider defining an implicit instance of `ToInput` for it.")
trait ToInput[Val, Raw] {
  def toInput(value: Val): (Raw, InputUnmarshaller[Raw])
}

object ToInput {
  class ScalarToInput[T] extends ToInput[T, T @@ ScalaInput] {
    def toInput(value: T) = (ScalaInput.scalaInput(value), InputUnmarshaller.scalaInputUnmarshaller)
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
}