package sangria.marshalling

import sangria.util.tag._

import scala.annotation.implicitNotFound
import scala.language.higherKinds

@implicitNotFound(
  "Type ${Val} cannot be used as a default value. Please consider defining an implicit instance of `ToInput` for it.")
trait ToInput[Val, Raw] {
  def toInput(value: Val): (Raw, InputUnmarshaller[Raw])
}

object ToInput {
  class ScalarToInput[T] extends ToInput[T, T @@ ScalaInput] {
    def toInput(value: T): (T @@ ScalaInput, InputUnmarshaller[T @@ ScalaInput]) =
      (ScalaInput.scalaInput(value), InputUnmarshaller.scalaInputUnmarshaller)
  }

  implicit def normalScalaInput[T]: ToInput[T @@ ScalaInput, T @@ ScalaInput] =
    new ToInput[T @@ ScalaInput, T @@ ScalaInput] {
      def toInput(value: T @@ ScalaInput): (T @@ ScalaInput, InputUnmarshaller[T @@ ScalaInput]) =
        (value, InputUnmarshaller.scalaInputUnmarshaller)
    }

  implicit val intInput: ScalarToInput[Int] = new ScalarToInput[Int]
  implicit val bigDecimalInput: ScalarToInput[BigDecimal] = new ScalarToInput[BigDecimal]
  implicit val bigIntInput: ScalarToInput[BigInt] = new ScalarToInput[BigInt]
  implicit val longInput: ScalarToInput[Long] = new ScalarToInput[Long]
  implicit val floatInput: ScalarToInput[Double] = new ScalarToInput[Double]
  implicit val booleanInput: ScalarToInput[Boolean] = new ScalarToInput[Boolean]
  implicit val stringInput: ScalarToInput[String] = new ScalarToInput[String]
}
