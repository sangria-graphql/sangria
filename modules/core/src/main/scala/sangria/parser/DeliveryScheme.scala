package sangria.parser

import sangria.since2_1_7

import scala.util.{Failure, Success, Try}

@deprecated("Removed in 3.0. Expect a Try result instead.", since2_1_7)
trait DeliveryScheme[T] {
  type Result

  def success(result: T): Result
  def failure(error: Throwable): Result
}

object DeliveryScheme extends AlternativeDeliverySchemes {
  implicit def Try[T]: DeliveryScheme[T] { type Result = Try[T] } =
    new DeliveryScheme[T] {
      type Result = Try[T]

      def success(result: T) = Success(result)
      def failure(error: Throwable) = Failure(error)
    }
}

@deprecated("Removed in 3.0. See the member deprecations for advice.", since2_1_7)
trait AlternativeDeliverySchemes {
  @deprecated(
    "Removed in 3.0. Use the default Try scheme instead and convert with Try.toEither.",
    since2_1_7
  )
  implicit def Either[T]: DeliveryScheme[T] { type Result = Either[Throwable, T] } =
    new DeliveryScheme[T] {
      type Result = Either[Throwable, T]

      def success(result: T) = Right(result)
      def failure(error: Throwable) = Left(error)
    }

  @deprecated(
    "Removed in 3.0. Use the default Try scheme instead and convert with Try.get.",
    since2_1_7
  )
  implicit def Throw[T]: DeliveryScheme[T] { type Result = T } =
    new DeliveryScheme[T] {
      type Result = T

      def success(result: T) = result
      def failure(error: Throwable) = throw error
    }
}
