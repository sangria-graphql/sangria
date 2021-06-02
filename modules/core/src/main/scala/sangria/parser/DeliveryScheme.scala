package sangria.parser

import com.twitter.util.{Throw => TryThrow, Return, Try}

trait DeliveryScheme[T] {
  type Result

  def success(result: T): Result
  def failure(error: Throwable): Result
}

object DeliveryScheme extends AlternativeDeliverySchemes {
  implicit def Try[T]: DeliveryScheme[T] { type Result = Try[T] } =
    new DeliveryScheme[T] {
      type Result = Try[T]

      def success(result: T) = Return(result)
      def failure(error: Throwable) = TryThrow(error)
    }
}

trait AlternativeDeliverySchemes {
  implicit def Either[T]: DeliveryScheme[T] { type Result = Either[Throwable, T] } =
    new DeliveryScheme[T] {
      type Result = Either[Throwable, T]

      def success(result: T) = Right(result)
      def failure(error: Throwable) = Left(error)
    }

  implicit def Throw[T]: DeliveryScheme[T] { type Result = T } =
    new DeliveryScheme[T] {
      type Result = T

      def success(result: T) = result
      def failure(error: Throwable) = throw error
    }
}
