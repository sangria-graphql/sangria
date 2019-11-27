package sangria.parser

import scala.util.{Failure, Success, Try}


trait DeliveryScheme[T] {
  type Result

  def success(result: T): Result
  def failure(error: Throwable): Result
}

object DeliveryScheme extends AlternativeDeliverySchemes {
  implicit def Try[T] =
    new DeliveryScheme[T] {
      type Result = Try[T]

      def success(result: T) = Success(result)
      def failure(error: Throwable) = Failure(error)
    }
}

trait AlternativeDeliverySchemes {
  implicit def Either[T] =
    new DeliveryScheme[T] {
      type Result = Either[Throwable, T]

      def success(result: T) = Right(result)
      def failure(error: Throwable) = Left(error)
    }

  implicit def Throw[T] =
    new DeliveryScheme[T] {
      type Result = T

      def success(result: T) = result
      def failure(error: Throwable) = throw error
    }
}
