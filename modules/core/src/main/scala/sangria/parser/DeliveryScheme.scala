package sangria.parser

import scala.util.{Failure, Success, Try}

/** A mechanism for returning a success or failure result.
  *
  * @tparam T
  *   type of the successful result
  */
trait DeliveryScheme[T] {

  /** Type that encapsulates a successful output of type [[T]] or a failure of type [[Throwable]]. */
  type Result

  /** Return a result that encapsulates the given success output. */
  def success(result: T): Result

  /** Return a result that encapsulates the given failure. */
  def failure(error: Throwable): Result
}

object DeliveryScheme extends AlternativeDeliverySchemes {
  implicit def Try[T]: DeliveryScheme[T] { type Result = Try[T] } =
    new DeliveryScheme[T] {
      type Result = Try[T]

      def success(result: T): Try[T] = Success(result)
      def failure(error: Throwable): Try[T] = Failure(error)
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

      def success(result: T): T = result
      def failure(error: Throwable) = throw error
    }
}
