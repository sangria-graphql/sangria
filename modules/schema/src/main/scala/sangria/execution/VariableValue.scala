package sangria.execution

import sangria.marshalling.ResultMarshaller
import sangria.schema.InputType
import sangria.util.Cache
import sangria.validation.Violation

case class VariableValue(
  fn: (
    ResultMarshaller,
      ResultMarshaller,
      InputType[_]) => Either[Vector[Violation], Trinary[ResultMarshaller#Node]]) {
  private val cache =
    Cache.empty[(Int, Int), Either[Vector[Violation], Trinary[ResultMarshaller#Node]]]

  def resolve(
    marshaller: ResultMarshaller,
    firstKindMarshaller: ResultMarshaller,
    actualType: InputType[_]): Either[Vector[Violation], Trinary[firstKindMarshaller.Node]] =
    cache
      .getOrElseUpdate(
        System.identityHashCode(firstKindMarshaller) -> System.identityHashCode(
          actualType.namedType),
        fn(marshaller, firstKindMarshaller, actualType))
      .asInstanceOf[Either[Vector[Violation], Trinary[firstKindMarshaller.Node]]]
}
