package sangria.schema

import sangria.marshalling.{InputParser, ToInput}

case class DefaultValueParser[T](
    schema: Schema[_, _],
    parser: InputParser[T],
    toInput: ToInput[T, _])

object DefaultValueParser {
  def forType[T](schema: Schema[_, _])(implicit
      parser: InputParser[T],
      toInput: ToInput[T, _]): DefaultValueParser[T] =
    DefaultValueParser[T](schema, parser, toInput)
}
