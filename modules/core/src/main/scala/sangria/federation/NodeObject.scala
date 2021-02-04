package sangria.federation

trait NodeObject[Node] {

  def __typename: String
  def decode[T](implicit ev: Decoder[Node, T]): Either[Exception, T]
}