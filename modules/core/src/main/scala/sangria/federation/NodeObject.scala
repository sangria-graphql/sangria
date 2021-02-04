package sangria.federation

trait NodeObject {

  def __typename: String
  def decode[T]: Either[Exception, T]
}