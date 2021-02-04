package sangria.federation

trait Decoder[Node, T] {

  def decode(node: Node): Either[Exception, T]
}
