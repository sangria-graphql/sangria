package sangria.ast

/** A location within a GraphQL source code string.
  *
  * @param sourceId The ID of the source code.
  * @param index The offset of the location as characters from the start of the source code.
  * @param line The line number of the location within the source code.
  * @param column The column number of the location within the source code.
  */
case class AstLocation(sourceId: String, index: Int, line: Int, column: Int)

object AstLocation {
  def apply(index: Int, line: Int, column: Int): AstLocation =
    AstLocation("", index, line, column)
}
