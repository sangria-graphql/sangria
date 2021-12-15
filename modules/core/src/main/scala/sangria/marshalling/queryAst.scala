package sangria.marshalling

import sangria.ast
import sangria.parser.QueryParser

import scala.util.Try

object queryAst extends QueryAstCore {
  implicit object QueryAstInputParser extends InputParser[ast.Value] {
    def parse(str: String): Try[ast.Value] = QueryParser.parseInput(str)
  }
}
