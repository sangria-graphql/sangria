package sangria

import org.parboiled2.ParserInput
import sangria.parser.{SyntaxError, QueryParser}

import scala.util.{Success, Failure}

object ExampleParsing extends App {

  val res = QueryParser.parse(ParserInput(
    """
      query FetchLukeAndLeiaAliased {
        luke: human(id: "1000") {
          friends(sort: NAME)
        }
        leia: human(id: "10103") {
          name
        }
      }
    """))

  res match {
    case Success(ast) =>
      println("SUCCESS")
      println(ast)
    case Failure(error: SyntaxError) =>
      println(error.message)
    case Failure(error) =>
      error.printStackTrace()
  }
}