package sangria

import org.parboiled2.ParserInput
import sangria.parser.{SyntaxError, QueryParser}
import sangria.renderer.Renderer

import scala.util.{Success, Failure}

object ExampleParsing extends App {

  val query =
    """
      query FetchLukeAndLeiaAliased($someVar: Int = 1.23,$anotherVar: Int = 123)@include(if: true) @include(if: false){
        luke: human(id: "1000")@include(if: true){
          friends(sort: NAME)
        }
        leia: human(id: "10103\n \u00F6 ö") {
          name
        }

        ... on User {
          birth{day}
        }

        ...Foo
      }

      fragment Foo on User @foo(bar: 1){
        baz
      }
    """    
  
  val res = QueryParser.parse(ParserInput(query))

  res match {
    case Success(ast) =>
      println("SUCCESS")
      println(ast)

      println(Renderer.render(ast))
      println(Renderer.render(ast, Renderer.Compact))
    case Failure(error: SyntaxError) =>
      println(error.message)
    case Failure(error) =>
      error.printStackTrace()
  }
}