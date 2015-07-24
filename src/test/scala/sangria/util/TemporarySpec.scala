package sangria.util

import org.scalatest.{Matchers, WordSpec}
import sangria.TestData.{FriendsResolver, CharacterRepo}
import sangria.execution.Executor
import sangria.parser.{SyntaxError, QueryParser}
import sangria.renderer.{QueryRenderer, SchemaRenderer}
import sangria.schema.TestSchema
import sangria.introspection.introspectionQuery

import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.{Failure, Success}

import scala.concurrent.ExecutionContext.Implicits.global

class TemporarySpec extends WordSpec with Matchers {

  "Just playing around" should {
    "parsing" in {
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

      val res = QueryParser.parse(query)

      res match {
        case Success(ast) =>
          println("SUCCESS")
          println(ast)

          println(QueryRenderer.render(ast))
          println(QueryRenderer.render(ast, QueryRenderer.Compact))
        case Failure(error: SyntaxError) =>
          println(error.message)
        case Failure(error) =>
          error.printStackTrace()
      }
    }

    "values and defers" in {
      val ast = QueryParser.parse(
        s"""
           query testQ ($$xx: Boolean!) {
             #${(1 to 1000) map ("field" + _) mkString " "}

             ... on Query {
               getDroid
             }

             ...Foo
           }

           fragment Foo on Query {
             human(id: "1003") @include(if: $$xx) {id, __typename, name, friends{name}}
             test {name}
             project {name id primaryFunction}
           }
        """
      ) match {
        case Success(v) => v
        case Failure(error: SyntaxError) =>
          println(error.formattedError)
          throw error
        case Failure(e) => throw e
      }

      val vars = Map("xx" -> true)

      {
        import sangria.integration.Json4sSupport._
        import org.json4s.native.JsonMethods._

        println("Json4s marshalling:\n")
        println(pretty(render(Await.result(
          Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
              .execute(ast, arguments = Some(vars)), Duration.Inf))))
      }

      {
        import sangria.integration.SprayJsonSupport._

        println("\nSprayJson marshalling:\n")
        println(Await.result(
          Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
              .execute(ast, arguments = Some(vars)), Duration.Inf).prettyPrint)
      }

      {
        import sangria.integration.PlayJsonSupport._
        import play.api.libs.json._

        println("\nPlayJson marshalling:\n")
        println(Json.prettyPrint(Await.result(
          Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
              .execute(ast, arguments = Some(vars)), Duration.Inf)))
      }

      {
        import sangria.integration.SprayJsonSupport._

        val res = Await.result(
          Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
              .execute(introspectionQuery, arguments = Some(vars)), Duration.Inf)

        println("\nIntrospection:\n")
        println(res.prettyPrint)

        println("\nIntrospection rendered:\n")
        println(SchemaRenderer.renderSchema(res).get)
      }
    }
  }

}
