package sangria.renderer

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.{Directive, Field, AstNode}
import sangria.parser.QueryParser
import sangria.util.{StringMatchers, FileUtil}
import sangria.macros._

import scala.util.Success

class QueryRendererSpec extends WordSpec with Matchers with StringMatchers {
  "QueryRenderer" should {

    // IGNORED: will be fixed with comments support in rendering
    "render kitchen sink" ignore {
      val Success(ast) = QueryParser.parse(FileUtil loadQuery "kitchen-sink.graphql")

      val prettyRendered = QueryRenderer.render(ast, QueryRenderer.Pretty)
      val compactRendered = QueryRenderer.render(ast, QueryRenderer.Compact)

      val Success(prettyParsed) = QueryParser.parse(prettyRendered)
      val Success(compactParsed) = QueryParser.parse(compactRendered)

      AstNode.withoutPosition(ast) should be (AstNode.withoutPosition(prettyParsed))
      AstNode.withoutPosition(ast) should be (AstNode.withoutPosition(compactParsed))

      compactRendered should be (
        "query queryName($foo:ComplexType,$site:Site=MOBILE){whoever123is:node(id:[123,456]){" +
            "id ... on User@defer{field2{id alias:field1(first:10,after:$foo)@include(if:$foo){id ...frag}}}}}" +
            "mutation likeStory{like(story:123)@defer{story{id}}}" +
            "subscription StoryLikeSubscription($input:StoryLikeSubscribeInput){storyLikeSubscribe(input:$input){story{likers{count} likeSentence{text}}}}" +
            "fragment frag on Friend{foo(size:$size,bar:$b,obj:" +
            "{key:\"value\"})}{unnamed(truthy:true,falsey:false) query ... @skip(unless:$foo){id} ... {id}}")

      prettyRendered should equal (
        """query queryName($foo: ComplexType, $site: Site = MOBILE) {
         |  whoever123is: node(id: [123, 456]) {
         |    id
         |    ... on User @defer {
         |      field2 {
         |        id
         |        alias: field1(first: 10, after: $foo) @include(if: $foo) {
         |          id
         |          ...frag
         |        }
         |      }
         |    }
         |  }
         |}
         |
         |mutation likeStory {
         |  like(story: 123) @defer {
         |    story {
         |      id
         |    }
         |  }
         |}
         |
         |subscription StoryLikeSubscription($input: StoryLikeSubscribeInput) {
         |  storyLikeSubscribe(input: $input) {
         |    story {
         |      likers {
         |        count
         |      }
         |      likeSentence {
         |        text
         |      }
         |    }
         |  }
         |}
         |
         |fragment frag on Friend {
         |  foo(size: $size, bar: $b, obj: {key: "value"})
         |}
         |
         |{
         |  unnamed(truthy: true, falsey: false)
         |  query
         |  ...  @skip(unless: $foo) {
         |    id
         |  }
         |  ...  {
         |    id
         |  }
         |}""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "render partial AST" in {
      val ast = Field(Some("al"), "field1", Nil, List(Directive("foo", Nil)), Nil)

      QueryRenderer.render(ast) should be ("al: field1 @foo")
    }

    "correctly prints query operations without name" in {
      val ast = graphql"query { id, name }"

      QueryRenderer.render(ast) should equal (
        """{
          |  id
          |  name
          |}""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "correctly prints query operations with artifacts and without name" in {
      val ast = graphql"query ($$foo: TestType) @testDirective { id, name }"

      QueryRenderer.render(ast) should equal (
        """query ($foo: TestType) @testDirective {
          |  id
          |  name
          |}""".stripMargin) (after being strippedOfCarriageReturns)
    }

    "correctly prints mutation operations with artifacts and without name" in {
      val ast = graphql"mutation ($$foo: TestType) @testDirective { id, name }"

      QueryRenderer.render(ast) should equal (
        """mutation ($foo: TestType) @testDirective {
          |  id
          |  name
          |}""".stripMargin) (after being strippedOfCarriageReturns)
    }
  }
}