package sangria.renderer

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.{Directive, Field, AstNode}
import sangria.parser.QueryParser
import sangria.schema.TestSchema
import sangria.util.FileUtil

import scala.util.Success

class RendererSpec extends WordSpec with Matchers {
  "Renderer" should {
    "render kitchen sink" in {
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
            "mutation likeStory{like(story:123)@defer{story{id}}}fragment frag on Friend{foo(size:$size,bar:$b,obj:" +
            "{key:\"value\"})}{unnamed(truthy:true,falsey:false) query}")

      prettyRendered should be (
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
          |fragment frag on Friend {
          |  foo(size: $size, bar: $b, obj: {key: "value"})
          |}
          |
          |{
          |  unnamed(truthy: true, falsey: false)
          |  query
          |}""".stripMargin)
    }

    "render partial AST" in {
      val ast = Field(Some("al"), "field1", Nil, List(Directive("foo", Nil)), Nil)

      QueryRenderer.render(ast) should be ("al: field1 @foo")
    }
  }
}