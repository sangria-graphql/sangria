package sangria.renderer

import sangria.ast._
import sangria.parser.{ParserConfig, QueryParser}
import sangria.util.{FileUtil, StringMatchers}
import sangria.ast
import sangria.macros._
import sangria.visitor.VisitorCommand

import scala.util.Success
import org.scalatest.matchers.should.Matchers
import org.scalatest.wordspec.AnyWordSpec
import sangria.schema.AstNodeTransformer

class QueryRendererSpec extends AnyWordSpec with Matchers with StringMatchers {
  val quotes = "\"\"\""

  "QueryRenderer" when {
    "rendering queries" should {
      "render kitchen sink" in {
        val Success(ast) = QueryParser.parse(FileUtil.loadQuery("kitchen-sink.graphql"))

        val prettyRendered = QueryRenderer.render(ast, QueryRenderer.Pretty)
        val compactRendered = QueryRenderer.render(ast, QueryRenderer.Compact)

        val Success(prettyParsed) = QueryParser.parse(prettyRendered)
        val Success(compactParsed) = QueryParser.parse(compactRendered)

        AstNodeTransformer.withoutAstLocations(ast) should be(
          AstNodeTransformer.withoutAstLocations(prettyParsed))
        AstNodeTransformer.withoutAstLocations(ast, stripComments = true) should be(
          AstNodeTransformer.withoutAstLocations(compactParsed, stripComments = true))

        compactRendered should be(
          "query queryName($foo:ComplexType,$site:Site=MOBILE){whoever123is:node(id:[123,456]){" +
            "id ... on User@defer{field2{id alias:field1(first:10,after:$foo)@include(if:$foo){id" +
            " ...frag}}}}}\nmutation likeStory{like(story:123)@defer{story{id}}}\nsubscription St" +
            "oryLikeSubscription($input:StoryLikeSubscribeInput){storyLikeSubscribe(input:$input)" +
            "{story{likers{count} likeSentence{text}}}}\nfragment frag on Friend{foo(size:$size,b" +
            "ar:$b,obj:{key:\"value\"})}\n{unnamed(truthy:true,falsey:false) query ... @skip(unle" +
            "ss:$foo){id} ... {id}}")

        prettyRendered should equal("""# Copyright (c) 2015, Facebook, Inc.
           |# All rights reserved.
           |#
           |# This source code is licensed under the BSD-style license found in the
           |# LICENSE file in the root directory of this source tree. An additional grant
           |# of patent rights can be found in the PATENTS file in the same directory.
           |
           |query queryName($foo: ComplexType, $site: Site = MOBILE) {
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
           |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "render partial AST" in {
        val ast = Field(
          Some("al"),
          "field1",
          Vector.empty,
          Vector(Directive("foo", Vector.empty)),
          Vector.empty)

        QueryRenderer.render(ast) should be("al: field1 @foo")
      }

      "correctly prints query operations without name" in {
        val ast = graphql"query { id, name }"

        QueryRenderer.render(ast) should equal("""{
            |  id
            |  name
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "correctly prints query operations with artifacts and without name" in {
        val ast = graphql"query ($$foo: TestType) @testDirective { id, name }"

        QueryRenderer.render(ast) should equal("""query ($foo: TestType) @testDirective {
            |  id
            |  name
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "correctly prints mutation operations with artifacts and without name" in {
        val ast = graphql"mutation ($$foo: TestType) @testDirective { id, name }"

        QueryRenderer.render(ast) should equal("""mutation ($foo: TestType) @testDirective {
            |  id
            |  name
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "render trailing comments inline for normal selections" in {
        val ast =
          gql"""
           {
             a {
               # aaa


               field1 #bbb
               #sdasdsa
               ... Bar #dfsdfsdfsd

               field3 #ccc
             } #ddddd

             b {
               ...Foo#fdfsdfds
             }
           }
         """

        QueryRenderer.render(ast) should equal("""{
            |  a {
            |    # aaa
            |
            |    field1 # bbb
            |
            |    # sdasdsa
            |    ...Bar # dfsdfsdfsd
            |    field3 # ccc
            |  }
            |
            |  # ddddd
            |
            |  b {
            |    ...Foo # fdfsdfds
            |  }
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "render trailing comments inline for type and enum definitions" in {
        val ast =
          gql"""
           enum Size {
             #comment1
             XL #comment2
             XXL #comment3
           }

           interface Fruit {
             #comment1
             test1: String #comment2
             #another comment
             test2(a: String): Int #comment3
           }

           type Foo implements Fruit {
             #comment1
             test1: String #comment2
             test2(s: Size): Int #comment3
           }

           input Bar {
             #comment1
             test1: String #comment2
             test2: Int! = 1 #comment3
           }
         """

        QueryRenderer.render(ast) should equal("""enum Size {
            |  # comment1
            |  XL # comment2
            |  XXL # comment3
            |}
            |
            |interface Fruit {
            |  # comment1
            |  test1: String # comment2
            |
            |  # another comment
            |  test2(a: String): Int # comment3
            |}
            |
            |type Foo implements Fruit {
            |  # comment1
            |  test1: String # comment2
            |  test2(s: Size): Int # comment3
            |}
            |
            |input Bar {
            |  # comment1
            |  test1: String # comment2
            |  test2: Int! = 1 # comment3
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "preserve comments on definitions and fields" in {
        val ast =
          graphql"""
            # comment 1
            mutation ($$foo: TestType)
                # comment 2
                # comment 3
                @testDirective {
              # comment 4
              id,
              # comment 5
              name {
                bar
                # arg trailing
              }

              # comment 6
              # comment 7


              #comment 8
              #comment 9

            }

            # fooo
            fragment Foo on Comment {
              # c1
              # c2
              a, b
              # e1
            }


            #{
            #  a {
            #    b
            #  }
            #}

            # comment d1
              # comment d2


              #comment d3
            #comment d4


          """

        QueryRenderer.render(ast) should equal("""# comment 1
            |mutation ($foo: TestType) @testDirective {
            |  # comment 4
            |  id
            |
            |  # comment 5
            |  name {
            |    bar
            |
            |    # arg trailing
            |  }
            |
            |  # comment 6
            |  # comment 7
            |
            |  # comment 8
            |  # comment 9
            |}
            |
            |# fooo
            |fragment Foo on Comment {
            |  # c1
            |  # c2
            |  a
            |  b
            |
            |  # e1
            |}
            |
            |# {
            |#  a {
            |#    b
            |#  }
            |# }
            |
            |# comment d1
            |# comment d2
            |
            |# comment d3
            |# comment d4""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "preserve comments on fragment spreads and inline fragments" in {
        val ast =
          graphql"""
            # comment 1
            query MyQuery($$foo: TestType){
              # comment 4
              id,
              # comment1
              # comment2

              ... {
                c, d
              }
              # comment3
              # comment4
              ... on Cat {
                c, d
                # end1
              }
              # comment5
              # comment6
              ...Foo
              # end
            }

            # fooo
            fragment Foo on Comment {
              a, b
            }
          """

        QueryRenderer.render(ast) should equal("""# comment 1
            |query MyQuery($foo: TestType) {
            |  # comment 4
            |  id
            |
            |  # comment1
            |  # comment2
            |
            |  ...  {
            |    c
            |    d
            |  }
            |
            |  # comment3
            |  # comment4
            |  ... on Cat {
            |    c
            |    d
            |
            |    # end1
            |  }
            |
            |  # comment5
            |  # comment6
            |  ...Foo
            |
            |  # end
            |}
            |
            |# fooo
            |fragment Foo on Comment {
            |  a
            |  b
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "preserve comments on arguments and variables" in {
        val ast =
          graphql"""
            # comment 1
            query MyQuery(
            # foo
            #bar
            $$foo: TestType, $$second: Int = 1,
            # aaa
            # bbbb
            $$third: String = "hello") {

              id(first: 1,
                #hello
                #world
                second:$$foo, third: 1,
                # foo
                # bar



                # 111111
                # 12345


                last: {a: b, c: [1,2]}),
            }
          """

        QueryRenderer.render(ast) should equal("""# comment 1
            |query MyQuery(
            |    # foo
            |    # bar
            |    $foo: TestType, $second: Int = 1,
            |
            |    # aaa
            |    # bbbb
            |    $third: String = "hello") {
            |  id(first: 1,
            |
            |    # hello
            |    # world
            |    second: $foo, third: 1,
            |
            |    # foo
            |    # bar
            |
            |    # 111111
            |    # 12345
            |
            |    last: {a: b, c: [1, 2]})
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "correctly render input values and preserve comments on values and fields" in {
        val input =
          graphqlInput"""
            # root
            # comment!

            {
              # data field!
              data: {
                human: {
                  #just a name
                  name: "Luke Skywalker"
                  appearsIn: [
                    #first enum
                    NEWHOPE,
                    EMPIRE,
                    #last one
                    JEDI
                  ]
                  # foo bar

                  # baz

                  friends:
                  # comment here!
                  [{
                    id: "1002"
                    name: "Han Solo"
                  },

                  # value comment
                  {
                    id: "1003"
                    # name comment
                    name: "Leia Organa"
                  }, {
                    id: "2000"
                    name: "C-3PO"
                  }, {
                    id: "2001"
                    name: "R2-D2"
                  }]

                  obj:
                    # another value comment
                    {a: b, c: "d"}

                  one:
                    # value one

                    # yay
                    1
                }
              }
            }
          """

        QueryRenderer.render(input, QueryRenderer.PrettyInput) should equal("""# root
            |# comment!
            |
            |{
            |  # data field!
            |  data: {
            |    human: {
            |      # just a name
            |      name: "Luke Skywalker"
            |      appearsIn: [
            |        # first enum
            |        NEWHOPE, EMPIRE,
            |
            |        # last one
            |        JEDI]
            |
            |      # foo bar
            |
            |      # baz
            |
            |      friends:
            |        # comment here!
            |        [{
            |          id: "1002"
            |          name: "Han Solo"
            |        },
            |
            |        # value comment
            |        {
            |          id: "1003"
            |
            |          # name comment
            |          name: "Leia Organa"
            |        }, {
            |          id: "2000"
            |          name: "C-3PO"
            |        }, {
            |          id: "2001"
            |          name: "R2-D2"
            |        }]
            |      obj:
            |        # another value comment
            |        {
            |          a: b
            |          c: "d"
            |        }
            |      one:
            |        # value one
            |
            |        # yay
            |        1
            |    }
            |  }
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))

      }

      "render queries with block strings (erase block meta-info)" in {
        val Success(origAst) = QueryParser.parse(FileUtil.loadQuery("block-string.graphql"))

        val ast = AstVisitor.visit(
          origAst,
          AstVisitor { case s: StringValue =>
            VisitorCommand.Transform(s.copy(block = false, blockRawValue = None))
          }
        )

        val prettyRendered = QueryRenderer.render(ast, QueryRenderer.Pretty)
        val compactRendered = QueryRenderer.render(ast, QueryRenderer.Compact)

        val Success(prettyParsed) = QueryParser.parse(prettyRendered)
        val Success(compactParsed) = QueryParser.parse(compactRendered)

        AstNodeTransformer.withoutAstLocations(ast) should be(
          AstNodeTransformer.withoutAstLocations(prettyParsed))
        AstNodeTransformer.withoutAstLocations(ast, stripComments = true) should be(
          AstNodeTransformer.withoutAstLocations(compactParsed, stripComments = true))

        compactRendered should be(
          "query FetchLukeAndLeiaAliased($someVar:String=\"hello \\\\\\n  world\"" +
            "){luke:human(id:\"1000\",bar:\" \\\\\\\"test\\n123 \\\\u0000\") ...Foo" +
            "}\nfragment Foo on User@foo(bar:1){baz@docs(info:\"\\\"\\\"\\\"\\n\\\"" +
            "\\\"\\\" this \\\" is \\\"\\\"\\na description! \\\"\\\"\\\"\")}")

        prettyRendered should equal(
          """query FetchLukeAndLeiaAliased($someVar: String = "hello \\\n  world") {
            |  luke: human(id: "1000", bar: " \\\"test\n123 \\u0000")
            |  ...Foo
            |}
            |
            |fragment Foo on User @foo(bar: 1) {
            |  baz @docs(info: "\"\"\"\n\"\"\" this \" is \"\"\na description! \"\"\"")
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "render queries with block strings " in {
        val Success(ast) = QueryParser.parse(FileUtil.loadQuery("block-string.graphql"))

        def withoutRaw(withRaw: Document, block: Boolean = true) = AstVisitor.visit(
          withRaw,
          AstVisitor { case s: StringValue =>
            VisitorCommand.Transform(
              s.copy(block = if (block) s.block else false, blockRawValue = None))
          })

        val prettyRendered = QueryRenderer.render(ast, QueryRenderer.Pretty)
        val compactRendered = QueryRenderer.render(ast, QueryRenderer.Compact)

        val Success(prettyParsed) = QueryParser.parse(prettyRendered)
        val Success(compactParsed) = QueryParser.parse(compactRendered)

        AstNodeTransformer.withoutAstLocations(withoutRaw(ast)) should be(
          AstNodeTransformer.withoutAstLocations(withoutRaw(prettyParsed)))
        AstNodeTransformer.withoutAstLocations(
          withoutRaw(ast, block = false),
          stripComments = true) should be(
          AstNodeTransformer
            .withoutAstLocations(withoutRaw(compactParsed, block = false), stripComments = true))

        compactRendered should be(
          "query FetchLukeAndLeiaAliased($someVar:String=\"hello \\\\\\n  world\"" +
            "){luke:human(id:\"1000\",bar:\" \\\\\\\"test\\n123 \\\\u0000\") ...Foo" +
            "}\nfragment Foo on User@foo(bar:1){baz@docs(info:\"\\\"\\\"\\\"\\n\\\"" +
            "\\\"\\\" this \\\" is \\\"\\\"\\na description! \\\"\\\"\\\"\")}")

        prettyRendered should equal(FileUtil.loadQuery("block-string-rendered.graphql"))(
          after.being(strippedOfCarriageReturns))
      }

      "Experimental: correctly prints fragment defined variables" in {
        val query =
          """
            fragment Foo($a: ComplexType, $b: Boolean = false) on TestType {
              id
            }
          """

        QueryParser.parse(query).isFailure should be(true)

        val Success(ast) =
          QueryParser.parse(query, ParserConfig.default.withExperimentalFragmentVariables)

        QueryRenderer.renderPretty(ast) should equal(
          """fragment Foo($a: ComplexType, $b: Boolean = false) on TestType {
            |  id
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }

      "correctly render variable definition directives" in {
        val query =
          """
            query ($foo: TestType = {a: 123} @testDirective(if: true) @test) { id }
          """

        val Success(ast) = QueryParser.parse(query)

        QueryRenderer.renderPretty(ast) should equal(
          """query ($foo: TestType = {a: 123} @testDirective(if: true) @test) {
            |  id
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }
    }

    "rendering schema definition" should {
      "renders minimal ast" in {
        QueryRenderer.render(ast.ScalarTypeDefinition("DateTime")) should be("scalar DateTime")
      }

      "renders directive definitions" in {
        val directive = ast.DirectiveDefinition(
          "custom",
          arguments = Vector.empty,
          locations = Vector(
            ast.DirectiveLocation("FIELD"),
            ast.DirectiveLocation("FRAGMENT_SPREAD"),
            ast.DirectiveLocation("INLINE_FRAGMENT"))
        )
        val prettyRendered = QueryRenderer.render(directive, QueryRenderer.Pretty)
        val compactRendered = QueryRenderer.render(directive, QueryRenderer.Compact)

        compactRendered should equal(
          """directive@custom on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT""".stripMargin
        )(after.being(strippedOfCarriageReturns))
        prettyRendered should equal(
          """directive @custom on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT""".stripMargin
        )(after.being(strippedOfCarriageReturns))
      }

      "render kitchen sink" in {
        val Success(ast) = QueryParser.parse(FileUtil.loadQuery("schema-kitchen-sink.graphql"))

        def noBlock(a: AstNode) = AstVisitor.visit(
          a,
          AstVisitor { case v: StringValue =>
            VisitorCommand.Transform(v.copy(block = false, blockRawValue = None))
          })

        val prettyRendered = QueryRenderer.render(ast, QueryRenderer.Pretty)
        val compactRendered = QueryRenderer.render(ast, QueryRenderer.Compact)

        val Success(prettyParsed) = QueryParser.parse(prettyRendered)
        val Success(compactParsed) = QueryParser.parse(compactRendered)

        AstNodeTransformer.withoutAstLocations(ast, stripComments = true) should be(
          AstNodeTransformer.withoutAstLocations(prettyParsed, stripComments = true))

        AstNodeTransformer.withoutAstLocations(noBlock(ast), stripComments = true) should be(
          AstNodeTransformer.withoutAstLocations(noBlock(compactParsed), stripComments = true))

        compactRendered should equal("""schema{query:QueryType mutation:MutationType}
            |"type description!" type Foo implements Bar{one:Type two(argument:InputType!):Type three(argument:InputType,other:String):Int four(argument:String="string"):String five(argument:[String]=["string","string"]):String "More \"\"\" descriptions \\" six(argument:InputType={key:"value"}):Type}
            |type AnnotatedObject @onObject(arg:"value"){annotatedField(arg:Type="default"@onArg):Type@onField}
            |" It's an interface!" interface Bar{one:Type four(argument:String="string"):String}
            |interface AnnotatedInterface@onInterface{annotatedField(arg:Type@onArg):Type@onField}
            |union Feed=Story|Article|Advert
            |union AnnotatedUnion@onUnion=A|B
            |scalar CustomScalar
            |scalar AnnotatedScalar@onScalar
            |enum Site{"description 1" DESKTOP "description 2" MOBILE}
            |enum AnnotatedEnum@onEnum{ANNOTATED_VALUE@onEnumValue OTHER_VALUE}
            |input InputType{key:String! answer:Int=42}
            |input AnnotatedInput@onInputObjectType{annotatedField:Type@onField}
            |extend type Foo {seven(argument:[String]):Type}
            |extend type Foo @onType
            |"cool skip" directive@skip(if:Boolean!)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT
            |directive@include(if:Boolean!)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT""".stripMargin)(
          after.being(strippedOfCarriageReturns))

        prettyRendered should equal(FileUtil.loadQuery("schema-kitchen-sink-pretty.graphql"))
      }

      "renders schema with comments correctly" in {
        val ast =
          graphql"""
            # fdwfsdfsdfsd
            # sfddsafsdfs
            type Foo implements Bar@dfdsfsdf(aaa: 1)@qqq(aaa:[1,2]) {
              one: Type
              # aaaaaaaaa
              two(
                # fskjlghfdkjlhgd
                argument: InputType!): Type
              three(
                #aaaaa
                # vvvvvv

                # dffff
                argument: InputType @aaa(c: b),

                # My
                # comment!
                other: String @ddd(aa:1)@xxx(ttt:"sdfdsf")): Int
              four(argument: String = "string"): String
              five(argument: [String] = ["string", "string"]): String @aaaa(if: true)
              six(argument: InputType = {key: "value"}): Type
              another(argument: InputType = {key: "value"} mylist: [String] = ["string", "string"]): Type

              #te
            }

            type AnnotatedObject @onObject(arg: "value") {
              # comment
              annotatedField(arg: Type = "default" @onArg): Type @onField
            }

            # It's an interface!
            interface Bar {
              one: Type
              four(argument: String = "string"): String
              #ie
            }

            union Feed = Story | Article | Advert

            union AnnotatedUnion @onUnion = A | B

            scalar CustomScalar

            scalar AnnotatedScalar @onScalar

            # enum comment
            enum Site {
              # value 1
              DESKTOP
              # value 2
              MOBILE
              # ee
            }

            # enum comment 1
            # enum comment 2
            enum AnnotatedEnum @onEnum {
              ANNOTATED_VALUE @onEnumValue
              OTHER_VALUE
            }

            input InputType {
              key: String!
              answer: Int = 42
              #ite
            }

            # it's input
            input AnnotatedInput @onInputObjectType {
              # field comment
              annotatedField: Type @onField

              key: String!



              # ljkdhfkj
              answer: Int = 42 @foo(bar: baz)
            }

            # qwerty
            extend type Foo {
              seven(
                # hello
                argument: [String]): Type
            }

            # yay!
            directive @skip(if: Boolean!) on
              # multi
              # line
              FIELD |
              # my comment!
              FRAGMENT_SPREAD | INLINE_FRAGMENT

            # schema comment!
            schema @myDir(a: b)@another(a: b, c: 1234.45){
              query: QueryType

              # another comment
              # line 2

              mutation: MutationType
              #se
            }
          """

        val prettyRendered = QueryRenderer.render(ast, QueryRenderer.Pretty)
        val compactRendered = QueryRenderer.render(ast, QueryRenderer.Compact)

        val Success(prettyParsed) = QueryParser.parse(prettyRendered)
        val Success(compactParsed) = QueryParser.parse(compactRendered)

        AstNodeTransformer.withoutAstLocations(ast, stripComments = true) should be(
          AstNodeTransformer.withoutAstLocations(prettyParsed, stripComments = true))

        AstNodeTransformer.withoutAstLocations(ast, stripComments = true) should be(
          AstNodeTransformer.withoutAstLocations(compactParsed, stripComments = true))

        compactRendered should equal(
          """type Foo implements Bar@dfdsfsdf(aaa:1)@qqq(aaa:[1,2]){one:Type two(argument:InputType!):Type three(argument:InputType@aaa(c:b),other:String@ddd(aa:1)@xxx(ttt:"sdfdsf")):Int four(argument:String="string"):String five(argument:[String]=["string","string"]):String@aaaa(if:true) six(argument:InputType={key:"value"}):Type another(argument:InputType={key:"value"},mylist:[String]=["string","string"]):Type}
            |type AnnotatedObject @onObject(arg:"value"){annotatedField(arg:Type="default"@onArg):Type@onField}
            |interface Bar{one:Type four(argument:String="string"):String}
            |union Feed=Story|Article|Advert
            |union AnnotatedUnion@onUnion=A|B
            |scalar CustomScalar
            |scalar AnnotatedScalar@onScalar
            |enum Site{DESKTOP MOBILE}
            |enum AnnotatedEnum@onEnum{ANNOTATED_VALUE@onEnumValue OTHER_VALUE}
            |input InputType{key:String! answer:Int=42}
            |input AnnotatedInput@onInputObjectType{annotatedField:Type@onField key:String! answer:Int=42@foo(bar:baz)}
            |extend type Foo {seven(argument:[String]):Type}
            |directive@skip(if:Boolean!)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT
            |schema@myDir(a:b)@another(a:b,c:1234.45){query:QueryType mutation:MutationType}""".stripMargin)(
          after.being(strippedOfCarriageReturns))

        prettyRendered should equal("""# fdwfsdfsdfsd
            |# sfddsafsdfs
            |type Foo implements Bar @dfdsfsdf(aaa: 1) @qqq(aaa: [1, 2]) {
            |  one: Type
            |
            |  # aaaaaaaaa
            |  two(
            |    # fskjlghfdkjlhgd
            |    argument: InputType!): Type
            |  three(
            |    # aaaaa
            |    # vvvvvv
            |
            |    # dffff
            |    argument: InputType @aaa(c: b),
            |
            |    # My
            |    # comment!
            |    other: String @ddd(aa: 1) @xxx(ttt: "sdfdsf")): Int
            |  four(argument: String = "string"): String
            |  five(argument: [String] = ["string", "string"]): String @aaaa(if: true)
            |  six(argument: InputType = {key: "value"}): Type
            |  another(argument: InputType = {key: "value"}, mylist: [String] = ["string", "string"]): Type
            |
            |  # te
            |}
            |
            |type AnnotatedObject @onObject(arg: "value") {
            |  # comment
            |  annotatedField(arg: Type = "default" @onArg): Type @onField
            |}
            |
            |# It's an interface!
            |interface Bar {
            |  one: Type
            |  four(argument: String = "string"): String
            |
            |  # ie
            |}
            |
            |union Feed = Story | Article | Advert
            |
            |union AnnotatedUnion @onUnion = A | B
            |
            |scalar CustomScalar
            |
            |scalar AnnotatedScalar @onScalar
            |
            |# enum comment
            |enum Site {
            |  # value 1
            |  DESKTOP
            |
            |  # value 2
            |  MOBILE
            |
            |  # ee
            |}
            |
            |# enum comment 1
            |# enum comment 2
            |enum AnnotatedEnum @onEnum {
            |  ANNOTATED_VALUE @onEnumValue
            |  OTHER_VALUE
            |}
            |
            |input InputType {
            |  key: String!
            |  answer: Int = 42
            |
            |  # ite
            |}
            |
            |# it's input
            |input AnnotatedInput @onInputObjectType {
            |  # field comment
            |  annotatedField: Type @onField
            |  key: String!
            |
            |  # ljkdhfkj
            |  answer: Int = 42 @foo(bar: baz)
            |}
            |
            |# qwerty
            |extend type Foo {
            |  seven(
            |    # hello
            |    argument: [String]): Type
            |}
            |
            |# yay!
            |directive @skip(if: Boolean!) on
            |  # multi
            |  # line
            |  FIELD |
            |
            |  # my comment!
            |  FRAGMENT_SPREAD | INLINE_FRAGMENT
            |
            |# schema comment!
            |schema @myDir(a: b) @another(a: b, c: 1234.45) {
            |  query: QueryType
            |
            |  # another comment
            |  # line 2
            |
            |  mutation: MutationType
            |
            |  # se
            |}""".stripMargin)(after.being(strippedOfCarriageReturns))
      }
    }
  }
}
