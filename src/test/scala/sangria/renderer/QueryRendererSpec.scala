package sangria.renderer

import org.scalatest.{Matchers, WordSpec}
import sangria.ast.{Comment, Directive, Field, AstNode}
import sangria.parser.QueryParser
import sangria.util.{StringMatchers, FileUtil}
import sangria.ast
import sangria.macros._

import scala.util.Success

class QueryRendererSpec extends WordSpec with Matchers with StringMatchers {
  "QueryRenderer" when {
    "rendering queries" should {
      "render kitchen sink" in {
        val Success(ast) = QueryParser.parse(FileUtil loadQuery "kitchen-sink.graphql")

        val prettyRendered = QueryRenderer.render(ast, QueryRenderer.Pretty)
        val compactRendered = QueryRenderer.render(ast, QueryRenderer.Compact)

        val Success(prettyParsed) = QueryParser.parse(prettyRendered)
        val Success(compactParsed) = QueryParser.parse(compactRendered)

        AstNode.withoutPosition(ast) should be (AstNode.withoutPosition(prettyParsed))
        AstNode.withoutPosition(ast, stripComments = true) should be (
          AstNode.withoutPosition(compactParsed, stripComments = true))

        compactRendered should be (
          "query queryName($foo:ComplexType,$site:Site=MOBILE){whoever123is:node(id:[123,456]){" +
          "id ... on User@defer{field2{id alias:field1(first:10,after:$foo)@include(if:$foo){id" +
          " ...frag}}}}}\nmutation likeStory{like(story:123)@defer{story{id}}}\nsubscription St" +
          "oryLikeSubscription($input:StoryLikeSubscribeInput){storyLikeSubscribe(input:$input)" +
          "{story{likers{count} likeSentence{text}}}}\nfragment frag on Friend{foo(size:$size,b" +
          "ar:$b,obj:{key:\"value\"})}\n{unnamed(truthy:true,falsey:false) query ... @skip(unle" +
          "ss:$foo){id} ... {id}}")

        prettyRendered should equal (
          """# Copyright (c) 2015, Facebook, Inc.
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

        QueryRenderer.render(ast) should equal (
          """# comment 1
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
            |# comment d4""".stripMargin) (after being strippedOfCarriageReturns)
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

        QueryRenderer.render(ast) should equal (
          """# comment 1
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
            |}""".stripMargin) (after being strippedOfCarriageReturns)
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

        QueryRenderer.render(ast) should equal (
          """# comment 1
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
            |}""".stripMargin) (after being strippedOfCarriageReturns)
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

        QueryRenderer.render(input, QueryRenderer.PrettyInput) should equal (
          """# root
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
            |}""".stripMargin) (after being strippedOfCarriageReturns)

      }
    }

    "rendering schema definition" should {
      "renders minimal ast" in {
        QueryRenderer.render(ast.ScalarTypeDefinition("DateTime")) should be ("scalar DateTime")
      }

      "render kitchen sink" in {
        val Success(ast) = QueryParser.parse(FileUtil loadQuery "schema-kitchen-sink.graphql")

        val prettyRendered = QueryRenderer.render(ast, QueryRenderer.Pretty)
        val compactRendered = QueryRenderer.render(ast, QueryRenderer.Compact)

        val Success(prettyParsed) = QueryParser.parse(prettyRendered)
        val Success(compactParsed) = QueryParser.parse(compactRendered)

        AstNode.withoutPosition(ast, stripComments = true) should be (
          AstNode.withoutPosition(prettyParsed, stripComments = true))

        AstNode.withoutPosition(ast, stripComments = true) should be (
          AstNode.withoutPosition(compactParsed, stripComments = true))

        compactRendered should equal (
          """schema{query:QueryType mutation:MutationType}
            |type Foo implements Bar{one:Type two(argument:InputType!):Type three(argument:InputType,other:String):Int four(argument:String="string"):String five(argument:[String]=["string","string"]):String six(argument:InputType={key:"value"}):Type}
            |type AnnotatedObject @onObject(arg:"value"){annotatedField(arg:Type="default"@onArg):Type@onField}
            |interface Bar{one:Type four(argument:String="string"):String}
            |interface AnnotatedInterface@onInterface{annotatedField(arg:Type@onArg):Type@onField}
            |union Feed=Story|Article|Advert
            |union AnnotatedUnion@onUnion=A|B
            |scalar CustomScalar
            |scalar AnnotatedScalar@onScalar
            |enum Site{DESKTOP MOBILE}
            |enum AnnotatedEnum@onEnum{ANNOTATED_VALUE@onEnumValue OTHER_VALUE}
            |input InputType {key:String! answer:Int=42}
            |input AnnotatedInput @onInputObjectType{annotatedField:Type@onField}
            |extend type Foo {seven(argument:[String]):Type}
            |extend type Foo @onType{}
            |type NoFields {}
            |directive@skip(if:Boolean!)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT
            |directive@include(if:Boolean!)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT""".stripMargin) (after being strippedOfCarriageReturns)

        prettyRendered should equal (
          """# Copyright (c) 2015, Facebook, Inc.
            |# All rights reserved.
            |#
            |# This source code is licensed under the BSD-style license found in the
            |# LICENSE file in the root directory of this source tree. An additional grant
            |# of patent rights can be found in the PATENTS file in the same directory.
            |
            |schema {
            |  query: QueryType
            |  mutation: MutationType
            |}
            |
            |type Foo implements Bar {
            |  one: Type
            |  two(argument: InputType!): Type
            |  three(argument: InputType, other: String): Int
            |  four(argument: String = "string"): String
            |  five(argument: [String] = ["string", "string"]): String
            |  six(argument: InputType = {key: "value"}): Type
            |}
            |
            |type AnnotatedObject @onObject(arg: "value") {
            |  annotatedField(arg: Type = "default" @onArg): Type @onField
            |}
            |
            |# It's an interface!
            |interface Bar {
            |  one: Type
            |  four(argument: String = "string"): String
            |}
            |
            |interface AnnotatedInterface @onInterface {
            |  annotatedField(arg: Type @onArg): Type @onField
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
            |enum Site {
            |  # value 1
            |  DESKTOP
            |
            |  # value 2
            |  MOBILE
            |}
            |
            |enum AnnotatedEnum @onEnum {
            |  ANNOTATED_VALUE @onEnumValue
            |  OTHER_VALUE
            |}
            |
            |input InputType {
            |  key: String!
            |  answer: Int = 42
            |}
            |
            |input AnnotatedInput @onInputObjectType {
            |  # field comment
            |  annotatedField: Type @onField
            |}
            |
            |extend type Foo {
            |  seven(argument: [String]): Type
            |}
            |
            |extend type Foo @onType {}
            |
            |type NoFields {}
            |
            |directive @skip(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT
            |
            |directive @include(if: Boolean!) on FIELD | FRAGMENT_SPREAD | INLINE_FRAGMENT""".stripMargin) (after being strippedOfCarriageReturns)
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

        AstNode.withoutPosition(ast, stripComments = true) should be (
          AstNode.withoutPosition(prettyParsed, stripComments = true))

        AstNode.withoutPosition(ast, stripComments = true) should be (
            AstNode.withoutPosition(compactParsed, stripComments = true))

        compactRendered should equal (
          """type Foo implements Bar@dfdsfsdf(aaa:1)@qqq(aaa:[1,2]){one:Type two(argument:InputType!):Type three(argument:InputType@aaa(c:b),other:String@ddd(aa:1)@xxx(ttt:"sdfdsf")):Int four(argument:String="string"):String five(argument:[String]=["string","string"]):String@aaaa(if:true) six(argument:InputType={key:"value"}):Type another(argument:InputType={key:"value"},mylist:[String]=["string","string"]):Type}
            |type AnnotatedObject @onObject(arg:"value"){annotatedField(arg:Type="default"@onArg):Type@onField}
            |interface Bar{one:Type four(argument:String="string"):String}
            |union Feed=Story|Article|Advert
            |union AnnotatedUnion@onUnion=A|B
            |scalar CustomScalar
            |scalar AnnotatedScalar@onScalar
            |enum Site{DESKTOP MOBILE}
            |enum AnnotatedEnum@onEnum{ANNOTATED_VALUE@onEnumValue OTHER_VALUE}
            |input InputType {key:String! answer:Int=42}
            |input AnnotatedInput @onInputObjectType{annotatedField:Type@onField key:String! answer:Int=42@foo(bar:baz)}
            |extend type Foo {seven(argument:[String]):Type}
            |directive@skip(if:Boolean!)on FIELD|FRAGMENT_SPREAD|INLINE_FRAGMENT
            |schema@myDir(a:b)@another(a:b,c:1234.45){query:QueryType mutation:MutationType}""".stripMargin)  (after being strippedOfCarriageReturns)

        prettyRendered should equal (
          """# fdwfsdfsdfsd
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
            |}""".stripMargin) (after being strippedOfCarriageReturns)
      }
    }
  }
}