## Sangria

**Sangria** is a scala [GraphQL](http://facebook.github.io/graphql/) client and server library.

The initial version is WIP. More information about the project will come soon.

[![Build Status](https://travis-ci.org/OlegIlyenko/sangria.svg)](https://travis-ci.org/OlegIlyenko/sangria)

SBT Configuration:

    libraryDependencies += "com.github.olegilyenko" %% "sangria" % "0.0.1"

The version number speaks for itself, don't use it in production just yet :)

SBT Configuration (snapshot):

    resolvers += "Sonatype snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/"
    libraryDependencies += "com.github.olegilyenko" %% "sangria" % "0.0.2-SNAPSHOT"


Examples below demonstrate some of the features, that are implemented so far. More features will come very soon!

### Query Parser and Renderer

Example usage:

```scala
import sangria.ast.Document
import sangria.parser.QueryParser
import sangria.renderer.QueryRenderer

import scala.util.Success

val query =
  """
    query FetchLukeAndLeiaAliased(
          $someVar: Int = 1.23
          $anotherVar: Int = 123) @include(if: true) {
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

    fragment Foo on User @foo(bar: 1) {
      baz
    }
  """

// Parse GraphQl query
val Success(document: Document) = QueryParser.parse(query)

// Pretty rendering of GraphQl query as a `String`
println(QueryRenderer.render(document))

// Compact rendering of GraphQl query as a `String`
println(QueryRenderer.render(document, QueryRenderer.Compact))
```

### Schema Definition

Here is an example of GraphQL schema DSL:

```scala
import sangria.schema._

val EposideEnum = EnumType(
  "Episode",
  Some("One of the films in the Star Wars Trilogy"),
  List(
    EnumValue("NEWHOPE",
      value = TestData.Eposide.NEWHOPE,
      description = Some("Released in 1977.")),
    EnumValue("EMPIRE",
      value = TestData.Eposide.EMPIRE,
      description = Some("Released in 1980.")),
    EnumValue("JEDI",
      value = TestData.Eposide.JEDI,
      description = Some("Released in 1983."))))

val Character: InterfaceType[Unit, TestData.Character] =
  InterfaceType(
    "Character",
    "A character in the Star Wars Trilogy",
    () => List[Field[Unit, TestData.Character]](
      Field("id", StringType,
        Some("The id of the character."),
        resolve = _.value.id),
      Field("name", OptionType(StringType),
        Some("The name of the character."),
        resolve = _.value.name),
      Field("friends", ListType(Character),
        Some("The friends of the character, or an empty list if they have none."),
        resolve = ctx => DeferFriends(ctx.value.friends)),
      Field("appearsIn", ListType(EposideEnum),
        Some("Which movies they appear in."),
        resolve = _.value.appearsIn)
    ))

val Human =
  ObjectType[Unit, Human](
    "Human",
    "A humanoid creature in the Star Wars universe.",
    List[Field[Unit, Human]](
      Field("id", StringType,
        Some("The id of the human."),
        resolve = _.value.id),
      Field("name", OptionType(StringType),
        Some("The name of the human."),
        resolve = _.value.name),
      Field("friends", ListType(Character),
        Some("The friends of the human, or an empty list if they have none."),
        resolve = (ctx) => DeferFriends(ctx.value.friends)),
      Field("homePlanet", OptionType(StringType),
        Some("The home planet of the human, or null if unknown."),
        resolve = _.value.homePlanet)
    ),
    Character :: Nil)

val Droid = ObjectType[Unit, Droid](
  "Droid",
  "A mechanical creature in the Star Wars universe.",
  List[Field[Unit, Droid]](
    Field("id", StringType,
      Some("The id of the droid."),
      resolve = Projection("_id", _.value.id)),
    Field("name", OptionType(StringType),
      Some("The name of the droid."),
      resolve = ctx => Future.successful(ctx.value.name)),
    Field("friends", ListType(Character),
      Some("The friends of the droid, or an empty list if they have none."),
      resolve = ctx => DeferFriends(ctx.value.friends)),
    Field("primaryFunction", OptionType(StringType),
      Some("The primary function of the droid."),
      resolve = Projection(_.value.primaryFunction))
  ),
  Character :: Nil)

val ID = Argument("id", StringType)

val Query = ObjectType[CharacterRepo, Unit](
  "Query", List[Field[CharacterRepo, Unit]](
    Field("hero", Character, resolve = (ctx) => ctx.ctx.getHero),
    Field("human", OptionType(Human),
      arguments = ID :: Nil,
      resolve = ctx => ctx.ctx.getHuman(ctx arg ID)),
    Field("droid", Droid,
      arguments = ID :: Nil,
      resolve = Projector((ctx, f)=> ctx.ctx.getDroid(ctx arg ID).get)),
    Field("test", OptionType(Droid),
      resolve = ctx => UpdateCtx(Future.successful(ctx.ctx.getDroid("2001").get))(droid => ctx.ctx)),
    Field("project", OptionType(Droid), resolve =
        Projector((ctx, projections) => {
          println("Projected fields: " + projections.flatMap(_.asVector))
          ctx.ctx.getDroid("2001")
        }))
  ))

val StarWarsSchema = Schema(Query)
```

#### Deferred Values and Resolver

In the example schema, you probably noticed, that some of the resolve functions return `DeferFriends`. It is defined like this:

```scala
case class DeferFriends(friends: List[String]) extends Deferred[List[Character]]
```

Defer mechanism allows you to postpone the execution of particular fields and then batch them together in order to optimise object retrieval.
In this example all of the characters have list of friends, but they only have IDs of them. You need to fetch from somewhere in order to progress
query execution. Retrieving evey friend one-by-one would be inefficient, since you potentially need to access an external database
in order to do so. Defer mechanism allows you to batch all these friend list retrieval requests in one efficient request to the DB. In order to do it,
you need to implement a `DeferredResolver`, that will get a list of deferred values:

```scala
class FriendsResolver extends DeferredResolver {
  override def resolve(deferred: List[Deferred[Any]]): Future[List[List[Character]]] =
    // your bulk friends retrieving logic
}
```

#### Projections

Sangria also introduces the concept of projections. If you are fetching your data from the database (like let's say MongoDB), then it can be
very helpful to know which fields are needed for the query ahead-of-time in order to make efficient projection in the DB query.

`Projector` and `Projection` allow you to do this. They both can wrap a `resolve` function. `Projector` enhances wrapped `resolve` function
with the list of projected fields, and `Projection` allows you to mark fields which are relevant for `Projector` and also allows you to customise projected
field name (this is helpful, if your DB field names are different from the GraphQL field names).

#### Input and Context Objects

Many schema elements, like `ObjectType`, `Field` or `Schema` itself, take two type parameters: `Ctx` and `Val`:

* `Val` - represent values that are returned by `resolve` function and given to resolve function as a part of the `Context`. In the schema example,
  `Val` can be a `Human`, `Droid`, `String`, etc.
* `Ctx` - represents some contextual object that flows across the whole execution (and doesn't change in most of the cases). It can be provided to execution by the user
  in order to help fulfill the GraphQL query. A typical example of such context object is as service or repository object that is able to access
  a Database. In example schema some of the fields, like `droid` or `human` make use of it in order to access the character repository.

### Schema Execution

Here is an example of how you can execute example schema:

```scala
import sangria.execution.Executor

Executor(TestSchema.StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
  .execute(queryAst, arguments = Some(vars))
```

The result of the execution is a `Future` of marshaled GraphQL result (see next section)

### Result Marshalling and Input Unmarshalling

GraphQL query execution needs to know how to serialize the result of execution and how to deserialize arguments/variables.
Sangria does not hard-code the serialisation mechanism. Instead it provides two traits for this:

* `ResultMarshaller` - knows how to serialize results of execution
* `InputUnmarshaller[Node]` - knows how to deserialize the arguments/variables

At the moment Sangria provides these implementations:

* `sangria.integration.Json4sSupport` - json4s serialization/deserialization
* `sangria.integration.SprayJsonSupport` - spray-json serialization/deserialization
* `sangria.integration.PlayJsonSupport` - play-json serialization/deserialization
* The default one, which serializes/deserializes to scala `Map`/`List`

In order to use one of these, just import it and the result of execution will be of the correct type:

```scala
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
```

## Mailing List

If you would like to discuss something about sangria or have some questions, please feel free to join our mailing list:

[https://groups.google.com/forum/#!forum/sangria-graphql](https://groups.google.com/forum/#!forum/sangria-graphql)

## License

**Sangria** is licensed under [Apache License, Version 2.0](http://www.apache.org/licenses/LICENSE-2.0).
