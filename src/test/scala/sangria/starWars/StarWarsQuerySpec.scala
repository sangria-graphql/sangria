package sangria.starWars

import org.scalatest.{Matchers, WordSpec}
import sangria.execution.Executor
import sangria.marshalling.InputUnmarshaller
import sangria.parser.QueryParser
import sangria.schema._
import sangria.starWars.TestSchema.StarWarsSchema
import sangria.starWars.TestData.{FriendsResolver, CharacterRepo}
import sangria.util.FutureResultSupport
import InputUnmarshaller.mapVars

import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Success

class StarWarsQuerySpec extends WordSpec with Matchers with FutureResultSupport {
  "Basic Queries" should {
    "Correctly identifies R2-D2 as the hero of the Star Wars Saga" in {
      val Success(query) = QueryParser.parse("""
        query HeroNameQuery {
          hero {
            name
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver).execute(query).await should be (
        Map(
          "data" → Map(
            "hero" → Map(
              "name" → "R2-D2"))))
    }

    "Allows us to query for the ID and friends of R2-D2" in {
      val Success(query) = QueryParser.parse("""
        query HeroNameAndFriendsQuery {
          hero {
            id
            name
            friends {
              name
            }
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver).execute(query).await should be (
        Map(
          "data" → Map(
            "hero" → Map(
              "id" → "2001",
              "name" → "R2-D2",
              "friends" → List(
                Map("name" → "Luke Skywalker"),
                Map("name" → "Han Solo"),
                Map("name" → "Leia Organa")
              )))))
    }

    "Hero query should succeed even if not all types are referenced indirectly" in {
      val Success(query) = QueryParser.parse("""
        query HeroNameAndFriendsQuery {
          hero {
            id
            name
            friends {
              name
            }
          }
        }
        """)

      val HeroOnlyQuery = ObjectType[CharacterRepo, Unit](
        "HeroOnlyQuery", fields[CharacterRepo, Unit](
          Field("hero", TestSchema.Character,
            arguments = TestSchema.EpisodeArg :: Nil,
            resolve = (ctx) ⇒ ctx.ctx.getHero(ctx.arg(TestSchema.EpisodeArg)))
        ))

      val heroOnlySchema = Schema(HeroOnlyQuery, additionalTypes = TestSchema.Human :: TestSchema.Droid :: Nil)

      Executor(heroOnlySchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver).execute(query).await should be (
        Map(
          "data" → Map(
            "hero" → Map(
              "id" → "2001",
              "name" → "R2-D2",
              "friends" → List(
                Map("name" → "Luke Skywalker"),
                Map("name" → "Han Solo"),
                Map("name" → "Leia Organa")
              )))))
    }
  }

  "Nested Queries" should {
    "Allows us to query for the friends of friends of R2-D2" in {
      val Success(query) = QueryParser.parse("""
        query NestedQuery {
          hero {
            name
            friends {
              name
              appearsIn
              friends {
                name
              }
            }
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver).execute(query).await should be (
        Map(
          "data" → Map(
            "hero" → Map(
              "name" → "R2-D2",
              "friends" → List(
                Map(
                  "name" → "Luke Skywalker",
                  "appearsIn" → List("NEWHOPE", "EMPIRE", "JEDI"),
                  "friends" → List(
                    Map("name" → "Han Solo"),
                    Map("name" → "Leia Organa"),
                    Map("name" → "C-3PO"),
                    Map("name" → "R2-D2")
                  )
                ),
                Map(
                  "name" → "Han Solo",
                  "appearsIn" → List("NEWHOPE", "EMPIRE", "JEDI"),
                  "friends" → List(
                    Map("name" → "Luke Skywalker"),
                    Map("name" → "Leia Organa"),
                    Map("name" → "R2-D2")
                  )
                ),
                Map(
                  "name" → "Leia Organa",
                  "appearsIn" → List("NEWHOPE", "EMPIRE", "JEDI"),
                  "friends" → List(
                    Map("name" → "Luke Skywalker"),
                    Map("name" → "Han Solo"),
                    Map("name" → "C-3PO"),
                    Map("name" → "R2-D2")
                  )
                )
              )
            )
          )
        )
      )
    }
  }

  "Using IDs and query parameters to refetch objects" should {
    "Allows us to query for Luke Skywalker directly, using his ID" in {
      val Success(query) = QueryParser.parse("""
        query FetchLukeQuery {
          human(id: "1000") {
            name
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver).execute(query).await should be (
        Map(
          "data" → Map(
            "human" → Map(
              "name" → "Luke Skywalker"
            )
          )
        ))
    }

    "Allows us to create a generic query, then use it to fetch Luke Skywalker using his ID" in {
      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = mapVars("someId" → "1000")

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, variables = args).await should be (
          Map(
            "data" → Map(
              "human" → Map(
                "name" → "Luke Skywalker"
              )
            )
          ))
    }

    "Allows us to create a generic query, then use it to fetch Han Solo using his ID" in {
      val Success(query) = QueryParser.parse("""
        query FetchSomeIDQuery($someId: String!) {
          human(id: $someId) {
            name
          }
        }
        """)

      val args = mapVars("someId" → "1002")

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, variables = args).await should be (
          Map(
            "data" → Map(
              "human" → Map(
                "name" → "Han Solo"
              )
            )
          ))
    }

    "Allows us to create a generic query, then pass an invalid ID to get null back" in {
      val Success(query) = QueryParser.parse("""
        query humanQuery($id: String!) {
          human(id: $id) {
            name
          }
        }
        """)

      val args = mapVars("id" → "not a valid id")

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
        .execute(query, variables = args).await should be (
          Map(
            "data" → Map(
              "human" → null
            )
          ))
    }
  }

  "Using aliases to change the key in the response" should {
    "Allows us to query for Luke, changing his key with an alias" in {
      val Success(query) = QueryParser.parse(
        """
        query FetchLukeAliased {
          luke: human(id: "1000") {
            name
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
          .execute(query).await should be (
        Map(
          "data" → Map(
            "luke" → Map(
              "name" → "Luke Skywalker")
          )
        ))
    }

    "Allows us to query for both Luke and Leia, using two root fields and an alias" in {
      val Success(query) = QueryParser.parse(
        """
        query FetchLukeAndLeiaAliased {
          luke: human(id: "1000") {
            name
          }
          leia: human(id: "1003") {
            name
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
          .execute(query).await should be (
        Map(
          "data" → Map(
            "luke" → Map(
              "name" → "Luke Skywalker"),
            "leia" → Map(
              "name" → "Leia Organa")
          )
        ))
    }
  }

  "Uses fragments to express more complex queries" should {
    "Allows us to query using duplicated content" in {
      val Success(query) = QueryParser.parse("""
        query DuplicateFields {
          luke: human(id: "1000") {
            name
            homePlanet
          }
          leia: human(id: "1003") {
            name
            homePlanet
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
          .execute(query).await should be (
        Map(
          "data" → Map(
            "luke" → Map(
              "name" → "Luke Skywalker",
              "homePlanet" → "Tatooine"),
            "leia" → Map(
              "name" → "Leia Organa",
              "homePlanet" → "Alderaan")
          )
        ))
    }

    "Allows us to use a fragment to avoid duplicating content" in {
      val Success(query) = QueryParser.parse("""
        query UseFragment {
          luke: human(id: "1000") {
            ...HumanFragment
          }
          leia: human(id: "1003") {
            ...HumanFragment
          }
        }

        fragment HumanFragment on Human {
          name
          homePlanet
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
          .execute(query).await should be (
        Map(
          "data" → Map(
            "luke" → Map(
              "name" → "Luke Skywalker",
              "homePlanet" → "Tatooine"),
            "leia" → Map(
              "name" → "Leia Organa",
              "homePlanet" → "Alderaan")
          )
        ))
    }
  }

  "Using __typename to find the type of an object" should {
    "Allows us to verify that R2-D2 is a droid" in {
      val Success(query) = QueryParser.parse("""
        query CheckTypeOfR2 {
          hero {
            __typename
            name
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
          .execute(query).await should be (
        Map(
          "data" → Map(
            "hero" → Map(
              "__typename" → "Droid",
              "name" → "R2-D2")
          )
        ))
    }

    "Allows us to verify that Luke is a human" in {
      val Success(query) = QueryParser.parse("""
        query CheckTypeOfLuke {
          hero(episode: EMPIRE) {
            __typename
            name
          }
        }
        """)

      Executor(StarWarsSchema, userContext = new CharacterRepo, deferredResolver = new FriendsResolver)
          .execute(query).await should be (
        Map(
          "data" → Map(
            "hero" → Map(
              "__typename" → "Human",
              "name" → "Luke Skywalker")
          )
        ))
    }
  }
}