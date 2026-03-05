package sangria.starWars

import sangria.parser.QueryParser
import sangria.starWars.TestSchema.StarWarsSchema
import sangria.util.FutureResultSupport
import sangria.validation.QueryValidator
import org.scalatest.matchers.should.Matchers
import org.scalatest.TryValues._
import org.scalatest.wordspec.AnyWordSpec

class StartWarsValidationSpec extends AnyWordSpec with Matchers with FutureResultSupport {
  "Basic Queries" should {
    "Validates a complex but valid query" in {
      val query = QueryParser
        .parse("""
        query NestedQueryWithFragment {
          hero {
            ...NameAndAppearances
            friends {
              ...NameAndAppearances
              friends {
                ...NameAndAppearances
              }
            }
          }
        }

        fragment NameAndAppearances on Character {
          name
          appearsIn
        }
        """)
        .success
        .value

      QueryValidator.default.validateQuery(StarWarsSchema, query, Map.empty, None) should be(
        Symbol("empty"))
    }

    "Notes that non-existent fields are invalid" in {
      val query = QueryParser
        .parse("""
        query HeroSpaceshipQuery {
          hero {
            favoriteSpaceship
          }
        }
        """)
        .success
        .value

      QueryValidator.default.validateQuery(
        StarWarsSchema,
        query,
        Map.empty,
        None) should have size 1
    }

    "Requires fields on objects" in {
      val query = QueryParser
        .parse("""
        query HeroNoFieldsQuery {
          hero
        }
        """)
        .success
        .value

      QueryValidator.default.validateQuery(
        StarWarsSchema,
        query,
        Map.empty,
        None) should have size 1
    }

    "Disallows fields on scalars" in {
      val query = QueryParser
        .parse("""
        query HeroFieldsOnScalarQuery {
          hero {
            name {
              firstCharacterOfName
            }
          }
        }
        """)
        .success
        .value

      QueryValidator.default.validateQuery(
        StarWarsSchema,
        query,
        Map.empty,
        None) should have size 1
    }

    "Disallows object fields on interfaces" in {
      val query = QueryParser
        .parse("""
        query DroidFieldOnCharacter {
          hero {
            name
            primaryFunction
          }
        }
        """)
        .success
        .value

      QueryValidator.default.validateQuery(
        StarWarsSchema,
        query,
        Map.empty,
        None) should have size 1
    }

    "Allows object fields in fragments" in {
      val query = QueryParser
        .parse("""
        query DroidFieldInFragment {
          hero {
            name
            ...DroidFields
          }
        }

        fragment DroidFields on Droid {
          primaryFunction
        }
        """)
        .success
        .value

      QueryValidator.default.validateQuery(StarWarsSchema, query, Map.empty, None) should be(
        Symbol("empty"))
    }

    "Allows object fields in inline fragments" in {
      val query = QueryParser
        .parse("""
        query DroidFieldInFragment {
          hero {
            name
            ... on Droid {
              primaryFunction
            }
          }
        }
        """)
        .success
        .value

      QueryValidator.default.validateQuery(StarWarsSchema, query, Map.empty, None) should be(
        Symbol("empty"))
    }
  }
}
