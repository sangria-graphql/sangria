package sangria.starWars

import org.scalatest.{Matchers, WordSpec}
import sangria.parser.QueryParser
import sangria.starWars.TestSchema.StarWarsSchema
import sangria.util.FutureResultSupport
import sangria.validation.QueryValidator
import scala.util.Success

class StartWarsValidationSpec extends WordSpec with Matchers with FutureResultSupport {
  "Basic Queries" should {
    "Validates a complex but valid query" in {
      val Success(query) = QueryParser.parse("""
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

      QueryValidator.default.validateQuery(StarWarsSchema, query) should be ('empty)
    }

    "Notes that non-existent fields are invalid" in {
      val Success(query) = QueryParser.parse("""
        query HeroSpaceshipQuery {
          hero {
            favoriteSpaceship
          }
        }
        """)

      QueryValidator.default.validateQuery(StarWarsSchema, query) should have size 1
    }

    "Requires fields on objects" in {
      val Success(query) = QueryParser.parse("""
        query HeroNoFieldsQuery {
          hero
        }
        """)

      QueryValidator.default.validateQuery(StarWarsSchema, query) should have size 1
    }

    "Disallows fields on scalars" in {
      val Success(query) = QueryParser.parse("""
        query HeroFieldsOnScalarQuery {
          hero {
            name {
              firstCharacterOfName
            }
          }
        }
        """)

      QueryValidator.default.validateQuery(StarWarsSchema, query) should have size 1
    }

    "Disallows object fields on interfaces" in {
      val Success(query) = QueryParser.parse("""
        query DroidFieldOnCharacter {
          hero {
            name
            primaryFunction
          }
        }
        """)

      QueryValidator.default.validateQuery(StarWarsSchema, query) should have size 1
    }

    "Allows object fields in fragments" in {
      val Success(query) = QueryParser.parse("""
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

      QueryValidator.default.validateQuery(StarWarsSchema, query) should be ('empty)
    }

    "Allows object fields in inline fragments" in {
      val Success(query) = QueryParser.parse("""
        query DroidFieldInFragment {
          hero {
            name
            ... on Droid {
              primaryFunction
            }
          }
        }
        """)

      QueryValidator.default.validateQuery(StarWarsSchema, query) should be ('empty)
    }
  }
}
