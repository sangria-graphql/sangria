package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class PossibleFragmentSpreadsSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new PossibleFragmentSpreads)

  "Validate: Possible fragment spreads" should {
    "of the same object" in expectPasses(
      """
        fragment objectWithinObject on Dog { ...dogFragment }
        fragment dogFragment on Dog { barkVolume }
      """)

    "of the same object with inline fragment" in expectPasses(
      """
        fragment objectWithinObjectAnon on Dog { ... on Dog { barkVolume } }
      """)

    "object into an implemented interface" in expectPasses(
      """
        fragment objectWithinInterface on Pet { ...dogFragment }
        fragment dogFragment on Dog { barkVolume }
      """)

    "object into containing union" in expectPasses(
      """
        fragment objectWithinUnion on CatOrDog { ...dogFragment }
        fragment dogFragment on Dog { barkVolume }
      """)

    "union into overlapping interface" in expectPasses(
      """
        fragment unionWithinInterface on Pet { ...catOrDogFragment }
        fragment catOrDogFragment on CatOrDog { __typename }
      """)

    "union into overlapping union" in expectPasses(
      """
        fragment unionWithinUnion on DogOrHuman { ...catOrDogFragment }
        fragment catOrDogFragment on CatOrDog { __typename }
      """)

    "interface into implemented object" in expectPasses(
      """
        fragment interfaceWithinObject on Dog { ...petFragment }
        fragment petFragment on Pet { name }
      """)

    "interface into overlapping interface" in expectPasses(
      """
        fragment interfaceWithinInterface on Pet { ...beingFragment }
        fragment beingFragment on Being { name }
      """)

    "interface into overlapping interface in inline fragment" in expectPasses(
      """
        fragment interfaceWithinInterface on Pet { ... on Being { name } }
      """)

    "interface into overlapping union" in expectPasses(
      """
        fragment interfaceWithinUnion on CatOrDog { ...petFragment }
        fragment petFragment on Pet { name }
      """)

    "different object into object" in expectFails(
      """
        fragment invalidObjectWithinObject on Cat { ...dogFragment }
        fragment dogFragment on Dog { barkVolume }
      """,
      List(
        "Fragment 'dogFragment' cannot be spread here as objects of type 'Cat' can never be of type 'Dog'." → Some(Pos(2, 53))
      ))

    "different object into object in inline fragment" in expectFails(
      """
        fragment invalidObjectWithinObjectAnon on Cat {
          ... on Dog { barkVolume }
        }
      """,
      List(
        "Fragment cannot be spread here as objects of type 'Cat' can never be of type 'Dog'." → Some(Pos(3, 11))
      ))

    "object into not implementing interface" in expectFails(
      """
        fragment invalidObjectWithinInterface on Pet { ...humanFragment }
        fragment humanFragment on Human { pets { name } }
      """,
      List(
        "Fragment 'humanFragment' cannot be spread here as objects of type 'Pet' can never be of type 'Human'." → Some(Pos(2, 56))
      ))

    "object into not containing union" in expectFails(
      """
        fragment invalidObjectWithinUnion on CatOrDog { ...humanFragment }
        fragment humanFragment on Human { pets { name } }
      """,
      List(
        "Fragment 'humanFragment' cannot be spread here as objects of type 'CatOrDog' can never be of type 'Human'." → Some(Pos(2, 57))
      ))

    "union into not contained object" in expectFails(
      """
        fragment invalidUnionWithinObject on Human { ...catOrDogFragment }
        fragment catOrDogFragment on CatOrDog { __typename }
      """,
      List(
        "Fragment 'catOrDogFragment' cannot be spread here as objects of type 'Human' can never be of type 'CatOrDog'." → Some(Pos(2, 54))
      ))

    "union into non overlapping interface" in expectFails(
      """
        fragment invalidUnionWithinInterface on Pet { ...humanOrAlienFragment }
        fragment humanOrAlienFragment on HumanOrAlien { __typename }
      """,
      List(
        "Fragment 'humanOrAlienFragment' cannot be spread here as objects of type 'Pet' can never be of type 'HumanOrAlien'." → Some(Pos(2, 55))
      ))

    "union into non overlapping union" in expectFails(
      """
        fragment invalidUnionWithinUnion on CatOrDog { ...humanOrAlienFragment }
        fragment humanOrAlienFragment on HumanOrAlien { __typename }
      """,
      List(
        "Fragment 'humanOrAlienFragment' cannot be spread here as objects of type 'CatOrDog' can never be of type 'HumanOrAlien'." → Some(Pos(2, 56))
      ))

    "interface into non implementing object" in expectFails(
      """
        fragment invalidInterfaceWithinObject on Cat { ...intelligentFragment }
        fragment intelligentFragment on Intelligent { iq }
      """,
      List(
        "Fragment 'intelligentFragment' cannot be spread here as objects of type 'Cat' can never be of type 'Intelligent'." → Some(Pos(2, 56))
      ))

    "interface into non overlapping interface" in expectFails(
      """
        fragment invalidInterfaceWithinInterface on Pet {
          ...intelligentFragment
        }
        fragment intelligentFragment on Intelligent { iq }
      """,
      List(
        "Fragment 'intelligentFragment' cannot be spread here as objects of type 'Pet' can never be of type 'Intelligent'." → Some(Pos(3, 11))
      ))

    "interface into non overlapping interface in inline fragment" in expectFails(
      """
        fragment invalidInterfaceWithinInterfaceAnon on Pet {
          ...on Intelligent { iq }
        }
      """,
      List(
        "Fragment cannot be spread here as objects of type 'Pet' can never be of type 'Intelligent'." → Some(Pos(3, 11))
      ))

    "interface into non overlapping union" in expectFails(
      """
        fragment invalidInterfaceWithinUnion on HumanOrAlien { ...petFragment }
        fragment petFragment on Pet { name }
      """,
      List(
        "Fragment 'petFragment' cannot be spread here as objects of type 'HumanOrAlien' can never be of type 'Pet'." → Some(Pos(2, 64))
      ))
  }
}
