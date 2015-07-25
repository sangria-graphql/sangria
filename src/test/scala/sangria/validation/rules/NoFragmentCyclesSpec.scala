package sangria.validation.rules

import org.scalatest.WordSpec
import sangria.util.{Pos, ValidationSupport}

class NoFragmentCyclesSpec extends WordSpec with ValidationSupport {

  override val defaultRule = Some(new NoFragmentCycles)

  "Validate: No circular fragment spreads" should {
    "single reference is valid" in expectPasses(
      """
        fragment fragA on Dog { ...fragB }
        fragment fragB on Dog { name }
      """)

    "spreading twice is not circular" in expectPasses(
      """
        fragment fragA on Dog { ...fragB, ...fragB }
        fragment fragB on Dog { name }
      """)

    "spreading twice indirectly is not circular" in expectPasses(
      """
        fragment fragA on Dog { ...fragB, ...fragC }
        fragment fragB on Dog { ...fragC }
        fragment fragC on Dog { name }
      """)

    "double spread within abstract types" in expectPasses(
      """
        fragment nameFragment on Pet {
          ... on Dog { name }
          ... on Cat { name }
        }

        fragment spreadsInAnon on Pet {
          ... on Dog { ...nameFragment }
          ... on Cat { ...nameFragment }
        }
      """)

    "spreading recursively within field fails" in expectFails(
      """
        fragment fragA on Human { relatives { ...fragA } },
      """,
      List(
        "Cannot spread fragment fragA within itself." -> Some(Pos(2, 47))
      ))

    "no spreading itself directly" in expectFails(
      """
        fragment fragA on Dog { ...fragA }
      """,
      List(
        "Cannot spread fragment fragA within itself." -> Some(Pos(2, 33))
      ))

    "no spreading itself directly within inline fragment" in expectFails(
      """
        fragment fragA on Pet {
          ... on Dog {
            ...fragA
          }
        }
      """,
      List(
        "Cannot spread fragment fragA within itself." -> Some(Pos(4, 13))
      ))

    "no spreading itself indirectly" in expectFailsPosList(
      """
        fragment fragA on Dog { ...fragB }
        fragment fragB on Dog { ...fragA }
      """,
      List(
        "Cannot spread fragment fragA within itself via fragB." -> List(Pos(2, 33), Pos(3, 33))
      ))

    "no spreading itself indirectly reports opposite order" in expectFailsPosList(
      """
        fragment fragB on Dog { ...fragA }
        fragment fragA on Dog { ...fragB }
      """,
      List(
        "Cannot spread fragment fragB within itself via fragA." -> List(Pos(2, 33), Pos(3, 33))
      ))

    "no spreading itself indirectly within inline fragment" in expectFailsPosList(
      """
        fragment fragA on Pet {
          ... on Dog {
            ...fragB
          }
        }
        fragment fragB on Pet {
          ... on Dog {
            ...fragA
          }
        }
      """,
      List(
        "Cannot spread fragment fragA within itself via fragB." -> List(Pos(4, 13), Pos(9, 13))
      ))

    "no spreading itself deeply" in expectFailsPosList(
      """
        fragment fragA on Dog { ...fragB }
        fragment fragB on Dog { ...fragC }
        fragment fragC on Dog { ...fragO }
        fragment fragX on Dog { ...fragY }
        fragment fragY on Dog { ...fragZ }
        fragment fragZ on Dog { ...fragO }
        fragment fragO on Dog { ...fragA, ...fragX }
      """,
      List(
        "Cannot spread fragment fragA within itself via fragB, fragC, fragO." -> List(Pos(2, 33), Pos(3, 33), Pos(4, 33), Pos(8, 33)),
        "Cannot spread fragment fragX within itself via fragY, fragZ, fragO." -> List(Pos(5, 33), Pos(6, 33), Pos(7, 33), Pos(8, 43))
      ))

    "no spreading itself deeply two paths -- new rule" in expectFailsPosList(
      """
        fragment fragA on Dog { ...fragB, ...fragC }
        fragment fragB on Dog { ...fragA }
        fragment fragC on Dog { ...fragA }
      """,
      List(
        "Cannot spread fragment fragA within itself via fragB." -> List(Pos(2, 33), Pos(3, 33)),
        "Cannot spread fragment fragA within itself via fragC." -> List(Pos(2, 43), Pos(4, 33))
      ))
  }
}
