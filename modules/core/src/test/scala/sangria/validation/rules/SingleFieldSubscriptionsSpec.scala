package sangria.validation.rules

import sangria.util.{Pos, ValidationSupport}
import org.scalatest.wordspec.AnyWordSpec

class SingleFieldSubscriptionsSpec extends AnyWordSpec with ValidationSupport {

  override val defaultRule = Some(new SingleFieldSubscriptions)

  "Validate: Subscriptions with single field" should {
    "valid subscription" in expectPasses(
      """
        subscription ImportantEmails {
          importantEmails
        }
      """)

    "fails with more than one root field" in expectFails(
      """
        subscription ImportantEmails {
          importantEmails
          notImportantEmails
        }
      """,
      List(
        "Subscription 'ImportantEmails' must select only one top level field." -> Some(Pos(4, 11))
      ))

    "fails with more than one root field including introspection" in expectFails(
      """
        subscription ImportantEmails {
          importantEmails
          __typename
        }
      """,
      List(
        "Subscription 'ImportantEmails' must select only one top level field." -> Some(Pos(4, 11))
      ))

    "fails with many more than one root field" in expectFailsPosList(
      """
        subscription ImportantEmails {
          importantEmails
          notImportantEmails
          spamEmails
        }
      """,
      List(
        "Subscription 'ImportantEmails' must select only one top level field." -> List(Pos(4, 11), Pos(5, 11))
      ))

    "fails with more than one root field in anonymous subscriptions" in expectFailsPosList(
      """
        subscription {
          importantEmails
          notImportantEmails
        }
      """,
      List(
        "Anonymous Subscription must select only one top level field." -> List(Pos(4, 11))
      ))
  }
}
