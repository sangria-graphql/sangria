package sangria.execution

import sangria.streaming.SubscriptionStream

trait FieldTag

case class SubscriptionField[S[_]](stream: SubscriptionStream[S]) extends FieldTag
