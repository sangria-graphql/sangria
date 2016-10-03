package sangria.streaming

import sangria.schema.{Action, ValidOutType}

import language.higherKinds
import scala.annotation.implicitNotFound

@implicitNotFound(msg =
    "Type `${S}` is invalid subscription type. Possible reasons:\n" +
    "  * You have not imported one of the available stream implementation with `sangria.streaming.someImpl._`\n" +
    "  * You have not imported or defined execution context/materializer required for a particular stream implementation \n" +
    "  * The elements inside of the stream are not of the type `${A}[${Ctx}, ${Res}]`. If it's the case, consider transforming values in `${A}`s with something like `stream.map(action(_))`\n" +
    "  * The action resulting type `${Res}` is not compatible with GraphQL type `${Out}`")
trait SubscriptionStreamLike[S, A[_, _], +Ctx, Res, Out] {
  type StreamSource[_]

  def subscriptionStream: SubscriptionStream[StreamSource]
}

object SubscriptionStreamLike {
  implicit def default[S[_], A[_, _], Ctx, Res, Out](implicit ev: SubscriptionStream[S], ev1: ValidOutType[Res, Out]): SubscriptionStreamLike[S[A[Ctx, Res]], A, Ctx, Res, Out] =
    new SubscriptionStreamLike[S[A[Ctx, Res]], A, Ctx, Res, Out] {
      type StreamSource[X] = S[X]
      val subscriptionStream = ev
    }

}

