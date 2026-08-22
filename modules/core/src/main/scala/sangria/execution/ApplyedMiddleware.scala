package sangria.execution

class ApplyedMiddleware[Ctx, M <: Middleware[Ctx]](val queryVal: M#QueryVal, val middleware: M)

//trait ApplyedMiddleware3[Ctx, M <: Middleware[Ctx]] {
//  def middleware: M
//  def queryVal: M#QueryVal
//
//  def afterQuery(context: MiddlewareQueryContext[Ctx, _, _]): Unit =
//    middleware.afterQuery(queryVal, context)
//
//}
//object ApplyedMiddleware3 {
//  def apply[Ctx, M <: Middleware[Ctx]](m: M)(qval: m.QueryVal): ApplyedMiddleware3[Ctx, M] =
//    new ApplyedMiddleware3[Ctx, M] {
//      override val middleware: M = m
//      override val queryVal: M#QueryVal = qval
//    }
//}
