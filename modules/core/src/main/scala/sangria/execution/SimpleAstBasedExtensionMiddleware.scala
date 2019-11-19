package sangria.execution

import sangria.ast

class SimpleAstBasedExtensionMiddleware[Ctx](extensionFn: MiddlewareQueryContext[Ctx, _, _] => ast.Value) extends Middleware[Ctx] with MiddlewareExtension[Ctx] {
  override type QueryVal = Unit

  override def beforeQuery(context: MiddlewareQueryContext[Ctx, _, _]) = ()
  override def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Ctx, _, _]) = ()

  def afterQueryExtensions(queryVal: QueryVal, context: MiddlewareQueryContext[Ctx, _, _]) = {
    import sangria.marshalling.queryAst._

    Vector(Extension(extensionFn(context)))
  }
}
