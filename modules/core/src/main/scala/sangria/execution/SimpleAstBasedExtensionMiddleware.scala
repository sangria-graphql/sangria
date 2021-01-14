package sangria.execution

import sangria.ast

class SimpleAstBasedExtensionMiddleware[Ctx, F[_]](
    extensionFn: MiddlewareQueryContext[Ctx, _, _, F] => ast.Value)
    extends Middleware[Ctx, F]
    with MiddlewareExtension[Ctx, F] {
  override type QueryVal = Unit

  override def beforeQuery(context: MiddlewareQueryContext[Ctx, _, _, F]) = ()
  override def afterQuery(queryVal: QueryVal, context: MiddlewareQueryContext[Ctx, _, _, F]) = ()

  def afterQueryExtensions(queryVal: QueryVal, context: MiddlewareQueryContext[Ctx, _, _, F]) = {
    import sangria.marshalling.queryAst._

    Vector(Extension(extensionFn(context)))
  }
}
