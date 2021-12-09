package sangria.schema

import sangria.ast
import sangria.ast.Document
import sangria.marshalling.InputUnmarshaller

object SchemaMaterializer {

  /** Build a `Schema` for use by client tools.
    *
    * Given the result of a client running the introspection query, creates and returns a `Schema`
    * instance which can be then used with all sangria tools, but cannot be used to execute a query,
    * as introspection does not represent the "resolver", "parse" or "serialize" functions or any
    * other server-internal mechanisms.
    *
    * @param introspectionResult
    *   the result of introspection query
    */
  def buildFromIntrospection[T: InputUnmarshaller](introspectionResult: T): Schema[Any, Any] =
    IntrospectionSchemaMaterializer.buildSchema[T](introspectionResult)

  /** Build a `Schema` for use by client tools.
    *
    * Given the result of a client running the introspection query, creates and returns a `Schema`
    * instance which can be then used with all sangria tools, but cannot be used to execute a query,
    * as introspection does not represent the "resolver", "parse" or "serialize" functions or any
    * other server-internal mechanisms.
    *
    * @param introspectionResult
    *   the result of introspection query
    */
  def buildFromIntrospection[Ctx, T: InputUnmarshaller](
      introspectionResult: T,
      builder: IntrospectionSchemaBuilder[Ctx]): Schema[Ctx, Any] =
    IntrospectionSchemaMaterializer.buildSchema[Ctx, T](introspectionResult, builder)

  def buildFromAst(document: ast.Document): Schema[Any, Any] =
    AstSchemaMaterializer.buildSchema(document)

  def buildFromAst[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]): Schema[Ctx, Any] =
    AstSchemaMaterializer.buildSchema[Ctx](document, builder)

  def buildStubFromAst(document: ast.Document): Schema[Any, Any] =
    AstSchemaMaterializer.buildSchema(Document.emptyStub + document)

  def buildStubFromAst[Ctx](
      document: ast.Document,
      builder: AstSchemaBuilder[Ctx]): Schema[Ctx, Any] =
    AstSchemaMaterializer.buildSchema[Ctx](Document.emptyStub + document, builder)

  def buildDefinitions(document: ast.Document): Vector[Named] =
    AstSchemaMaterializer.definitions(document)

  def buildDefinitions[Ctx](document: ast.Document, builder: AstSchemaBuilder[Ctx]): Vector[Named] =
    AstSchemaMaterializer.definitions[Ctx](document, builder)
}
