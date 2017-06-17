## v1.2.2 (2017-06-17)

* Added new middleware traits `MiddlewareFromScalar` and `MiddlewareToScalar`. They provide a way to transform all scalar values from middleware (#249, #248). This have some advantages since middleware can be disable, chained together and has access to context information. Huge thanks to @BjRo and @Axxiss for helping with the feature design and implementation!
* Added new middleware trait `MiddlewareExtension` (#256). It provides an easy way to add [extensions](https://facebook.github.io/graphql/#sec-Response-Format) from middleware.
* Improved error message for `All fields within a Type should have unique names!` (#247). It now includes type and field information.
* Fixed helper methods for operation lookup in `ast.Document`.

## v1.2.1 (2017-05-18)

* Fixed `MeasureQueryDepth` reducer not keeping the largest depth found (#245, #246). Big thanks @Eluinhost for this contribution!
* Easier way to create `Args` (#243). Big thanks @vishalkuo for this contribution!
* More options to render a schema (#241). This improvement is especially useful for apps that use relay modern.

## v1.2.0 (2017-04-29)

* Provide convenient functions for IDL-based schema materialization and validation (#240). . For more info see the ["Query Validation" section of documentation](http://sangria-graphql.org/learn/#query-validation). Improvements include:
  * Introduced Schema.buildStubFromAst that builds a schema with a stub Query type
  * Introduced Schema.buildDefinitions that builds a list of definitions based on IDL AST (without a schema)
  * Introduced Document.emptyStub as a most basic, but valid document stub
  * Introduced alias query1 + query2 for document merge
* Add `Fetcher.deferSeqOptExplicit` or similar to explicitly get `Seq[Option[T]]` in the result (#230)
* Fixed scalar aliases when they are used with variables or schema is extended (#237)
* Preserve IDL directives at schema materialization time and use them in schema comparator (#236). This also adds `Vector[ast.Directive]` to all relevant schema definitions which may be extremely useful for future features and schema analysis
* Improve syntax error reporting for `graphql` macro (#235)
* Improve `Int`, `BigInt` and `Long` scalar value handling (#234)
* Propagate updated value through middleware's `afterField` (#233). For more info see the ["Middleware" section of documentation](http://sangria-graphql.org/learn/#middleware).
* Forbid 'true', 'false' and 'null' as names for Enum value (#239)

## v1.1.0 (2017-03-11)

* Added scalar type alias support (#225, #210). For more info see the ["Scalar Type Alias" section of documentation](http://sangria-graphql.org/learn/#scalar-type-alias).
* Greatly improved `AstVisitor` (#214). It now includes helper methods to traverse and **transform** an AST with type info and state. Sangria now integrates with [macro-visit](https://github.com/sangria-graphql/macro-visit) which was specifically written to traverse and transform ASTs which are similar to sangria's. For more info see the ["AstVisitor" section of documentation](http://sangria-graphql.org/learn/#astvisitor).
* Added `DocumentAnalyzer` and `SchemaBasedDocumentAnalyzer` that contain a lot of helper methods to analyze query. This includes newly introduced `deprecatedUsages` and `introspectionUsages` (#211, #207, #212). For more info see brand new ["Query And Schema Analysis" section of documentation](http://sangria-graphql.org/learn/#query-and-schema-analysis).
* Added `QueryReducer.hasIntrospection` and `QueryReducer.rejectIntrospection` that rejects queries which contain introspection fields (#211). This may be useful for production environments where introspection can potentially be abused.
* Added `Context.isIntrospection` and `sangria.introspection.isIntrospection` helper methods to easily distinguish between introspection types and fields in middleware and query reducers.
* Added `QueryReducer.measureDepth` and `QueryReducer.rejectMaxDepth` rejects queries that are deeper than provided threshold (In contrast to `Executor.execute(maxQueryDepth = ...)`, query reducer does it at query analysis time before actual execution).
* Added `derive*` macro settings (`TransformFieldNames` and `TransformInputFieldNames`) to transform field names. (#215) Big thanks to @ostronom for this contribution.
* Added ability to represent complex relations in Fetch API (#220).
* `ExecutionScheme.Extended` now returns updated user context for mutations (#209).
* Improved handling of tailing comments when rendering query AST (#219).
* Added aliases for `graphql`/`graphqlInput` macros: `gql`/`gqlInp`
* Using `Vector` instead of `List` for all AST nodes now. This is a **minor breaking change**.


## v1.0.0 (2017-01-16)

* Added `Action.sequence` combinator to compose a list of `LeafAction`s in a single `LeadAction` that can be returned from a `resolve` function (#206)
* Support of `Option[Id]` in `Fetcher.deferOpt` (#205)
* Implicit conversion from `Future[Deferred[A]]` to `Action` does not work (#201)
* Disallow creation of object types with empty list of fields (#200)
* SimpleFetcherCache does not cache Relation (#194)
* Fetching Relation Typing (#193)
* Helper method `sangroia.schema.action` is replaced with `Action.apply` and `LeafAction.apply`

## v1.0.0-RC5 (2016-11-28)

* Uphold spec for non-validation names not beginning with `__` (spec-change) (#189)
* Field cache does not consider output object polymorphism (#190)
* Added `QueryReducer.measureDepth` and `QueryReducer.rejectMaxDepth` query reducers (#191). `maxQueryDepth` argument was available for a long time on `Executor`, but in contrast to new query reducers, it measures depth along side of the execution. With query reducers it happens before query execution, thus allow to reject the query execution entirely.  
* FetcherDeferredOpt does not extends DeferredOpt (#188)
* Invalid operation name now be considered a client-side error and now implements `QueryAnalysisError` (#186, #187). Big thanks to @mattfenwick for working on this one!

## v1.0.0-RC4 (2016-11-20)

Sangria v1.0.0-RC4 is fully compliant with ["October 2016"](https://github.com/facebook/graphql/releases/tag/October2016) version of the GraphQL specification.

* In presence of explicit `null` value, the default will not be used (#185) (spec change). **Breaking change!** Arguments with default values are no longer treated as non-optional arguments. Though `Args` still preserves existing semantics (default is still applied, even in presence of explicit `null`). The same is true for input objects and optional input object fields with default values. 
* Added `Args.withArgs` and `Args.argDefinedInQuery` convenience methods.
* Validation rule for unique directives per location (#179).
* Enforces input coercion rules (#177).
* `MiddlewareQueryContext` and `ExecutionResult` now contain information about validation and query reducers' execution time (`validationTiming`, `queryReducerTiming`)
* `Middleware.fieldError` was not called in all possible exceptional situations (#184).
* New `QueryParser.parseInputWithVariables` provides a way to parse input values with variables. 
* Various bugfixes in error handing of deferred values and null/undefined value handling. 

## v1.0.0-RC3 (2016-11-05)

* Cross compile to scala 2.12 and scala 2.11 (#182, #183)  
* Schema comparator (#169, #165). It helps to compare different schemas or different versions of the same schema. It also provides an information whether particular change is a breaking change. This is a great example of GraphQL type system potential. For more info see the ["Schema Comparator" section of documentation](http://sangria-graphql.org/learn/#schema-comparator). 
* Improve handling of `NaN` and `infinity` values (#167, #168) 

## v1.0.0-RC2 (2016-10-10)

* Capture original exceptions only if necessary (based on the `ExecutionScheme`)
* Fixed issue with duplicate errors appearing during sequential query execution (mutations)

## v1.0.0-RC1 (2016-10-08)

Towards 1.0!

* Stream-based subscriptions (#98, #166). For more info see the ["Stream-based Subscriptions" section of documentation](http://sangria-graphql.org/learn/#stream-based-subscriptions).
* High-level Fetch API for deferred value resolution (#164). For more info see the ["High-level Fetch API" section of documentation](http://sangria-graphql.org/learn/#high-level-fetch-api).
* Huge improvements in deferred value resolution (#161). [Here is](https://gist.github.com/OlegIlyenko/ea8769eec855fbe1adb304780b8b0e9d) just an example of how batching algorithm is improved in comparison to previous version. For more info see the ["Deferred Value Resolution" section of documentation](http://sangria-graphql.org/learn/#deferred-value-resolution). It got a lot of new content.
* Introduced `ExecutionScheme`. It allows to change the result type of an exaction. So now you can get some meta-information about a query execution itself, and not only the `Future` of marshaled result. For more info see the ["Alternative Execution Scheme" section of documentation](http://sangria-graphql.org/learn/#alternative-execution-scheme).
* Minor breaking changes:
  * `DeferredResolver` and `Deferred` are moved to `sangria.execution.deferred` package.
  * `DeferredResolver.resolve` method signature is changes a bit (2 new arguments were added). Here is the new signature:
    
    ```scala
    def resolve(deferred: Vector[Deferred[Any]], ctx: Ctx, queryState: Any)(implicit ec: ExecutionContext): Vector[Future[Any]]
    ```

## v0.7.3 (2016-08-26)

* Description formatting/parsing is updated based in the changes in the reference implementation (#155). More places of the query now preserve the comments. For instance all trailing comments within a section set and at the end of the document are preserved and rendered.
* Ensure that the result of deferred value resolution has the same size as the deferred list (#154).
* During macro-based derivation, default value should never be a `null` for an optional arguments (#153). 
* Executor now properly handles undefined values (like `null` and `None`) even if GraphQL type is not null (#152)

## v0.7.2 (2016-08-01)

* Transitive types are now collected for all types provided via `additionalTypes` to a schema definition (#149).
*	`ObjectType.withInstanceCheck` provides an easier way to customize an instance check on `ObjectType` (#148).
* `Enumeration` derivation macro should now only collects instances of `Enumeration#Value` (#151).
*	Ensure that all unreferenced types are collected during the schema extension.
* `sangria.ast.Type.namedType` helper method.

## v0.7.1 (2016-07-02)

* Provide `extendSchema` utility function (#113). This feature allows you to extend existing schema with additional types and existing types with additional fields. It may be very useful for client-side tools and for server implementations in cases where parts of a schema are dynamically generated or coming from external sources (like database).
    
  Here is a small example of how you can use it:
    
  ```scala
  val schema: Schema[Ctx, Val] = Schema(...)
  
  val schemaExtensions =
    graphql"""
      extend type Human {
        pet: Animal @loadPetInfo
      }

      interface Animal {
        name: String!
      }

      type Dog implements Animal {
        name: String!
        nickname: String
      }

      type Cat implements Animal {
        name: String!
        age: Int
      }
    """
    
  val myCustomBuilder = new DefaultAstSchemaBuilder[Ctx] {...}
    
  val extendedSchema = 
    schema.extend(schemaExtensions, myCustomBuilder)  
  ``` 
   
  Just like with AST-based schema materialization, you can provide a custom schema builder which allows you to control most of the aspects of generated parts of the schema.  
* Handling of more than one `ProjectionName` for one field (#146).
* Updated context propagated only to siblings (#145).

## v0.7.0 (2016-06-12)

* Initial [CATs (Compatibility Acceptance Tests)](https://github.com/graphql-cats/graphql-cats) support (#142). The test suite itself is still work-in-progress, but sangria includes an integration which executes all currently available test cases. 
* [IDL (schema definition)](https://github.com/facebook/graphql/pull/90) syntax parsing and rendering (#137, #62)
* AST-based schema materializer (#139, #115). This feature may be very useful for different tools that need to create an executable schema based on IDL definitions. Almost any aspect of generated in-memory schema representation can be customized via custom implementation of `AstSchemaBuilder`. This feature is already used in sangria itself for CATs (Compatibility Acceptance Tests) integration. At the moment default implementation of `AstSchemaBuilder` treats comments that start with `##` as a field/object/argument description.        
* Partial resolve `Action`s (#140). This change introduces 2 new `Action`s that can be returned back from a `resolve` function: `PartialValue` and `PartialFutureValue`. This allows you to return a list of errors in addition to a successfully resolved value (which may contain only partial result due to the errors). 
* Preserve comments during the AST parsing (#105). Most of the AST classes got comment `Option[Comment]` field. It can be very useful for query formatting because `QueryRenderer` also got support for comments and able to render them.
* Include execution path in error objects (#143). This may be helpful for client side tools that would like to analyze error messages and programmatically use them in some way. This is a minor braking change since `field` property on error is removed in favor of new `path` property which is a list.   
* Introspection-based schema materializer now also uses more advanced `IntrospectionSchemaBuilder` (similar to the AST-based one) instead of `MaterializationLogic`, which is now removed. This introduces a minor breaking change, but in a long run `IntrospectionSchemaBuilder` will provide much more flexibility.    
* Add comment/directive support in the introspection-based schema renderer (#136).
* Validation: improving overlapping fields quality (#133)  
* Deprecated directive (#132)
* New directive locations (#131)
* Default values should be in GraphQL format (introspection) (#141)
* Added support for case objects defined in companion object (#135). Big thanks to @joprice for contributing this improvement!
* `SchemaRenderer` now has improved default value rendering
* Execution `path` now got it's own class `ExecutionPath` (which is now used instead of simple `Vector[String]`). This introduces a minor breaking change.

## v0.6.3 (2016-05-01)

* Marshaling for [Amazon Ion](http://amznlabs.github.io/ion-docs/index.html) data format is introduced. Amazon Ion is a richly-typed, self-describing, hierarchical data serialization format offering interchangeable binary and text representations.

  You need following dependency to use it:
   
  ```scala
  "org.sangria-graphql" %% "sangria-ion" % "0.1.0"
  ```
  
  In order to use Ion marshalling, you need an implicit instance of `IonSystem` in scope as well:
  
  ```scala
  import sangria.marshalling.ion._
  
  implicit val ionSystem = IonSystemBuilder.standard().build()
  
  val result: Future[IonValue] = Executor.execute(schema, query)
  ```
* Marshalling API is updated to v0.2.1. It introduces a minor breaking change. This change introduces performance improvements to scalar value marshalling and gives much more flexibility in terms of the type of marshaled values. 
   
  `ResultMarshaller` now able to communicate it's natively supported capabilities to a `ScalarType` via `MarshallerCapability`. A set of standard marshaller capabilities were introduced:
  
  * `DateSupport` - Marshaller supports `java.util.Date` natively.
  * `CalendarSupport` - Marshaller supports `java.util.Calendar` natively.
  * `BlobSupport` - Marshaller supports large binary objects in form of `Array[Byte]` natively. 

  This still requires you to create a custom scalar types (for dates, blobs, etc.), but it gives you an ability to generically use native features of underlying data format. 
  
  `ScalarType` now also able to communicate back to marshaller via `ScalarValueInfo`. This can be used, for instance, to represent an `Array[Byte]` as a `clob` type instead of `blob` in formats that support both of them (like Amazon Ion).
* Include possible field, argument, type names when validation fails (#126).
* Deepen introspection query from 3 levels to 7 (#128).
* Improve validation error message when field names conflict (#130).
* Interface hierarchies are not correctly rendered with `SchemaRenderer` (#125).
* Updated parboiled to v2.1.3

## v0.6.2 (2016-04-10)

This release is fully compatible with ["April 2016"](https://github.com/facebook/graphql/releases/tag/April2016) version of the GraphQL specification.

* Return type overlap validation (#124) (spec change).
* `deriveContextObjectType`/`deriveObjectType` do not work with Option arguments in some cases (#123)

## v0.6.1 (2016-04-02)

A minor maintenance release to keep up with the spec changes. 

* Field order in the result now reflects field order in the query (according to the spec) for all marshalling libraries that support field ordering (#99) (spec change). 
* Directive `locations` field replaces `onOperation`, `onFragment` and `onField` (#119) (spec change).
* Low-level marshalling API is improved: it's now possible to use efficient map builders (which also able to preserver an order of the fields). This improves serialization performance and minimizes memory footprint. All marshalling libraries already take advantage of this API.
* `SchemaRenderer` prints duplicated fields for a type that implements an interface (#122)

## v0.6.0 (2016-03-19)

* Macro-Based GraphQL Type Derivation (#120). See ["Macro-Based GraphQL Type Derivation" section of the documentation](http://sangria-graphql.org/learn/#macro-based-graphql-type-derivation) for more info.
* Prepared Queries (#118). See ["Prepared Queries" section of the documentation](http://sangria-graphql.org/learn/#prepared-queries) for more info. 
* `Executor.execute` now returns `Future` with failure if error happened before query execution (#109). It can be extremely helpful when you need to take some action or produce different result in case of error. Typical example is returning different HTTP status code.     

  **CAUTION: breaking change and action needed!** Since things like validation errors and errors in query reducers are now explicitly returned as a `Future` failure and not as a successful result, you need to take some action to handle them. In order to migrate, all you need to do is to add following `recover`:
  
  ```scala
  Executor.execute(schema, query).recover {
    case error: ErrorWithResolver ⇒ error.resolveError
  }
  ```
  
  `recover` function will make sure that all of the errors, that were previously handled internally in `Executor`, are now properly handled. **Code above will produce exactly the same result as before.** `resolveError` produces a valid GraphQL response JSON and will use custom exception handler, if you have provided one.
      
  This new approach to error handling gives you much more flexibility. For example in most cases it makes a lot of sense to return 400 HTTP status code if query validation failed. It was not really possible to do this before. Now you able to do something like this (using playframefork in this particular example):
      
  ```scala
  executor.execute(query, ...)
    .map(Ok(_))
    .recover {
      case error: QueryAnalysisError ⇒ BadRequest(error.resolveError)
      case error: ErrorWithResolver ⇒ InternalServerError(error.resolveError)
    }
  ```
  
  This code will produce status code 400 in case of any error caused by client (query validation, invalid operation name, etc.).
   
  Errors that happened in a query reducer would be wrapped in `QueryReducingError`. Here is an example of returning custom status code in case of error in the query reducer:
  
  ```scala
  val authReducer = QueryReducer.collectTags[MyContext, String] {
    case Permission(name) ⇒ name
  } { (permissionNames, ctx) ⇒
    if (ctx.isUserAuthorized(permissionNames)) ctx
    else throw AuthorizationException("User is not authorized!")
  }
  
  Executor.execute(schema, queryAst, userContext = new MyContext, queryReducers = authReducer :: Nil)
    .map(Ok(_))
    .recover {
      case QueryReducingError(error: AuthorizationException) ⇒ Unauthorized(error.getMessage)
      case error: QueryAnalysisError ⇒ BadRequest(error.resolveError)
      case error: ErrorWithResolver ⇒ InternalServerError(error.resolveError)
    }
  ```
  
  HTTP status code would be 401 for unauthorized users.
  
  If you have issues with the migration, please raise [an issue](https://github.com/sangria-graphql/sangria/issues/new) so that we can find a good solution together.
* **Minor breaking change.** `userContext` and `root` arguments of `Executor` are moved in `Executor.execute` method. 
* Detect name collisions with incompatible types during schema definition (#117)
* Introduced a type alias `Executor.ExceptionHandler` for exception handler partial function 

## v0.5.2 (2016-02-28)

* Added introspection-based schema materializer (#21). This feature has a lot of potential for clint-side tools, testing, mocking, 
  creating facade GraphQL servers, etc.

  Here is simple example of how you can use this feature (Using circe in this particular example):
  
  ```scala
  import io.circe._
  import sangria.marshalling.circe._
  
  val introspectionResults: Json = ??? // coming from other server or file
  val clientSchema: Schema[Unit, Unit] = 
    Schema.buildFromIntrospection(introspectionResults)  
  ```
  
  It takes a results of full introspection query (loaded from the server, file, etc.) and recreates the schema definition with stubs for 
  resolve methods. You can customize a lot of aspects of materialization by providing custom `MaterializationLogic` implementation 
  (you can also extend `DefaultMaterializationLogic` class). This means that you can, for instance, plug in some generic field resolution logic (`resolveField` method) or
  provide generic logic for custom scalars (`coerceScalar*` methods). Without these customisations schema only would be able to execute introspection queries. 
     
  By default, default values (for input object fields and arguments) would be ignored because it's just a string as far as introspection API is concerned. However you can enable default value
  support if you know the format of the default values (in many cases it would be JSON). There is even a helper function for this:
    
  ```scala
  import spray.json._
  import sangria.marshalling.sprayJson._
  
  val clientSchema: Schema[Unit, Unit] = 
    Schema.buildFromIntrospection(introspectionResults,
      MaterializationLogic.withDefaultValues[Unit, JsValue])
  ```
  
  This will inform schema materializer that default values are serialized as JSON and that spray-json should be used to work with them (please note, that 
  circe does not have a built-in JSON parsing support, so it can't be used out-of-the-box here. On the other hand, it's pretty easy to add support for particular circe
  parser by defining an implicit instance of `InputParser` type class).  
* `SchemaRenderer.renderSchema` is now able to render `Schema` objects and only introspection results (#114). This can be useful if you already 
  have schema in memory and don't want to execute an introspection query against the schema in order to render it.
* Query validation rule: Unique variable names (#112)
* Add suggested types to incorrect field message (#111)
* Introspection result now has a parser which deserializes a JSON (or any other format) to a set of case classes. This may simplify client-side tools that work with introspection queries.
  Please use `sangria.introspection.IntrospectionParser.parse` to parse an introspection query results.
* Introduced `InputParser` type class in order provide optional support for default value parsing in schema materialization. 
* Updated descriptions of a scalar values
* Updated dependencies
* Minor improvements

## v0.5.1 (2016-01-23)

* JSON library integration is extracted to separate libraries (#38). Evey integration library will have a separate and independent versioning and release cycle. Following new libraries were introduced:
  * [sangria-marshalling-api](https://github.com/sangria-graphql/sangria-marshalling-api) now includes all of the interfaces that marshalling
    library needs to implement.
  * [sangria-marshalling-testkit](https://github.com/sangria-graphql/sangria-marshalling-testkit) contains a set of generic test cases that can be used
    to test a concrete marshalling library integration.
  * [sangria-spray-json](https://github.com/sangria-graphql/sangria-spray-json) contains an integration with [spray-json](https://github.com/spray/spray-json) library.
    From now on, please use following dependency if you would like to use spray-json support:
    
    ```scala
    libraryDependencies += "org.sangria-graphql" %% "sangria-spray-json" % "0.1.0"
    ```
    
    The package is changed for the sake of consistency. From now on please use following import:
    
    ```scala
    import sangria.marshalling.sprayJson._
    ```
  * [sangria-play-json](https://github.com/sangria-graphql/sangria-play-json) contains an integration with [play-json](https://www.playframework.com/documentation/2.2.x/ScalaJson) library.
    From now on, please use following dependency if you would like to use play-json support:
    
    ```scala
    libraryDependencies += "org.sangria-graphql" %% "sangria-play-json" % "0.1.0"
    ```
    
    The package is changed for the sake of consistency. From now on please use following import:
    
    ```scala
    import sangria.marshalling.playJson._
    ```
  * [sangria-json4s-native](https://github.com/sangria-graphql/sangria-json4s-native) contains an integration with [json4s-native](http://json4s.org) library.
    From now on, please use following dependency if you would like to use json4s-native support:
    
    ```scala
    libraryDependencies += "org.sangria-graphql" %% "sangria-json4s-native" % "0.1.0"
    ```
    
    The package is changed for the sake of consistency. From now on please use following import:
    
    ```scala
    import sangria.marshalling.json4s.native._
    ```
  * [sangria-json4s-jackson](https://github.com/sangria-graphql/sangria-json4s-jackson) contains an integration with [json4s-jackson](http://json4s.org) library.
    From now on, please use following dependency if you would like to use json4s-jackson support:
    
    ```scala
    libraryDependencies += "org.sangria-graphql" %% "sangria-json4s-jackson" % "0.1.0"
    ```
    
    The package is changed for the sake of consistency. From now on please use following import:
    
    ```scala
    import sangria.marshalling.json4s.jackson._
    ```  
  * [sangria-circe](https://github.com/sangria-graphql/sangria-circe) contains an integration with [circe](http://circe.io) library.
    From now on, please use following dependency if you would like to use circe support:
    
    ```scala
    libraryDependencies += "org.sangria-graphql" %% "sangria-circe" % "0.1.0"
    ```
    
    The package is changed for the sake of consistency. From now on please use following import:
    
    ```scala
    import sangria.marshalling.circe._
    ```
* [Argonaut](http://argonaut.io) scala JSON library is now supported via [sangria-argonaut](https://github.com/sangria-graphql/sangria-argonaut) (#59).
  Please use following dependency if you would like to use argonaut support:
  
  ```scala
  libraryDependencies += "org.sangria-graphql" %% "sangria-argonaut" % "0.1.0"
  ```
  
  And here is an import statement:
  
  ```scala
  import sangria.marshalling.argonaut._
  ```
* Added `operationType` and `operation` on `ast.Document` to easily identify the operation type (#110)
* Added a utility function to convert between different input representations (#108). 
  This functionality is available though `sangria.marshalling.MarshallingUtil`. 

## v0.5.0 (2015-12-03)

* A lot of performance improvements across the whole library
* Added basic subscription support as defined in the spec (https://github.com/facebook/graphql/pull/109) and reference implementation (#89).
  At the moment subscriptions are pretty basic, so it's meant more for experiments rather than for use in real applications. 
  It is very likely that this feature will experience breaking changes in the near future (spec change)
* Much better handling of input objects (#37, #70). A new type-class is introduced: `FromInput`. It provides high-level and low-level 
  way to deserialize arbitrary input objects, just like `ToInput`.
   
  In order to use this feature, you need to provide a type parameter to the `InputObjectType`:
   
  ```scala
  case class Article(title: String, text: Option[String])
  
  val ArticleType = InputObjectType[Article]("Article", List(
    InputField("title", StringType),
    InputField("text", OptionInputType(StringType))))
    
  val arg = Argument("article", ArticleType)
  ```
  
  This code will not compile unless you define an implicit instance of `FromInput` for `Article` case class:

  ```scala
  implicit val manual = new FromInput[Article] {
    val marshaller = CoercedScalaResultMarshaller.default
    def fromResult(node: marshaller.Node) = {
      val ad = node.asInstanceOf[Map[String, Any]]

      Article(
        title = ad("title").asInstanceOf[String],
        text = ad.get("text").flatMap(_.asInstanceOf[Option[String]])
    }
  }
  ```

  As you can see, you need to provide a `ResultMarshaller` for desired format and then use a marshaled value to create a domain object based on it.
  Many instances of `FromInput` are already provided out-of-the-box. For instance `FromInput[Map[String, Any]]` was added to support existing map-like 
  data-structure format. All supported Json libraries also provide `FromInput[JsValue]` so that you can use Json AST instead of working with `Map[String, Any]`.
  
  Moreover, play-json and spray-json integration provide support for `Reads` and `JsonFormat`. This means that your domain objects are automatically
  supported as long as you have `Reads` or `JsonFormat` defined for them. For instance this example should compile and work just fine without explicit 
  `FromInput` declaration:
  
  ```scala
  import sangria.integration.playJson._
  import play.api.libs.json._
  
  case class Article(title: String, text: Option[String])
  
  implicit val articleFormat = Json.format[Article]
    
  val ArticleType = InputObjectType[Article]("Article", List(
    InputField("title", StringType),
    InputField("text", OptionInputType(StringType))))
    
  val arg = Argument("article", ArticleType)
  ```
  
  **CAUTION: this is minor breaking change**. Together with `null` value support, this feature changes the way input objects are 
  deserialized into map-like structures (which still happens by default). Optional input fields will now produce input 
  objects like:
  ```scala 
  // for JSON input: {"op1": "foo", "opt2": null} 
  Map("opt1" → Some("foo"), "opt2" → None)
  
  // for JSON input: {"op1": "foo"} 
  Map("opt1" → Some("foo"))
  ```
  instead of (old format):
  ```scala 
  // for JSON input: {"op1": "foo", "opt2": null} 
  Map("opt1" → "foo")
  
  // for JSON input: {"op1": "foo"} 
  Map("opt1" → "foo")
  ```   
  As you can see, this allows you to distinguish between "undefined" json object fields and json object fields that are set to `null`.
* `null` value support (as defined in the spec change: https://github.com/facebook/graphql/pull/83) (#55) (spec change)
* Extracted input value parsing and made it a first-class citizen (#103). So now you can parse and render any `ast.Value` independently from 
  GraphQL query. There is even a new `graphqlInput` macros available:
  ```scala
  import sangria.renderer.QueryRenderer
  import sangria.macros._
  import sangria.ast
  
  val parsed: ast.Value =
    graphqlInput"""
      {
        id: "1234345"
        version: 2 # changed 2 times
        deliveries: [
          {id: 123, received: false, note: null, state: OPEN}
        ]
      }
    """
  
  val rendered: String =
    QueryRenderer.render(parsed, QueryRenderer.PrettyInput)
  
  println(rendered)
  ```
  It will print something like this:
  ```js
  {
    id: "1234345"
    version: 2
    deliveries: [{
      id: 123
      received: false
      note: null
      state: OPEN
    }]
  }
  ```
  `InputUnmarshaller` and `ResultMarshaller` are also now available for it, so you can use `ast.Value` as a variables or it can be a result 
  of GraphQL query execution (instead of more traditional JSON).
* `ToInput`, `InputUnmarshaller` and `ResultMarshaller` are moved to `sangria.marshalling` package.
* Improved error messages for input values (#86). Now they will contain the reason why particular value is invalid.
* Implementations of interfaces can include additional field args (#90) (spec change)
* Loosen overlapping field validation rules (#94) (spec change)
* False positive validation error from fragment cycle when unknown fragment (#95)
* Interfaces with covariant return types (#96)
* A lot of minor changes and performance improvements in validation rules and query validator (#97) (spec change)
* Add error handling in the `SchemaRenderer` (#100)
* Ambiguous implicit when using a `UnionType` bug (#101)
* A lot of internal refactorings (especially in variable and argument processing) to make everything above possible

## v0.4.3 (2015-10-16)

* `QueryReducer` is introduced. It allows you to analyze a query and take an action before it's executed. It provides very similar functionality to
  complexity analysis (introduced in previous release), but in much more generic form. That's because complexity analysis is now rewritten as
  a `QueryReducer`. In order to migrate, you need to replace `measureComplexity` function with `QueryReducer.measureComplexity`. Here is an example:
  ```scala
  val complReducer = QueryReducer.measureComplexity[MyCtx] { (c, ctx) ⇒
    complexity = c
    ctx
  }

  Executor.execute(schema, query,
    userContext = new MyCtx,
    queryReducers = complReducer :: Nil)
  ```
  Since rejection of complex queries is such a common use-case, there is now a helper function to create a reducer for it:
  ```scala
  val rejectComplexQuery = QueryReducer.rejectComplexQueries[MyCtx](14, (c, ctx) ⇒
    new IllegalArgumentException(s"Too complex query: max allowed complexity is 14.0, but got $c"))
  ```
* `Middleware` got a type parameter for `Ctx`. This is a minor breaking change. If you don't use the `userContext` inside of the `Middleware`,
  then you can just parametrize it with `Any`.
* Complexity function on field should also be given the `Ctx` (#87)

## v0.4.2 (2015-10-12)

* Query complexity calculation mechanism is implemented (#85). This mechanism makes a rough estimation of the query complexity before it is executed.
  Every field in the query gets a default score `1.0`. The "complexity" of the query is the sum of all field scores. You can customize the
  field score with `complexity` argument:
  ```scala
  Field("pets", OptionType(ListType(PetType)),
    arguments = Argument("limit", IntType) :: Nil,
    complexity = Some((args, childrenScore) ⇒ 25.0D + args.arg[Int]("limit") * childrenScore),
    resolve = ctx ⇒ ...)
  ```
  If you would like to use this feature, you need to provide `measureComplexity` argument to the `Executor`. For example:
  ```scala
  val rejectComplexQueries = (c: Double) ⇒
    if (c > 1000)
      throw new IllegalArgumentException(s"Too complex query: max allowed complexity is 1000.0, but got $c")
    else ()

  val exceptionHandler: Executor.ExceptionHandler = {
    case (m, e: IllegalArgumentException) ⇒ HandledException(e.getMessage)
  }

  Executor.execute(schema, query,
    exceptionHandler = exceptionHandler,
    measureComplexity = Some(rejectComplexQueries))
  ```
  The complexity of full introspection query (used by tools like GraphiQL) is `102.0`.
* json4s-jackson is now supported in addition to native (#84). This results in minor import change:
  ```scala
  // before

  sangria.integration.json4s._

  // after

  // either (same behaviour as before)
  sangria.integration.json4s.native._

  //or
  sangria.integration.json4s.jackson._
  ```
* json4s is updated to version 3.3.0 (#84)
* Provide a helpful error messages if schema has a broken circular references (which cause `fields` to be null) (#83)

## v0.4.1 (2015-10-03)

For the most part implemented spec changes. Now compatible with "October 2015" version of the GraphQL spec.

* Type condition optional on inline fragments. (#82) (spec change)
* Make operation name optional (#81) (spec change)
* Introspection descriptions for scalars and introspection (#80)
* `beforeField` now able to replace value and prevent `resolve` call (#79). This can be useful for things like caching. It contains minor breaking change - return type of `beforeField` has changed. If you are implementing it, just return `continue` if your `FieldVal` was `Unit` or `continue(someFieldVal)`.
* `Projection` and `NoProjection` should be tags instead of resolve function wrappers (#78). Backwards-incompatible change: you need to replace
  `Projection` with `ProjectionName` tag and `NoProjection` with `ProjectionExclude` tag. here is an example:
  ```scala
  // before

  Field("id", StringType,
    Some("The id of the droid."),
    resolve = Projection("_id", _.value.id)),

  // after

  Field("id", StringType,
    Some("The id of the droid."),
    tags = ProjectionName("_id") :: Nil,
    resolve = _.value.id)
  ```

## v0.4.0 (2015-09-27)

This release contains quite a few backwards-incompatible changes, but fear not - all of them are renames and similar minor changes which should be easy to migrate.
I collected all of them in the change list below. They were necessary in order to ensure consistent naming and improve the structure and flexibility of the library.

* #68 - Better handling of default input values. It's a part of ongoing effort to improve handling of input objects (#37). Default values should now have an instance
  of `ToInput` type-class which is defined for all supported input types like scala map-like data structures, different json ASTs, etc.
  It even supports things like `Writes` from play-json or `JsonFormat` from spray-json by default. This means that you can use your domain objects (like `User` or `Apple`) as a default value for input fields or arguments
  as long as you have `Writes` or `JsonFormat` defined for them. The mechanism is very extensible, of course: you just need to define implicit `ToInput[T]` for a class you want to use as a default value.
  This change makes it impossible to verify the default value type at compile time, since it can have any shape, like Json AST or maybe even some binary format. Don't worry though,
  at a schema creation time all default values would be validated according to the input type.
* #77 - Middleware support. This addition has a huge potential: you can measure performance, collect metrics, enforce security, etc. on a field and query level. Moreover
  it makes it much easier for people to share standard middleware in a libraries (e.g. sangria-security, sangria-graphite, sangria-influxdb, etc.). In order to ensure generic classification of
  fields, every field now got a generic list or `FieldTag`s which allow to provide user-defined meta information about this field
  (just to highlight a few examples: `Permission("ViewOrders")`, `Authorized`, `Measured`, etc.). You can find more info in [docs](http://sangria-graphql.org/learn/#middleware) and [auth example](http://sangria-graphql.org/learn/#middleware-based-auth)
* #76 - You can now provide `maxQueryDepth` to `Executor`. It will then enforce this constraint for all queries (very useful if query has recursive types) [Docs](http://sangria-graphql.org/learn/#limiting-query-depth)
* #69 - `DeferredResolver` now got `userContext` as an argument. (breaking change: you need to provide a type parameter and one extra argument in `resolve` for your `DeferredResolver`s. you you are not interested in `userContext`, you can just use `Any` type)
* Renamed Json support objects in order to make more concise import syntax (breaking change: you need to rename imports as well):
  * `sangria.integration.CirceSupport` → `sangria.integration.circe`
  * `sangria.integration.Json4sSupport` → `sangria.integration.json4s`
  * `sangria.integration.PlayJsonSupport` → `sangria.integration.playJson`
  * `sangria.integration.SprayJsonSupport` → `sangria.integration.sprayJson`
* `ResultMarshaller` and `InputUnmarshaller` are moved in the `integration` package
* Renamed execution `arguments` to `variables` in order to be consistent with the spec (breaking change: you need to rename this argument as well, if you are using named arguments)
* Refactored variables and `InputUnmarshaller`. In order to avoid extra complexity it now does not have a dependent type. Instead it uses "type tagging" for scala map variables.
  It's a minor breaking change. If you are providing execution variables as a scala map, then you need to use `mapVars` or `emptyMapVars` which are defined in `InputUnmarshaller` companion object (these functions do not wrap `Map` - they only needed to ensure type constraints):
  ```scala
  Executor.execute(mySchema, query, variables = mapVars(Map("someId" → "1000")))

  // or

  Executor.execute(mySchema, query, variables = mapVars("someId" → "1000"))
  ```
* #72 - `scala.util.Try` now can be returned from `resolve` in order to indicate a successful or failed result
* #65 - `DeprecationTracker` should be called even if deprecation is in the interface type
* #66 - `DeprecationTracker` should provide more contextual information (breaking change: the signature of `deprecatedFieldUsed` is changed. It now provides much more contextual information, but you need to update the code that implements it)
* #74 - Improved unicode handling (spec change)
* #67 - circe integration throws NoSuchElementException during execution
* #75 - Identical documents should be equal
* #73 - Verify input field uniqueness (spec change - new validation rule)
* Minor bugfixes

## v0.3.1 (2015-08-27)

* #58 - Implement CirceJsonSupport in order to be able to integrate with Circe
* #53 - Add `map` in `Action`
* #53 - Ensure Ctx proper inheritance behavior
*	#33 - `grapql` string context macro to create parsed document and verify query at compile time (big thanks to @dlreeves for implementing this feature). Here is an example how you can use it:
  ```scala
  import sangria.macros._

  val queryAst = graphql"""
    query FetchSomeIDQuery {
      human(id: "1000") {
        name
      }
    }
    """
  ```
  If there is a syntax error in the query, you will see it at the compile time.

## v0.3.0 (2015-08-16)

* #45 - Added `Long` scalar type
* #49 - `UpdateCtxAction` should also work for query types
* #50 - Sanity check - fields should have unique name within the same type definition
* #31, #32 - More test coverage for "projections" and "deferred" features
* #51 - Custom exception handler now should return `message` and list of additional filed
* The `interfaces` property syntax changed. In order to ensure type safety, improve type inference and allow type-class based relations between `InterfaceType` and `ObjectType` you now need to use following syntax:
  ```scala
  val PersonType = ObjectType("Person", interfaces = interfaces[Unit, Person](NamedType, BeingType), fields = ...)
  ```
  instead of old syntax
  ```scala
  val PersonType = ObjectType[Unit, Person]("Person", interfaces = NamedType :: BeingType :: Nil, fields = ...)

  // or

  val PersonType = ObjectType[Unit, Person]("Person", interfaces = List(NamedType, BeingType), fields = ...)
  ```
* Fields in `ObjectType` and `InterfaceType` got small convenience method fields. You now can use it like this:

  ```scala
  val DogType = ObjectType("Dog", fields[Unit, Dog](
    Field("name", OptionType(StringType), resolve = _.value.name),
    Field("barks", OptionType(BooleanType), resolve = _.value.barks)))
  ```
* `withPossibleTypes` was introduced on `InterfaceType` and `Field` in order to provide a convenient way to the list of possible implementation types of interface
* Added convenience method `Executor.execute`
* Other minor improvements to make **sangria-relay** possible

## v0.2.2 (2015-08-09)

* #44 - Add ability to add types explicitly in schema, for cases when they are not referenced anywhere else
  * `Schema` now has additional argument `additionalTypes` which can be used like this: `Schema(HeroOnlyQuery, additionalTypes = Human :: Droid :: Nil)`

## v0.2.1 (2015-08-07)

* Spec changes - sew validation rules:
  * `LoneAnonymousOperation`
  * `UniqueArgumentNames`
  * `UniqueFragmentNames`
  * `UniqueOperationNames`

## v0.2.0 (2015-08-02)

* groupId is changed to **org.sangria-graphql**
* Added missing query validation rules (#30 #29 #28 #27 #26 #25 #24 #23)
* #36 - Change semantics of `Projector` feature
  * `Projector` now gets all of the fields, not only fields marked with `Projection`
  * `Projection` now allows to customize the name
  * `NoProjection` allows to remove field from projections list
  * `Projectior` allows to specify how deep it should look (the level arg)

## v0.1.0 (2015-07-26)

Initial feature-complete release
