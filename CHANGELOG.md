## v0.4.3 (2015-12-02)

* A lot of performance improvements across the whole library
* Added basic subscription support as defined in the spec (https://github.com/facebook/graphql/pull/109) and reference implementation (#89).
  At the moment subscription is pretty basic, so it's meant more for experiments rather than use in real applications.
* `null` value support (as defined in the spec change: https://github.com/facebook/graphql/pull/83) (#55)



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

  val exceptionHandler: PartialFunction[(ResultMarshaller, Throwable), HandledException] = {
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
  at a schema creation time all default values would be validated according to the input type. You can find more info in [docs](http://sangria-graphql.org/learn/#middleware) and [auth example](http://sangria-graphql.org/learn/#middleware-based-auth)
* #77 - Middleware support. This addition has a huge potential: you can measure performance, collect metrics, enforce security, etc. on a field and query level. Moreover
  it makes it much easier for people to share standard middleware in a libraries (e.g. sangria-security, sangria-graphite, sangria-influxdb, etc.). In order to ensure generic classification of
  fields, every field now got a generic list or `FieldTag`s which allow to provide user-defined meta information about this field
  (just to highlight a few examples: `Permission("ViewOrders")`, `Authorized`, `Measured`, etc.).
* #76 - You can now provide `maxQueryDepth` to `Executor`. It will then enforce this constraint for all queries (very useful if query has recursive types) [Docs](http://sangria-graphql.org/learn/#limiting-query-depth)
* #69 - `DeferredResolver` now got `userContext` as an argument. (breaking change: you need to provide a type parameter and one extra argument in `resolve` for your `DeferredResolver`s. you you are not interested in `userContext`, you can just use `Any` type)
* Renamed Json support objects in order to make more concise import syntax (breaking change: you need to rename imports as well):
  * `sangria.integration.CirceSupport` -> `sangria.integration.circe`
  * `sangria.integration.Json4sSupport` -> `sangria.integration.json4s`
  * `sangria.integration.PlayJsonSupport` -> `sangria.integration.playJson`
  * `sangria.integration.SprayJsonSupport` -> `sangria.integration.sprayJson`
* `ResultMarshaller` and `InputUnmarshaller` are moved in the `integration` package
* Renamed execution `arguments` to `variables` in order to be consistent with the spec (breaking change: you need to rename this argument as well, if you are using named arguments)
* Refactored variables and `InputUnmarshaller`. In order to avoid extra complexity it now does not have a dependent type. Instead it uses "type tagging" for scala map variables.
  It's a minor breaking change. If you are providing execution variables as a scala map, then you need to use `mapVars` or `emptyMapVars` which are defined in `InputUnmarshaller` companion object (these functions do not wrap `Map` - they only needed to ensure type constraints):
  ```scala
  Executor.execute(mySchema, query, variables = mapVars(Map("someId" -> "1000")))

  // or

  Executor.execute(mySchema, query, variables = mapVars("someId" -> "1000"))
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
