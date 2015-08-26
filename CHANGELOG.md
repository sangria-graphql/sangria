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
