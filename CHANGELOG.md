## ??? (???)

* #45 - Added `Long` scalar type
* #49 - `UpdateCtxAction` should also work for query types
* #50 - Sanity check - fields should have unique name within the same type definition
* #31, #32 - More test coverage for "projections" and "deferred" features
* Minor bigfix that makes it possible now for interfaces to have a list of interfaces


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
