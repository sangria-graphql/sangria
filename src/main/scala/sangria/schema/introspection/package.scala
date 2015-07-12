package sangria.schema

package object introspection {
  val __Type = ???

  val __InputValue = ObjectType(
    name = "__InputValue",
    fields = List[Field[Unit, Argument[_]]](
      Field("name", StringType, resolve = _.value.name),
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field("type", __Type, resolve = _.value.argumentType),
      Field("defaultValue", OptionType(StringType), resolve = ???)
    ))

  val __Directive = ObjectType(
    name = "__Directive",
    fields = List[Field[Unit, Directive]](
      Field("name", StringType, resolve = _.value.name),
      Field("description", OptionType(StringType), resolve = _.value.description),
      Field("args", ListType(__InputValue), resolve = _.value.arguments),
      Field("onOperation", BooleanType, resolve = _.value.onOperation),
      Field("onFragment", BooleanType, resolve = _.value.onFragment),
      Field("onField", BooleanType, resolve = _.value.onField)
    ))

  val __Schema = ObjectType(
    name = "__Schema",
    description =
        "A GraphQL Schema defines the capabilities of a GraphQL " +
        "server. It exposes all available types and directives on " +
        "the server, as well as the entry points for query and " +
        "mutation operations.",
    fields = List[Field[Unit, Unit]](
      Field("types", ListType(__Type), Some("A list of all types supported by this server."), resolve = ???),
      Field("queryType", __Type, Some("The type that query operations will be rooted at."), resolve = ???),
      Field("mutationType", OptionType(__Type),
        Some("If this server supports mutation, the type that mutation operations will be rooted at."), resolve = ???),
      Field("directives", ListType(__Directive),
        Some("A list of all directives supported by this server."), resolve = _.schema.directives)))
}
