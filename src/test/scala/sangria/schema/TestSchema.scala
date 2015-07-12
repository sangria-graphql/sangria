package sangria.schema

object TestSchema {
  val EposideEnum = EnumType("Episode", Some("One of the films in the Star Wars Trilogy"), List(
    EnumValue("NEWHOPE", value = 4, description = Some("Released in 1977.")),
    EnumValue("EMPIRE", value = 5, description = Some("Released in 1980.")),
    EnumValue("JEDI", value = 6, description = Some("Released in 1983."))))

  val Character: InterfaceType = InterfaceType("Character", "A character in the Star Wars Trilogy", () => List(
    Field("id", NonNullType(StringType), Some("The id of the character.")),
    Field("name", StringType, Some("The name of the character.")),
    Field("friends", ListType(Character), Some("The friends of the character, or an empty list if they have none.")),
    Field("appearsIn", ListType(EposideEnum), Some("Which movies they appear in."))
  ))

  val Human = ObjectType("Human", "A humanoid creature in the Star Wars universe.", List(
    Field("id", NonNullType(StringType), Some("The id of the human.")),
    Field("name", StringType, Some("The name of the human.")),
    Field("friends", ListType(Character), Some("The friends of the human, or an empty list if they have none.")),
    Field("homePlanet", StringType, Some("The home planet of the human, or null if unknown."))
  ), Character :: Nil)

  val Droid = ObjectType("Droid", "A mechanical creature in the Star Wars universe.", List(
    Field("id", NonNullType(StringType), Some("The id of the droid.")),
    Field("name", StringType, Some("The name of the droid.")),
    Field("friends", ListType(Character), Some("The friends of the droid, or an empty list if they have none.")),
    Field("primaryFunction", StringType, Some("The primary function of the droid."))
  ), Character :: Nil)

  val ID = Argument("id", NonNullInputType(StringType))

  val Query = ObjectType("Query", List(
    Field("hero", Character),
    Field("human", Human, arguments = ID :: Nil),
    Field("droid", Droid, arguments = ID :: Nil)
  ))

  val StarWarsSchema = Schema(Query)
}
