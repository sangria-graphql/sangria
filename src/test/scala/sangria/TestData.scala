package sangria

import sangria.schema.{Deferred, DeferredResolver}

import scala.util.Success

object TestData {
  object Eposide extends Enumeration {
    val NEWHOPE, EMPIRE, JEDI = Value
  }

  trait Character {
    def id: String
    def name: Option[String]
    def friends: List[String]
    def appearsIn: List[Eposide.Value]
  }

  case class Human(id: String, name: Option[String], friends: List[String], appearsIn: List[Eposide.Value], homePlanet: Option[String]) extends Character
  case class Droid(id: String, name: Option[String], friends: List[String], appearsIn: List[Eposide.Value], primaryFunction: Option[String]) extends Character

  case class DeferFriends(friends: List[String]) extends Deferred[List[Character]]

  class CharacterRepo extends DeferredResolver {
    val characters = List[Character](
      Human(
        id = "1000",
        name = Some("Luke Skywalker"),
        friends = List("1002", "1003", "2000", "2001"),
        appearsIn = List(Eposide.NEWHOPE, Eposide.EMPIRE, Eposide.JEDI),
        homePlanet = Some("Tatooine")),
      Human(
        id = "1001",
        name = Some("Darth Vader"),
        friends = List("1004"),
        appearsIn = List(Eposide.NEWHOPE, Eposide.EMPIRE, Eposide.JEDI),
        homePlanet = Some("Tatooine")),
      Human(
        id = "1002",
        name = Some("Han Solo"),
        friends = List("1000", "1003", "2001"),
        appearsIn = List(Eposide.NEWHOPE, Eposide.EMPIRE, Eposide.JEDI),
        homePlanet = None),
      Human(
        id = "1003",
        name = Some("Leia Organa"),
        friends = List("1000", "1002", "2000", "2001"),
        appearsIn = List(Eposide.NEWHOPE, Eposide.EMPIRE, Eposide.JEDI),
        homePlanet = Some("Alderaan")),
      Human(
        id = "1004",
        name = Some("Wilhuff Tarkin"),
        friends = List("1001"),
        appearsIn = List(Eposide.NEWHOPE, Eposide.EMPIRE, Eposide.JEDI),
        homePlanet = None),

      Droid(
        id = "2000",
        name = Some("C-3PO"),
        friends = List("1000", "1002", "1003", "2001"),
        appearsIn = List(Eposide.NEWHOPE, Eposide.EMPIRE, Eposide.JEDI),
        primaryFunction = Some("Protocol")),
      Droid(
        id = "2001",
        name = Some("R2-D2"),
        friends = List("1000", "1002", "1003"),
        appearsIn = List(Eposide.NEWHOPE, Eposide.EMPIRE, Eposide.JEDI),
        primaryFunction = Some("Astromech"))
    )

    def getHero() = characters(0)

    def getHuman(id: String): Human = characters.find(c => c.isInstanceOf[Human] && c.id == id).asInstanceOf[Human]

    def getDroid(id: String): Droid = characters.find(c => c.isInstanceOf[Human] && c.id == id).asInstanceOf[Droid]

    override def resolve(deferred: List[Deferred[_]]) = deferred map {
      case DeferFriends(friendIds) => Success(friendIds map (id => characters.find(_.id == id).get))
    }
  }
}
