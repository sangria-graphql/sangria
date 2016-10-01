package sangria.starWars

import sangria.execution.deferred.{Deferred, DeferredResolver}

import scala.concurrent.{ExecutionContext, Future}
import scala.util.Try

object TestData {
  object Episode extends Enumeration {
    val NEWHOPE, EMPIRE, JEDI = Value
  }

  trait Character {
    def id: String
    def name: Option[String]
    def friends: List[String]
    def appearsIn: List[Episode.Value]
  }

  case class Human(id: String, name: Option[String], friends: List[String], appearsIn: List[Episode.Value], homePlanet: Option[String]) extends Character
  case class Droid(id: String, name: Option[String], friends: List[String], appearsIn: List[Episode.Value], primaryFunction: Option[String]) extends Character

  case class DeferFriends(friends: List[String]) extends Deferred[List[Option[Character]]]

  val characters = List[Character](
    Human(
      id = "1000",
      name = Some("Luke Skywalker"),
      friends = List("1002", "1003", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine")),
    Human(
      id = "1001",
      name = Some("Darth Vader"),
      friends = List("1004"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Tatooine")),
    Human(
      id = "1002",
      name = Some("Han Solo"),
      friends = List("1000", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None),
    Human(
      id = "1003",
      name = Some("Leia Organa"),
      friends = List("1000", "1002", "2000", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = Some("Alderaan")),
    Human(
      id = "1004",
      name = Some("Wilhuff Tarkin"),
      friends = List("1001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      homePlanet = None),

    Droid(
      id = "2000",
      name = Some("C-3PO"),
      friends = List("1000", "1002", "1003", "2001"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Protocol")),
    Droid(
      id = "2001",
      name = Some("R2-D2"),
      friends = List("1000", "1002", "1003"),
      appearsIn = List(Episode.NEWHOPE, Episode.EMPIRE, Episode.JEDI),
      primaryFunction = Some("Astromech"))
  )

  class FriendsResolver extends DeferredResolver[Any] {
    override def resolve(deferred: Vector[Deferred[Any]], ctx: Any, queryState: Any)(implicit ec: ExecutionContext) = deferred map {
      case DeferFriends(friendIds) ⇒
        Future.fromTry(Try(friendIds map (id ⇒ characters.find(_.id == id))))
    }
  }

  class CharacterRepo {
    def getHero(episode: Option[Episode.Value]) =
      episode flatMap (_ ⇒ getHuman("1000")) getOrElse characters.last

    def getHuman(id: String): Option[Human] = characters.find(c ⇒ c.isInstanceOf[Human] && c.id == id).asInstanceOf[Option[Human]]

    def getDroid(id: String): Option[Droid] = characters.find(c ⇒ c.isInstanceOf[Droid] && c.id == id).asInstanceOf[Option[Droid]]

    def getCharacters(ids: Seq[String]): Seq[Character] = ids.flatMap(id ⇒ characters.find(_.id == id))
  }
}
