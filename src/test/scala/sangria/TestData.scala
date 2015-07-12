package sangria

import sangria.schema.{Deferred, DeferredResolver}

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

  case class DeferFriends[Ctx](friends: List[String]) extends Deferred[Ctx, List[Character]]

  class CharacterRepo extends DeferredResolver {
    val characters = List[Character]()

    def getHero() = characters(0)

    def getHuman(id: String): Human = characters.find(c => c.isInstanceOf[Human] && c.id == id).asInstanceOf[Human]

    def getDroid(id: String): Droid = characters.find(c => c.isInstanceOf[Human] && c.id == id).asInstanceOf[Droid]

    override def resolve(deferred: List[Deferred[_, _]]) = deferred map {
      case DeferFriends(friendIds) => friendIds map (id => characters.find(_.id == id).get)
    }
  }
}
