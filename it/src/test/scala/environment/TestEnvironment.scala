package controllers

import sangria.execution.Executor
import sangria.macros.LiteralGraphQLStringContext
import starWars.TestData.{CharacterRepo, FriendsResolver}
import starWars.TestSchema.StarWarsSchema
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import spray.json._
import DefaultJsonProtocol._

class TestEnvironment {

  type ExecutorResult = Map[String, Map[String, Map[String, String]]]

  def execute(): Future[String] = {

    val query = graphql"""
              query HeroNameQuery {
                hero {
                  name
                }
              }
              """

    val result: Future[Any] = Executor
      .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)

    result.map { obj =>
      val jsonObj = obj.asInstanceOf[ExecutorResult].toJson
      jsonObj.prettyPrint
    }

  }

}
