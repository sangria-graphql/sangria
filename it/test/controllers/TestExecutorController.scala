package controllers

import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import sangria.execution.Executor
import sangria.macros.LiteralGraphQLStringContext
import starWars.TestData.{CharacterRepo, FriendsResolver}
import starWars.TestSchema.StarWarsSchema

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import play.api.libs.json._

class TestExecutorController(val controllerComponents: ControllerComponents)
    extends BaseController {
  type ExecutorResult = Map[String, Map[String, Map[String, String]]]

  def index(): Action[AnyContent] = Action.async {
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
      val json = Json.toJson(obj.asInstanceOf[ExecutorResult])
      Ok(json)
    }
  }
}
