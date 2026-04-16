package controllers

import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}
import sangria.execution.Executor
import sangria.macros.LiteralGraphQLStringContext
import starWars.TestData.{CharacterRepo, FriendsResolver}
import starWars.TestSchema.StarWarsSchema

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future

class TestExecutorController(val controllerComponents: ControllerComponents) extends BaseController {

  def index(): Action[AnyContent] = Action.async {

    val query = graphql"""
              query HeroNameQuery {
                hero {
                  name
                }
              }
              """

    val result: Future[Any] =  Executor
        .execute(StarWarsSchema, query, new CharacterRepo, deferredResolver = new FriendsResolver)

    result.map { x =>
      Ok(s"ok:$x")
    }
  }
}