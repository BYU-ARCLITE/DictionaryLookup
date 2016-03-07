package controllers

import play.api._
import play.api.mvc._

object Application extends Controller {

  def index = Action {
    implicit request =>
      Ok(views.html.index())
  }

  def register = Action {
    implicit request =>
      Ok(views.html.register())
  }

  def manageAccount = Action {
    implicit request =>
      Ok(views.html.manageAccount())
  }

  def lookupTest = Action {
    implicit request =>
      Ok(views.html.lookupTest())
  }

  def aboutDict = Action {
    implicit request =>
      Ok(views.html.aboutDict())
  }

}