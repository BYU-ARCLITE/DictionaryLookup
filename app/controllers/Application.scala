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
/*
  def resetKey(name: String) = Action {
    implicit request =>
      Ok(views.html.resetKey(name))
  }

  def resetPass(name: String) = Action {
    implicit request =>
      Ok(views.html.resetPass(name))

  def resetServiceList(name: String) = Action {
    implicit request =>
      Ok(views.html.resetPass(name))
  }
*/
}