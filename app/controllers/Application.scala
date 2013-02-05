package controllers

import play.api._
import play.api.mvc._
import edu.byu.arclite.dictionary.DictionaryCache

object Application extends Controller {
  
  def index = Action {
    implicit request =>
      Ok(views.html.index())
  }
  
}