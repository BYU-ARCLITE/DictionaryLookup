package controllers

import scala.util.control.ControlThrowable
import controllers.authentication.Authentication
import models.User
import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import play.api.cache.Cache
import play.api.Play.current
import play.api.Logger

object UserManager extends Controller {

  def register = Action(parse.urlFormEncoded) {
    implicit request => try {
      val body = request.body
      val username = body("username")(0)
      User.create(
        username,
        body("password")(0),
        body("email")(0)
      ) match {
        case Some(user) => Ok(user.authKey)
        case None =>
          BadRequest(Json.obj(
            "success" -> false,
            "message" -> s"Username '$username' is already taken."
          ))
      }
    } catch {
      case e: Throwable =>
        play.Logger.debug("In register: " + e.getMessage())
        BadRequest(Json.obj(
          "success" -> false,
          "message" -> e.getMessage()
        ))
    }
  }

  def resetKey(username: String) = Authentication.authAction(username) {
    implicit request =>
      implicit user =>
        val nuser = user.setKey()
        Ok(nuser.authKey)
  }

  def resetPass(username: String) = Authentication.authAction(username, parse.urlFormEncoded) {
    implicit request =>
      implicit user =>
        val nuser = user.setPass(request.body("newpassword")(0))
        Ok(nuser.authKey)
  }

  def resetEmail(username: String) = Authentication.authAction(username, parse.urlFormEncoded) {
    implicit request =>
      implicit user =>
        user.setEmail(request.body("email")(0))
        Ok
  }

  def resetServiceList(username: String) = Authentication.authAction(username, parse.urlFormEncoded) {
    implicit request =>
      implicit user =>
        import scala.util.Try
        val serviceList = request.body.iterator
          .flatMap { case (key, values) => Try(key.toInt -> values).toOption }
          .toList.sortBy(_._1)
          .flatMap(_._2)
        user.setServices(serviceList)
        Ok
  }
}
