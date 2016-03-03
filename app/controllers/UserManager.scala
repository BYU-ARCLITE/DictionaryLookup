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
        case Some(user) => Ok(Json.obj(
		  "success" -> true,
		  "authkey" -> user.authKey
		))
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
        Ok(Json.obj(
		  "success" -> true,
		  "authkey" -> nuser.authKey
		))
  }

  def resetPass(username: String) = Authentication.authAction(username, parse.multipartFormData) {
    implicit request =>
      implicit user =>
        user.setPass(request.body.dataParts("newpassword")(0))
        Ok(Json.obj("success" -> true))
  }

  def resetEmail(username: String) = Authentication.authAction(username, parse.multipartFormData) {
    implicit request =>
      implicit user =>
        user.setEmail(request.body.dataParts("email")(0))
        Ok(Json.obj("success" -> true))
  }

  val IntPattern = raw"([1-9]\d*|0)".r
  def resetServiceList(username: String) =
    Authentication.authAction(username, parse.multipartFormData) {
      implicit request =>
        implicit user =>
		  import scala.util.Try
          val serviceList = request.body.dataParts.toList
            .collect { case (IntPattern(key), value :: _) =>
			  (key.toInt -> value)
			}
			.sortBy(_._1)
            .map(_._2)
          user.setServices(serviceList)
          Ok(Json.obj("success" -> true))
    }
}
