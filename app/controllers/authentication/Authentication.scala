package controllers.authentication

import play.api.mvc._
import models.User

object Authentication extends Controller {

  type Handler[A] = Request[A] => User => Result
  /**
   * @param f The action logic. A curried function which, given a request and the authenticated user, returns a result.
   * @return The result. Either a redirect, or the result returned by f.
   */
  def keyedAction[A](parser: BodyParser[A] = parse.anyContent)(f: Handler[A]) = Action(parser) {
    implicit request => try {
      request.headers.get("Authorization")
        .flatMap(User.findByKey _)
        .map(f(request))
        .getOrElse(Unauthorized)
      } catch {
        case _: Throwable => BadRequest
      }
  }

  def authAction[A](username: String, parser: BodyParser[A] = parse.anyContent)(f: Handler[A]) = Action(parser) {
    implicit request => try {
      val response = for {
        password <- request.headers.get("Authorization")
        user <- User.findByUsername(username)
        if user.checkpw(password)
      } yield f(request)(user)
      response.getOrElse(Unauthorized)
    } catch {
      case _: Throwable => BadRequest
    }
  }
}
