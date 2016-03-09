package controllers

import scala.util.control.ControlThrowable
import controllers.authentication.Authentication
import controllers.Utils._
import models.{User, ServiceLog}
import play.api.mvc.{Action, Controller, RequestHeader}
import play.api.libs.json._
import play.api.cache.Cache
import play.api.Play.current
import play.api.Logger

object Lookup extends Controller {

  val serviceMap = Map( "BYUDictionaries" -> LookupBYU,
                        "WWWJDIC" -> LookupWWWJDIC,
                        "WordReference" -> LookupWordReference,
                        "MerriamWebster" -> LookupMerriamWebster,
                        //"Collins" -> LookupCollins,
                        "Glosbe" -> LookupGlosbe,
                        "SeaLang" -> LookupSeaLang,
                        "GoogleTranslate" -> LookupGoogle,
                        "Madamira" -> LookupMadamira
                      )

  def callService(user: User, t: Translator, req: TRequest)
                 (implicit rh: RequestHeader, restart: TRestart):
                 Option[(JsObject, Boolean)] = {

    val name = t.name
	val tformat = t.codeFormat
    Logger.info(s"Converting codes to ${tformat.toString} for $name")

    val (text, rsrc, rdst, rformat) = req
    val langcodes = for {
      src <- LangCodes.convert(rformat, tformat, rsrc)
      dst <- LangCodes.convert(rformat, tformat, rdst)
    } yield (src, dst)

    langcodes.flatMap { case (src, dst) =>
      val key = s"$name:$src-$dst:$text"
	  Logger.info(s"Checking $name")

      Cache.getAs[JsObject](key)
        .map { json => (json, true) }
        .orElse {
		  (try {
            t.translate(user, src, dst, text)
		  } catch {
		    case e: Throwable =>
			  Logger.debug(s"Error in $name: ${e.getMessage()}")
			  None
		  }).map { json =>
            Cache.set(key, json, t.expiration)
            (json, false)
          }
        }
    }
  }

  def getFirst(rh: RequestHeader, user: User, req: TRequest, exclusions: Set[String] = Set()) :
              Option[JsObject] = {

    val restart : TRestart = { (text: String, excls: Set[String]) =>
      val (_, rsrc, rdst, format) = req
      getFirst(rh, user, (text, rsrc, rdst, format), exclusions ++ excls)
    }

    for {
      name <- user.getServices if !exclusions.contains(name)
      t <- serviceMap.get(name)
    } try {
      callService(user, t, req)(rh, restart) match {
      case Some((json, cached)) =>
        if (!cached) { ServiceLog.record(user, req, name, true) }
        return Some(json)
      case None =>
        ServiceLog.record(user, req, name, false)
      }
    } catch {
      case e: ControlThrowable => throw e
      case e: Throwable => {
        Logger.debug(e.getMessage)
        e.printStackTrace()
      }
    }
    None
  }

  def lookup(rh: RequestHeader, opts: Map[String, Seq[String]], user: User) = {
    val text = opts("text")(0)
    val src = opts("srcLang")(0)
    val dst = opts("dstLang")(0)
    val format = opts.get("codeFormat")
                  .flatMap(_.lift(0))
                  .map(Symbol(_))
                  .getOrElse('iso639_3)

    val basicResult = Json.obj(
      "src" -> src,
      "dst" -> dst,
      "codeFormat" -> format.toString,
      "text" -> text
    )

    getFirst(rh, user, (text, src, dst, format)) match {
    case Some(result) =>
      val response = Json.obj(
        "success" -> true,
        "message" -> "Found dictionary entries.",
        "result" -> (result ++ basicResult)
      )
      Logger.info(response.toString)
      Ok(response)
    case None =>
      NotFound(Json.obj(
        "success" -> false,
        "message" -> "No dictionary entries.",
        "result" -> basicResult
      ))
    }
  }

  def authlookup = Authentication.keyedAction(parse.multipartFormData) {
    implicit request =>
      implicit user => (try {
        lookup(request, request.body.dataParts, user)
      } catch {
        case e: Throwable =>
          play.Logger.debug(e.getMessage())
          BadRequest(Json.obj(
            "success" -> false,
            "message" -> e.getMessage()
          ))
      }).withHeaders("Access-Control-Allow-Origin" -> "*")
  }

  def preflight = Action {
    implicit request =>
      Ok.withHeaders(
        "Allow" -> "OPTIONS, POST, GET",
        "Access-Control-Allow-Origin" -> "*",
        "Access-Control-Allow-Methods" -> "POST, GET",
        "Access-Control-Allow-Headers" -> "Origin, Authorization, Content-Type"
      )
  }
}
