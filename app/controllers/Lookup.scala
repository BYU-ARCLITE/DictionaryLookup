package controllers

import scala.util.control.ControlThrowable
import controllers.authentication.Authentication
import models.{User, ServiceLog}
import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import play.api.cache.Cache
import play.api.Play.current
import play.api.Logger

object Lookup extends Controller {
  type TResult = (Set[String], JsObject)
  type TRequest = (String, String, String, Symbol)

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

  def callService(user: User, t: Translator, req: TRequest): Option[(Set[String], JsObject, Boolean)] = {
    val (text, rsrc, rdst, format) = req
    val langcodes = for {
	  src <- LangCodes.convert(format, t.codeFormat, rsrc)
	  dst <- LangCodes.convert(format, t.codeFormat, rdst)
	} yield (src, dst)
	
	langcodes.flatMap { case (src, dst) =>
	  val name = t.name
	  val key = s"$name:$src-$dst:$text"

      Logger.info("Checking "+name)
      Cache.getAs[(Set[String], JsObject)](key)
	    .map { case (names, json) => (names, json, true) }
        .orElse {
          t.translate(user, src, dst, text).map { case (names, json) =>
		    Cache.set(key, (names, json), t.expiration)
		    (names, json, false)
		  }
        }
    }
  }

  def getFirst(user: User, req: TRequest, exclusions: Set[String] = Set()): Option[TResult] = {
    for {
	  name <- user.getServices if !exclusions.contains(name)
	  t <- serviceMap.get(name)
	} try {
      Logger.info("Checking "+name)
      callService(user, t, req) match {
      case Some((names, json, cached)) =>
		if (!cached) { ServiceLog.record(user, req, name, true) }
		return Some((names, json))
      case None =>
        ServiceLog.record(user, req, name, false)
      }
    }
    else{
        t.translate(user, tsrc, tdst, text) match {
          case Some(res) =>
            ServiceLog.record(user, srcLang, destLang, text, name, true)
            return Some((t.name,t.expiration,res))
          case None =>{
            ServiceLog.record(user, srcLang, destLang, text, name, false)
          }
        }
      } 
    }
    catch {
      case e: ControlThrowable => throw e
      case e: Throwable => {
        Logger.debug(e.getMessage)
        e.printStackTrace()
      }
    }
    None
  }

  def lookup(opts: Map[String, Seq[String]], user: User) = {
    val text = opts("word")(0)
    val src = opts("srcLang")(0)
    val dst = opts("destLang")(0)
    val format = opts("codeFormat")
	              .lift(0)
	              .map(Symbol(_))
				  .getOrElse('iso639_3)

	val basicResult = Json.obj(
      "src" -> src,
      "dst" -> dst,
	  "codeFormat" -> format.toString,
      "text" -> text
    )

    getFirst(user, (text, src, dst, format)) match {
    case Some((names, result)) =>
      val response = Json.obj(
        "success" -> true,
        "result" -> (result ++ basicResult),
        "sources" -> names.toList
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

  def authlookup = Authentication.keyedAction(parse.urlFormEncoded) {
    implicit request =>
      implicit user => (try {
        lookup(request.body, user)
	  } catch {
        case _: Throwable => BadRequest
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
