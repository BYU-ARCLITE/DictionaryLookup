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
  type TResult = (String,Int,Seq[String])

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

  def getFirst(
    user: User, format: Symbol,
    srcLang: String, destLang: String,
	text: String, exclusions: Set[String] = Set()
  ): Option[TResult] = {
    for {
	  name <- user.getServices if !exclusions.contains(name)
	  t <- serviceMap.get(name)
	  tsrc <- LangCodes.convert(format, t.codeFormat, srcLang)
	  tdst <- LangCodes.convert(format, t.codeFormat, destLang)
	} try {
      Logger.info("Checking "+name)
      
      if(tsrc == "arz" || tsrc == "apc"){
      t.translate(user, tsrc, tdst, text) match {
        case Some(res) =>
          ServiceLog.record(user, "ara", destLang, text, name, true)
          return Some((t.name,t.expiration,res))
        case None =>{
          ServiceLog.record(user, "ara", destLang, text, name, false)
        }
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

  def lookup(opts: Map[String, Seq[String]], user: User) = (try {
    val srcLang = opts("srcLang")(0)
    val destLang = opts("destLang")(0)
    val format = 'iso639_3
    val text = opts("word")(0)
    val key = s"$srcLang-$destLang:$text"
    Cache.getAs[JsObject](key).map(json => Ok(json)).getOrElse {
      getFirst(user, format, srcLang, destLang, text) match {
        case Some((name,exp,tseq)) => {
          val response = Json.obj(
            "success" -> true,
            "entries" -> tseq,
            "source" -> name
          )
          Cache.set(key, response, exp)
          Logger.info(response.toString)
          Ok(response)
        }
        case None => NotFound(Json.obj("success" -> false, "message" -> "No dictionary entries."))
      }
    }
  } catch {
    case e: ControlThrowable => throw e
    case _: Throwable => BadRequest
  }).withHeaders("Access-Control-Allow-Origin" -> "*")

  def authlookup = Authentication.keyedAction(parse.urlFormEncoded) {
    implicit request =>
      implicit user =>
        lookup(request.body, user)
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
