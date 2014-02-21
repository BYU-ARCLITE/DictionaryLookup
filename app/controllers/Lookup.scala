package controllers

import scala.util.control.ControlThrowable
import controllers.authentication.Authentication
import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import play.api.cache.Cache
import play.api.Play.current
import play.api.Logger

object Lookup extends Controller {
  type TResult = (String,Int,Seq[String])
  type Extractor = Translator => Option[Seq[String]]

  val serviceMap = Map( "BYUDictionaries" -> LookupBYU,
                        "WordReference" -> LookupWordReference,
                        "GoogleTranslate" -> LookupGoogle )

  val defaultServices = List("BYUDictionaries", "WordReference", "GoogleTranslate")

  def getFirst(input: List[String], extractor: Extractor): Option[TResult] = {
    for(name <- input; t <- serviceMap.get(name)) try {
      Logger.info("Checking "+name)
      val res = extractor(t)
      if(res.isDefined)
        return res.map((t.name,t.expiration,_))
    } catch {
      case e: ControlThrowable => throw e
      case e: Throwable => {
        Logger.debug(e.getMessage)
        e.printStackTrace()
      }
    }
    None
  }

  def lookup(opts: Map[String, Seq[String]], priority: List[String]) = (try {
    val srcLang = opts("srcLang")(0)
    val destLang = opts("destLang")(0)
    val text = opts("word")(0)
    val key = s"$srcLang-$destLang:$text"
    Cache.getAs[JsObject](key).map(json => Ok(json)).getOrElse {
      getFirst(priority, _.translate(srcLang,destLang,text)) match {
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

  def getlookup = Action {
    implicit request =>
      lookup(request.queryString, defaultServices)
  }

  def authlookup = Authentication.keyedAction(parse.urlFormEncoded) {
    implicit request =>
      implicit user =>
        lookup(request.body, user.getServices)
  }
}
