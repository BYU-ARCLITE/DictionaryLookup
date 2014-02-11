package controllers

import scala.util.control.ControlThrowable
import play.api.mvc.{Action, Controller}
import play.api.libs.json._
import play.api.cache.Cache
import play.api.Play.current
import play.api.Logger

trait Translator {
  val name: String

  /**
   * Endpoint for translating
   * @param src The source language
   * @param dest The destination language
   * @param text The text to translate
   */
  def translate(src: String, dest: String, text: String): Option[Seq[String]]
}

object Lookup extends Controller {
  type TResult = (String,Seq[String])
  type Extractor = Translator => Option[Seq[String]]

  val services = List( LookupBYU,
                       LookupWordReference,
                       LookupGoogle )

  def getFirst(input: List[Translator], extractor: Extractor): Option[TResult] = {
    for(t <- input) try {
      val res = extractor(t)
      if(res.isDefined)
        return res.map((t.name,_))
    } catch {
      case e: ControlThrowable => throw e
      case e: Throwable => {
        Logger.debug(e.getMessage)
        e.printStackTrace()
      }
    }
    None
  }

  def lookup = Action {
    implicit request =>
      (try {
        // Get the languages and key word
        val srcLang = request.queryString("srcLang")(0)
        val destLang = request.queryString("destLang")(0)
        val text = request.queryString("word")(0).trim
        val key = srcLang+"-"+destLang+":"+text
        Cache.getAs[JsObject](key).map { response => Ok(response) }.getOrElse {
          getFirst(services, _.translate(srcLang,destLang,text)) match {
            case Some((name,tseq)) => {
              val response = Json.obj(
                "success" -> true,
                "entries" -> tseq,
                "source" -> name
              )
              Cache.set(key, response, 3600*24) //keep it for a day
              Ok(response)
            }
            case None => NotFound(Json.obj("success" -> false, "message" -> "No dictionary entries."))
          }
        }
      } catch {
        case e: ControlThrowable => throw e
        case _: Throwable => BadRequest
      }).withHeaders("Access-Control-Allow-Origin" -> "*")
  }
}
