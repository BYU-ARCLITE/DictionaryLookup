package controllers

import java.net.URLEncoder
import play.api.mvc.{Action, Controller}
import play.api.cache.Cache
import play.api.libs.json.{JsValue, JsObject, JsString, JsUndefined, Json}
import edu.byu.arclite.dictionary.DictionaryCache
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import scala.collection.mutable.ListBuffer
import scala.util.control.ControlThrowable
import scala.annotation.tailrec
import ExecutionContext.Implicits.global
import play.api.libs.ws.WS
import play.api.cache.Cache
import play.api.{Logger, Play}
import Play.current

trait Translator {
  def translate(src: String, dest: String, text: String): Option[JsObject]
}

object LookupGoogle extends Translator {

  val googleKey = Play.configuration.getString("google.key")

  /**
   * Endpoint for translating via Google
   * @param src The source language
   * @param dest The destination language
   * @param text The text to translate
   */
  def translate(src: String, dest: String, text: String) = {
    try googleKey.flatMap { key =>
      val query = WS.url("https://www.googleapis.com/language/translate/v2")
        .withQueryString("source" -> src, "target" -> dest, "q" -> text, "key" -> key).get()
      val result = Await.result(query, Duration.Inf)
      if(result.status != 200) None
      else
        Some(Json.obj("entries" -> (result.json \ "data" \ "translations" \\ "translatedText"), "source" -> "Google Translate"))
    } catch {
      case e: ControlThrowable => throw e
      case e: Throwable => {
        Logger.debug(e.getMessage)
        e.printStackTrace()
        None
      }
    }
  }
}

object LookupWordReference extends Translator {
  //See http://www.wordreference.com/docs/api.aspx
  val wordReferenceKey = Play.configuration.getString("wordReference.key")
  val TranslationList = List("FirstTranslation", "SecondTranslation", "ThirdTranslations", "FourthTranslation")
  val PartsList = List("Entries","PrincipalTranslations","AdditionalTranslations")
  val whitespace = "\\s".r

  def forEntriesIn(json: JsValue)(body: JsValue => Unit) = {
    var term = json.as[JsObject].fields foreach { f =>
      val (key, obj) = f
      if(key.startsWith("term") && obj.isInstanceOf[JsObject])
        PartsList.foreach { partName =>
          (obj \ partName) match {
            case part:JsObject => part.fields.foreach { f => body(f._2) }
            case _ =>
          }
        }
    }
  }

  /**
   * Endpoint for translating via WordReference
   * @param src The source language
   * @param dest The destination language
   * @param text The text to translate
   */
  def translate(src: String, dest: String, text: String) = {
    if(whitespace.findFirstIn(text).isDefined) None
    else try wordReferenceKey.flatMap { key =>
      val url = "http://api.wordreference.com/0.8/" + URLEncoder.encode(key + "/json/" + src + dest + "/" + text, "UTF-8")
      val result = Await.result(WS.url(url).get(), Duration.Inf)
      if(result.status != 200) None
      else {
        val entries = ListBuffer[String]()
        forEntriesIn(result.json) { entry =>
          val translations = TranslationList.flatMap { name =>
            entry \ name \ "term" match {
              case translation:JsString => Some(translation.as[String])
              case _ => None
            }
          }
          if(translations.length > 0){
            entries += ("(" + (entry \ "OriginalTerm" \ "sense").as[String] + ") " + translations.mkString(", "))
          }
        }
        if(entries.length > 0)
          Some(Json.obj("entries" -> entries, "source" -> "WordReference.com"))
        else None
      }
    } catch {
      case e: ControlThrowable => throw e
      case e: Throwable => {
        Logger.debug(e.getMessage)
        e.printStackTrace()
        None
      }
    }
  }
}

object LookupBYU extends Translator {

  /**
   * Endpoint for translating via BYU Dictionaries
   * @param src The source language
   * @param dest The destination language
   * @param text The text to translate
   */
  def translate(src: String, dest: String, text: String) = {
    val dictionary = src + "-" + dest
    DictionaryCache.getDictionaryEntry(dictionary, text).map { entries =>
      Json.obj("entries" -> entries, "source" -> "BYU Dictionaries")
    }
  }
}

@tailrec
object getFirst extends ((List[Translator], Translator => Option[JsObject]) => Option[JsObject]) {
  def apply(input: List[Translator], extractor: Translator => Option[JsObject]) = input match {
    case first :: rest =>
      val res = extractor(first)
      if(res.isDefined) res
      else apply(rest, extractor)
    case List() => None
  }
}

object Lookup extends Controller {

  val services = List( LookupBYU,
                       LookupWordReference,
                       LookupGoogle )

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
            case Some(obj) => {
              val response = Json.obj("success" -> true) ++ obj
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
