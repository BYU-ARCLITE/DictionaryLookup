package controllers

import models.{User, ServiceLog}
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global
import controllers.LangCodes._

object LookupGlosbe extends Translator {
  /**
   * Glosbe Terms of Service
   *     API is free to use, regarding indicated data source license. There is a limit of calls
   *     that may be done from one IP in fixed period of time, to prevent from abuse. 
   *     The limit is not strict, there is heuristics that guesses whether queries comes from robot or human.
   *     If there are too many queries or they look non-human – IP gets blocked.
   *     If you are a developer and such case happens: please contact us.
   *     http://glosbe.com/a-api
   *
   * Example Requests
   *     Translation: http://glosbe.com/gapi/translate?from=spa&dest=eng&format=json&phrase=rosa&pretty=true
   *     Example Sentences: http://glosbe.com/gapi/tm?from=spa&dest=eng&format=json&phrase=rosa&page=1&pretty=true
   */

  val name = "Glosbe"
  val expiration = Utils.getExpiration("glosbe")

  /* Grabs all the direct Translations */
  def grabTranslations(json:Seq[JsObject], text:String) : Seq[String] = {
    for { tuc <- json } yield {
      val word = (tuc\ "phrase" \ "text").asOpt[String]
      word.getOrElse("")
    }
  }

  /**
   * Grabs all the definitions with the direct translations
   * Limits to 5 definitions, otherwise it is cumbersome to read 
   */
  def grabPhrase(json:Seq[JsObject], text:String) : Seq[String] = {
  for { tuc <- json } yield {
    val word = (tuc\"phrase"\"text").asOpt[String]

    val meanings = (tuc\ "meanings").asOpt[Seq[JsObject]]
    val defs : Seq[String] = if (meanings == None) Seq("")
      else
        for {
          text <- meanings.get
        } yield {
          val result = (text\ "text").asOpt[String]
          result getOrElse " "
        }
        
        if (defs.distinct == Seq("")) ""
        else ("<b>(" + word.getOrElse(text + "</b> - original text<b>") + ")</b><br>" + defs.distinct.slice(0,2).mkString("<br>"))
    }
  }

  /**
   * Makes request to Glosbe Api, then creates a Seq[String] from a valid response
   */
  def requestEntries(scode: String, dcode: String, text: String) = {

    val query = WS.url("http://glosbe.com/gapi/translate")
        .withQueryString("from" -> scode, "dest" -> dcode, "format" -> "json", "phrase" -> text).get()
    val result = Await.result(query, Duration.Inf)
    val json = result.json.as[JsObject]

    /* Glosbe gives own status, but at this point in time, it seems glitchy
     * When it gets better, it may be beneficial to use it. See Below:
     *  println("Result -> " + (json\"result").as[String])
     */
    if(result.status != 200) None
    else {

        val tuc = (json\ "tuc").asOpt[Seq[JsObject]]
        if (tuc == None) None
        else {
            val trans : String = grabTranslations(tuc.get,text).filterNot(w=>w=="").mkString(", ")
            val defs : Seq[String]= grabPhrase(tuc.get, text).filterNot(w => w=="")

            val addTransTitle = if (trans.length > 0) ("<b><i>Translations:</i></b><br>"+trans+"<br><br>") else ""

            if (defs.isEmpty && (addTransTitle == "")) None
            else if (defs.isEmpty) Some(Seq(addTransTitle))
            else Some((defs.updated(0, addTransTitle + "<b><i>Definitions:</i></b><br>" + defs(0))).filterNot(w=>w=="").slice(0,5))
        }
    }
  }

  /**
   * Endpoint for translating via Glosbe
   */
  def translate(user: User, src: String, dest: String, text: String) = {
    val upgrdSrc = upgradeLangCode(src)
    val upgrdDest = upgradeLangCode(dest)

    if (upgrdSrc != None && upgrdDest != None)
        requestEntries(upgrdSrc.get, upgrdDest.get, text)
    else {play.Logger.debug("Nothing found from Glosbe") 
    None} 
  }
}