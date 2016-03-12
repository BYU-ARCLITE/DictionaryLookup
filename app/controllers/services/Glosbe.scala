package controllers

import models.User
import Utils._
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.mvc._
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
  val codeFormat = 'iso639_3

  /** Glosbe lacks a well-defined list. It's just whatever people have added. */
  def getPairs = Set[(String, String)]()

  /**
   * Grabs all the definitions with the direct translations
   */
  def processResults(text: String, phrases:Seq[JsObject], dst: String): Seq[JsObject] = {
    val targets = LangCodes.convert('iso639_3, 'iso639_1, dst) match {
    case Some(p2code) => Seq(dst, p2code)
    case _ => Seq(dst)
    }

    def filterLanguage(obj: JsObject) = {
      (obj \ "language").asOpt[String].flatMap { lang =>
        if (!targets.contains(lang)) None
        else (obj \ "text").asOpt[String]
      }.toList
    }

    val lemmas = for {phrase  <- phrases } yield {

      val term = (phrase \ "phrase")
        .asOpt[JsObject].toList
        .flatMap(filterLanguage)

      val defs = (phrase \ "meanings")
        .asOpt[Seq[JsObject]].toList.flatten
        .flatMap(filterLanguage)

      val senses = (term ++ defs)
        .filterNot(_ == "").distinct

      if (senses.size == 0) None
      else {
        val lemma = Json.obj(
          "representations" -> Json.arr("Orthographic"),
          "lemmaForm" -> "lemma",
          "forms" -> Json.obj(
            "lemma" -> Json.obj(
              "Orthographic" -> Json.arr(text)
            )
          ),
          "senses" -> senses.map { definition =>
            Json.obj("definition" -> definition)
          },
          "sources" -> Json.arr(
            Json.obj("name" -> name, "attribution" -> s"<i>$name</i>")
          )
        )
        Some(lemma)
      }
    }

    lemmas.collect { case Some(lemma) => lemma }
  }

  /**
   * Endpoint for translating via Glosbe
   */
  def translate(user: User, src: String, dest: String, text: String)
               (implicit request: RequestHeader, restart: TRestart) = {
    val query = WS.url("http://glosbe.com/gapi/translate")
        .withQueryString("from" -> src, "dest" -> dest,
                         "format" -> "json", "phrase" -> text).get()
    val result = Await.result(query, Duration.Inf)
    val json = result.json.as[JsObject]

    /* Glosbe gives own status, but it seems glitchy.
     * When it gets better, it may be beneficial to use it. See Below:
     *  println("Result -> " + (json\"result").as[String])
     */
    if(result.status != 200) None
    else {
      (json \ "tuc").asOpt[Seq[JsObject]].flatMap { tuc =>
        val phrase = (json \ "phrase").as[String]
        val lemmas = processResults(phrase, tuc, dest)

        if (lemmas.size == 0) None
        else {
          val results = Json.obj(
            "words" -> Json.arr(
              Json.obj(
                "start" -> 0,
                "end" -> text.length,
                "lemmas" -> lemmas
              )
            )
          )
          Some(results)
        }
      }
    }
  }
}