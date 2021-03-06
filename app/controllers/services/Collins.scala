package controllers

import Utils._
import models.User
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import scala.xml._
import play.api.mvc._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global
import controllers.LangCodes._

object LookupCollins extends Translator {

  val name = "Collins"
  val expiration = Utils.getExpiration("Collins")
  val codeFormat = 'iso639_3

  val baseURL = "https://api.collinsdictionary.com/api/v1/dictionaries/"
  val attribution = """<a href="http://www.collinsdictionary.com">
                         www.Collinsdictionary.com
                       </a> © HarperCollins Publishers Ltd 2014"""

  val key = configuration.getString("collins.key")

  def getPairs = Set(("eng", "eng"))

  def listEntries(key: String, dict: String, text: String) = {
    val query = WS.url(s"$baseURL$dict/search")
                  .withHeaders(
                    "accessKey" -> key,
                    "accept" -> "application/json"
                  )
                  .withQueryString("q" -> text)
                  .get()

    val result = Await.result(query, Duration.Inf)
    if(result.status != 200) Nil
    else {
      val urls = (result.json \\ "entryUrlresults")
                   .flatMap(_.asOpt[String].toList)
      urls.flatMap { url =>
        val query = WS.url(url)
                  .withHeaders(
                    "accessKey" -> key,
                    "accept" -> "application/json"
                  )
                  .withQueryString("format" -> "xml")
                  .get()

        val result = Await.result(query, Duration.Inf)
        if(result.status != 200) Nil
        else {
          (result.json \ "entryContent").asOpt[String].flatMap { xmlstr =>
            import scala.util.Try
            Try(XML.loadString(xmlstr)).toOption
          }.toList
        }
      }
    }
  }

  def XMLtoJSON(xml: Elem) = {
    val form = (xml \\ "form")
    val pron = (form \\ "pron")
    Json.obj(
      "representations" -> Json.arr("Orthographic", "IPA", "WAV"),
      "lemmaForm" -> "lemma",
      "forms" -> Json.obj(
        "lemma" -> Json.obj(
          "Orthographic" -> (form \\ "orth").map(_.text),
          "IPA" -> pron.flatMap(
            _.child.collect { case Text(s) => s }
          ),
          "WAV" -> (pron \\ "source").flatMap(
            _.attribute("src").toList.flatten.map(_.text)
          )
        )
      ),
      "senses" -> (xml \\ "def").map(_.text).map { text =>
        Json.obj("definition" -> text)
      },
      "sources" -> Json.arr(
        Json.obj("name" -> name, "attribution" -> s"<i>$name</i>")
      )
    )
  }

  def translate(user: User, scode: String, dcode: String, text: String)
               (implicit request: RequestHeader, restart: TRestart) = {
    key.flatMap { keystr =>
      val xmlEntries = listEntries(keystr, "american-learner", text)
      if(xmlEntries.size == 0) None
      else Some(xmlEntries)
    }.map { xmlEntries =>
      Json.obj(
        "words" -> Json.arr(
          Json.obj(
            "start" -> 0,
            "end" -> text.length,
            "lemmas" -> xmlEntries.map(XMLtoJSON(_))
          )
        )
      )
    }
  }
}
