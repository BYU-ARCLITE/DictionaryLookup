package controllers

import models.User
import Utils._
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

import xml.{Elem, XML}
import java.io.InputStream;
import java.net.URL;

object LookupMerriamWebster extends Translator {
  val name = "Merriam-Webster Inc."
  val expiration = Utils.getExpiration("merriamWebster")
  val codeFormat = 'iso639_3

  val spanishKey = configuration.getString("merriamWebster.spanishKey")
  val collegiateKey = configuration.getString("merriamWebster.collegiateKey")

  def parseXMLResponse(URL:String, headWordTag:String) : Seq[JsObject] = {
    val url = new URL(URL)
    val body = url.openStream
    val XMLDoc = XML.load(body)
    val response = (XMLDoc \\ "entry_list")

    for {
      entry <- response \\ "entry"
      pos <- entry \ "fl"
    } yield {

      //val wav = (entry \\ "wav").text

      val word = (entry \\ headWordTag).text
      val ipa = (entry \\ "pr").map(_.text)

      val sound : Seq[String] = for {
        entry <- XMLDoc \\ "wav"
      } yield {
        val file = entry.text
        val dir = if(file.substring(0,3) == "bix" )
              "bix"
            else if (file.substring(0,2) == "gg")
              "gg"
            else if (file.substring(0,1).matches("""\d"""))
              "number"
            else
              file.substring(0,1)
        s"http://media.merriam-webster.com/soundc11/$dir/$file"
      }

      var reps = Seq("Orthographic")
      var lemmaReps = Map("Orthographic" -> Seq(word))
      
      if (ipa.size > 0) {
        reps = reps ++ Seq("IPA")
        lemmaReps = lemmaReps ++ Map("IPA" -> ipa)
      }

      if (sound.size > 0) {
        reps = reps ++ Seq("WAV")
        lemmaReps = lemmaReps ++ Map("WAV" -> sound)
      }

      Json.obj(
        "representations" -> reps,
        "pos" -> pos.text,
        "lemmaForm" -> "lemma",
        "forms" -> Json.obj("lemma" -> Json.toJson(lemmaReps)),
        "senses" -> (entry \\ "dt").map { dt =>
          Json.obj("definition" -> dt.text.replace("<br>", "\n"))
        },
        "sources" -> Seq(name)
      )
    }
  }

  /**
   * Endpoint for translating via merriamWebster
   */
  def translate(user: User, src: String, dst: String, text: String)
               (implicit restart: TRestart) = {
    val lemmas: Seq[JsObject] =
      if (((src == "spa" && dst == "eng") || (src == "eng" && dst == "spa")) && spanishKey.isDefined) {
        val key = spanishKey.get
        val url = "http://www.dictionaryapi.com/api/v1/references/spanish/xml/" + text.replaceAll("[^\\p{L}\\p{Nd}]+", "%20").trim +"?key="+key
        parseXMLResponse(url, "hw")
      } else if(src == "eng" && dst == "eng" && collegiateKey.isDefined){
        val key = collegiateKey.get
        val url = "http://www.dictionaryapi.com/api/v1/references/collegiate/xml/" + text.replaceAll("[^\\p{L}\\p{Nd}]+", "%20").trim +"?key="+key
        parseXMLResponse(url, "ew")
      } else Seq[JsObject]()

    if(lemmas.size == 0) None
    else {
      val results = Json.obj(
        //"translations" -> Json.arr("free translation text")
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