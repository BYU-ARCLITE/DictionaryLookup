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
  val ChrExp = raw"^(bix|gg).*".r
  val NumExp = raw"^(\d).*".r

  def parseXMLResponse(URL: String, attr: String, headWordTag: String) = {
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

      val sound = (entry \\ "wav").take(1).map { wav =>
        val file = wav.text
        val dir = file match {
          case ChrExp(str) => str
          case NumExp(_) => "number"
          case _ => file.substring(0,1)
        }
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
        "sources" -> Json.arr(
          Json.obj(
            "name" -> name,
            "attribution" -> attr
          )
        )
      )
    }
  }

  /**
   * Endpoint for translating via merriamWebster
   */
  def translate(user: User, src: String, dst: String, text: String)
               (implicit restart: TRestart) = {
    val logoURL = "http://www.dictionaryapi.com/images/info/branding-guidelines/mw-logo-light-background-50x50.png"
    val lemmas: Seq[JsObject] =
      if (((src == "spa" && dst == "eng") || (src == "eng" && dst == "spa")) && spanishKey.isDefined) {
        val key = spanishKey.get
        val url = "http://www.dictionaryapi.com/api/v1/references/spanish/xml/" + text.replaceAll("[^\\p{L}\\p{Nd}]+", "%20").trim +"?key="+key
        val attr = s"""
          <a href="http://www.spanishcentral.com/translate/$text"
            target="Merriam-Webster">$text at SpanishCentral.com</a>
          <br/>Merriam-Webster's Spanish-English Dictionary
          <div class="merriamLogo">
            <a href="http://www.spanishcentral.com/translate/$text"
              target="Merriam-Webster"><img src="$logoURL"/></a>
          </div>
        """
        parseXMLResponse(url, attr, "hw")
      } else if(src == "eng" && dst == "eng" && collegiateKey.isDefined){
        val key = collegiateKey.get
        val url = "http://www.dictionaryapi.com/api/v1/references/collegiate/xml/" + text.replaceAll("[^\\p{L}\\p{Nd}]+", "%20").trim +"?key="+key
        val attr = s"""
          <a href="http://www.merriam-webster.com/dictionary/$text"
            target="Merriam-Webster">$text at Merriam-Webster.com</a>
          <br/> Merriam-Webster's Collegiate® Dictionary
          <div class="merriamLogo">
            <a href="http://www.merriam-webster.com/dictionary/$text"
              target="Merriam-Webster"><img src="$logoURL"/></a>
          </div>
        """
        parseXMLResponse(url, attr, "ew")
      } else Seq[JsObject]()

    if(lemmas.size == 0) None
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