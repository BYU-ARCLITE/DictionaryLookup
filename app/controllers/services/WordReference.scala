package controllers

import models.User
import java.net.URLEncoder
import scala.concurrent.{ExecutionContext, Future, Await}
import scala.concurrent.duration._
import play.api.libs.json._
import play.api.libs.ws.WS
import play.api.Play.{current, configuration}
import ExecutionContext.Implicits.global

object LookupWordReference extends Translator {
  //See http://www.wordreference.com/docs/api.aspx
  val name = "WordReference"
  val expiration = Utils.getExpiration("wordReference")
  val codeFormat = 'iso639_1

  val wordReferenceKey = configuration.getString("wordReference.key")
  val TranslationList = List("FirstTranslation", "SecondTranslation", "ThirdTranslations", "FourthTranslation")
  val PartsList = List("Entries","PrincipalTranslations","AdditionalTranslations")
  val whitespace = "\\s".r

  val codeMap = Map(
    "ar" -> "ar", //Arabic
    "zh" -> "zh", //Chinese
    "cs" -> "cz", //Czech
    "en" -> "en", //English
    "fr" -> "fr", //French
    "el" -> "gr", //Greek
    "ja" -> "ja", //Japanese
    "ko" -> "ko", //Korean
    "pl" -> "pl", //Polish
    "pt" -> "pt", //Portuguese
    "ro" -> "ro", //Romanian
    "es" -> "es", //Spanish
    "tr" -> "tr" //Turkish
  )

  def mapEntriesIn(json: JsObject)(body: JsObject => List[String]) = json.fields
    .collect({ case (key, obj:JsObject) if key.startsWith("term") => obj})
    .toList.flatMap { obj =>
      PartsList.flatMap { partName =>
        (obj \ partName) match {
          case part:JsObject =>
            part.fields.collect({
              case (_, obj:JsObject) => obj
            }).toList.flatMap(body)
          case _ => Nil
        }
      }
    }

  def getTranslationsIn(entry: JsObject) = TranslationList.flatMap { name =>
    (entry \ name \ "term") match {
      case translation:JsString => List(translation.as[String])
      case _ => Nil
    }
  }

  def requestEntries(key: String, scode: String, dcode: String, text: String) = {
    val url = "http://api.wordreference.com/0.8/" + URLEncoder.encode(s"$key/json/$scode$dcode/$text", "UTF-8")
    val result = Await.result(WS.url(url).get(), Duration.Inf)
    if(result.status != 200) None
    else mapEntriesIn(result.json.as[JsObject]) { entry =>
      getTranslationsIn(entry) match {
        case Nil => Nil
        case translations =>
          List("(" + (entry \ "OriginalTerm" \ "sense").as[String] + ") " + translations.mkString(", "))
      }
    } match {
      case Nil => None
      case entries => Some(entries)
    }
  }

  /**
   * Endpoint for translating via WordReference
   */
  def translate(user: User, src: String, dest: String, text: String) = {
    if(whitespace.findFirstIn(text).isDefined) None
    else for {
      key <- wordReferenceKey
      scode <- codeMap.get(src)
      dcode <- codeMap.get(dest)
      if scode == "en" || dcode == "en"
      entries <- requestEntries(key, scode, dcode, text)
    } yield entries
  }
}